//===--- CGBlocks.cpp - Emit LLVM Code for declarations ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit blocks.
//
//===----------------------------------------------------------------------===//

#include "CGContracts.h"
#include "CGCXXABI.h"
#include "CGDebugInfo.h"
#include "CGObjCRuntime.h"
#include "CGOpenCLRuntime.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "ConstantEmitter.h"
#include "TargetInfo.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "clang/CodeGen/ConstantInitBuilder.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ScopedPrinter.h"
#include <algorithm>
#include <cstdio>
#include <optional>

using namespace clang;
using namespace CodeGen;

constexpr ContractEvaluationSemantic Enforce =
    ContractEvaluationSemantic::Enforce;
constexpr ContractEvaluationSemantic QuickEnforce =
    ContractEvaluationSemantic::QuickEnforce;
constexpr ContractEvaluationSemantic Observe =
    ContractEvaluationSemantic::Observe;
constexpr ContractEvaluationSemantic Ignore =
    ContractEvaluationSemantic::Ignore;

constexpr ContractDetectionMode PredicateFailed =
    ContractDetectionMode::PredicateFailed;
constexpr ContractDetectionMode ExceptionRaised =
    ContractDetectionMode::ExceptionRaised;

namespace clang::CodeGen {

enum ContractCheckpoint {
  EmittingContract,
  EmittingTryBody,
  EmittingCatchBody,
};

enum ContractEmissionStyle {
  /// Emit the contract violation as an inline basic block immediately following
  /// the predicate. The basic block is not shared by other contracts.
  Inline,

  /// Emit a single shared contract violation handler per-function.
  /// This only works when exceptions are disabled, otherwise the violation
  /// handler
  /// may throw from the violation handler.
  Shared
};

template <class T>
static llvm::Constant *CreateConstantInt(CodeGenFunction &CGF, T Sem) {
  static_assert(std::is_same_v<T, ContractEvaluationSemantic> ||
                std::is_same_v<T, ContractDetectionMode>);
  return llvm::ConstantInt::get(CGF.IntTy, (int)Sem);
}

struct CurrentContractInfo {

  const ContractStmt *Contract;
  ContractEmissionStyle Style;
  ContractCheckpoint Checkpoint = EmittingContract;
  ContractEvaluationSemantic Semantic;

  llvm::BasicBlock *Violation = nullptr;
  llvm::BasicBlock *End = nullptr;

  llvm::Constant *ViolationInfoGV = nullptr;
  Address EHPredicateStore = Address::invalid();
};

// A contract enforce block is a block used to create and call the violation
// handler for contracts set to 'enforce'. Such contracts never return after
// reporting a violation.
//
// It is used to create a single
// block that can be used to handle all contract violations in a function.
//
// The block is created lazily, and is only created if a contract is emitted
// with an enforce semantic.
struct SharedEnforceBlock {
  static SharedEnforceBlock Create(CodeGenFunction &CGF) {
    SharedEnforceBlock This;


    auto SavedIP = CGF.Builder.saveAndClearIP();

    This.Block = CGF.createBasicBlock("contract.violation.handler");
    CGF.Builder.SetInsertPoint(This.Block);
    This.IncomingPHI = CGF.Builder.CreatePHI(CGF.VoidPtrTy, 4);

    CGF.EmitHandleContractViolationCall(CreateConstantInt(CGF, Enforce),
                                        CreateConstantInt(CGF, PredicateFailed),
                                        This.IncomingPHI,
                                        /*IsNoReturn=*/true);
    CGF.Builder.CreateUnreachable();
    CGF.Builder.ClearInsertionPoint();
    CGF.Builder.restoreIP(SavedIP);

    return This;
  }

  llvm::BasicBlock *Block = nullptr;
  llvm::PHINode *IncomingPHI = nullptr;

private:
  SharedEnforceBlock() = default;
};

static void CreateTrap(CodeGenFunction &CGF) {
  auto &Builder = CGF.Builder;
  llvm::CallInst *TrapCall = CGF.EmitTrapCall(llvm::Intrinsic::trap);
  TrapCall->setDoesNotReturn();
  TrapCall->setDoesNotThrow();
  Builder.CreateUnreachable();
  Builder.ClearInsertionPoint();
}

static llvm::BasicBlock *CreateTrapBlock(CodeGenFunction &CGF) {
  auto &Builder = CGF.Builder;
  CGBuilderTy::InsertPoint SavedIP = Builder.saveAndClearIP();
  // Set up the terminate handler.  This block is inserted at the very
  // end of the function by FinishFunction.
  llvm::BasicBlock *ContractViolationTrapBlock =
      CGF.createBasicBlock("contract.violation.trap.handler");
  Builder.SetInsertPoint(ContractViolationTrapBlock);

  CreateTrap(CGF);

  Builder.restoreIP(SavedIP);
  return ContractViolationTrapBlock;
}

struct SharedTrapBlock {
  static SharedTrapBlock Create(CodeGenFunction &CGF) {
    SharedTrapBlock This;
    This.Block = CreateTrapBlock(CGF);
    return This;
  }

  llvm::BasicBlock *Block = nullptr;

private:
  SharedTrapBlock() = default;
};

struct CGContractData {
  std::optional<SharedEnforceBlock> EnforceBlock;
  std::optional<SharedTrapBlock> TrapBlock;
  std::optional<CurrentContractInfo> CurContract;

  SharedTrapBlock &GetSharedTrapBlock(CodeGenFunction &CGF) {
    if (!TrapBlock)
      TrapBlock = SharedTrapBlock::Create(CGF);
    return *TrapBlock;
  }

  SharedEnforceBlock &GetSharedEnforceBlock(CodeGenFunction &CGF) {
    if (!EnforceBlock)
      EnforceBlock = SharedEnforceBlock::Create(CGF);
    return *EnforceBlock;
  }
};

} // namespace clang::CodeGen

CurrentContractInfo *CodeGenFunction::CurContract() {
  return ContractData->CurContract ? &ContractData->CurContract.value()
                                   : nullptr;
}

struct CurrentContractRAII {
  CurrentContractRAII(CodeGenFunction &CGF, CurrentContractInfo CurContract)
      : CGF(CGF) {
    assert(!CGF.ContractData->CurContract);
    CGF.ContractData->CurContract.emplace(std::move(CurContract));
  }
  ~CurrentContractRAII() {
    assert(CGF.ContractData->CurContract);
    CGF.ContractData->CurContract.reset();
  }
  CodeGenFunction &CGF;
};

CGContractData *CGContractDataDeleter::Create() { return new CGContractData(); }
void CGContractDataDeleter::operator()(CGContractData *Data) const {

  if (Data)
    delete Data;
}

llvm::BasicBlock *
CodeGenFunction::GetSharedContractViolationEnforceBlock(bool Create) {
  if (!ContractData->EnforceBlock && !Create)
    return nullptr;
  return ContractData->GetSharedEnforceBlock(*this).Block;
}

llvm::BasicBlock *
CodeGenFunction::GetSharedContractViolationTrapBlock(bool Create) {
  if (!ContractData->TrapBlock && !Create)
    return nullptr;
  return ContractData->GetSharedTrapBlock(*this).Block;
}

[[maybe_unused]]
static std::string GetMangledContractViolationHandler(
    ContractEvaluationSemantic Sem,
    ContractDetectionMode Detect) {
  std::string result = "__handle_contract_violation_v4";
  result += [&]() -> const char* {
    switch (Detect) {
    case PredicateFailed:
      return "_pf";
    case ExceptionRaised:
      return "_pe";
    case ContractDetectionMode::Unspecified:
      llvm_unreachable("Here");
    }
    llvm_unreachable("cases should all be handled");
  }();
  result += [&]() -> const char* {
    switch (Sem) {
    case Observe:
      return "_so";
    case Enforce:
      return "_se";
    case Ignore:
    case QuickEnforce:
      llvm_unreachable("cases should not occur");
    }
  }();

  return result;
}

void CodeGenFunction::EmitHandleContractViolationCall(
    llvm::Constant *EvalSemantic, llvm::Constant *DetectionMode,
    llvm::Value *ViolationInfoGV, bool IsNoReturn) {
  auto &Ctx = getContext();

  CanQualType ArgTypes[3] = {Ctx.UnsignedIntTy, Ctx.UnsignedIntTy,
                             Ctx.VoidPtrTy};

  const CGFunctionInfo &VFuncInfo =
      CGM.getTypes().arrangeBuiltinFunctionDeclaration(getContext().VoidTy,
                                                       ArgTypes);

  StringRef TargetFuncName = "__handle_contract_violation_v3";
  llvm::FunctionType *VFTy = CGM.getTypes().GetFunctionType(VFuncInfo);
  llvm::FunctionCallee VFunc = CGM.CreateRuntimeFunction(VFTy, TargetFuncName);

  if (IsNoReturn) {
    llvm::Value *Args[3] = {EvalSemantic, DetectionMode, ViolationInfoGV};
    EmitNoreturnRuntimeCallOrInvoke(VFunc, Args);
    Builder.ClearInsertionPoint();
  } else {
    CallArgList Args;
    Args.add(RValue::get(EvalSemantic), getContext().UnsignedIntTy);
    Args.add(RValue::get(DetectionMode), getContext().UnsignedIntTy);
    Args.add(RValue::get(ViolationInfoGV), getContext().VoidPtrTy);
    EmitCall(VFuncInfo, CGCallee::forDirect(VFunc), ReturnValueSlot(), Args);
  }
}

// Check if function can throw based on prototype noexcept, also works for
// destructors which are implicitly noexcept but can be marked noexcept(false).
static bool FunctionCanThrow(const FunctionDecl *D) {
  const auto *Proto = D->getType()->getAs<FunctionProtoType>();
  if (!Proto) {
    // Function proto is not found, we conservatively assume throwing.
    return true;
  }
  return !isNoexceptExceptionSpec(Proto->getExceptionSpecType()) ||
         Proto->canThrow() != CT_Cannot;
}

static bool StmtCanThrow(const Stmt *S) {
  if (const auto *CE = dyn_cast<CallExpr>(S)) {
    const auto *Callee = CE->getDirectCallee();
    if (!Callee)
      // We don't have direct callee. Conservatively assume throwing.
      return true;

    if (FunctionCanThrow(Callee))
      return true;

    // Fall through to visit the children.
  }

  if (isa<CXXThrowExpr>(S))
    return true;

  if (const auto *TE = dyn_cast<CXXBindTemporaryExpr>(S)) {
    // Special handling of CXXBindTemporaryExpr here as calling of Dtor of the
    // temporary is not part of `children()` as covered in the fall through.
    // We need to mark entire statement as throwing if the destructor of the
    // temporary throws.
    const auto *Dtor = TE->getTemporary()->getDestructor();
    if (FunctionCanThrow(Dtor))
      return true;

    // Fall through to visit the children.
  }

  for (const auto *child : S->children())
    if (StmtCanThrow(child))
      return true;

  return false;
}

// Emit the contract expression.
void CodeGenFunction::EmitContractStmt(const ContractStmt &S) {
  assert(!CurContract() || CurContract()->Contract == &S);

  if (!CurContract()) {
    // FIXME(EricWF): Remove this. It's a hack to prevent crashing.

    EmitContractStmtAsFullStmt(S);

  } else if (CurContract()->Checkpoint == EmittingTryBody) {
    return EmitContractStmtAsTryBody(S);
  } else if (CurContract()->Checkpoint == EmittingCatchBody) {
    return EmitContractStmtAsCatchBody(S);
  } else {
    llvm_unreachable("Invalid checkpoint");
  }
}

// FIXME(EricWF): Do I really need this?
void CodeGenFunction::EmitContractStmtAsTryBody(const ContractStmt &S) {
  assert(CurContract() && CurContract()->Contract == &S &&
         CurContract()->Checkpoint == EmittingTryBody);
  Builder.CreateStore(EmitScalarExpr(S.getCond()),
                      CurContract()->EHPredicateStore);

}

void CodeGenFunction::EmitContractStmtAsCatchBody(const ContractStmt &S) {
  assert(CurContract() && CurContract()->Contract == &S &&
         CurContract()->Checkpoint == EmittingCatchBody);
  auto CurInfo = CurContract();

  if (CurInfo->Semantic == Enforce || CurInfo->Semantic == Observe) {
    // We have to emit the contract violation block inside the catch block so
    // that the handler can see the exception via std::current_exception
    EmitHandleContractViolationCall(
        CreateConstantInt(*this, CurInfo->Semantic),
        CreateConstantInt(*this, ExceptionRaised), CurInfo->ViolationInfoGV,
        /*IsNoReturn=*/CurInfo->Semantic == Enforce);
  } else if (CurInfo->Semantic == QuickEnforce) {
    CreateTrap(*this);
  } else {
    llvm_unreachable("Unhandled semantic");
  }
}

static CXXTryStmt *BuildTryCatch(const ContractStmt &S, CodeGenFunction &CGF) {
  auto &Ctx = CGF.getContext();
  auto Loc = S.getCond()->getExprLoc();

  llvm::SmallVector<Stmt *> BodyStmts;
  BodyStmts.push_back(const_cast<ContractStmt *>(&S));

  // FIXME(EricWF): THIS IS A TERRIBLE HACK.
  //   In order to emit the contract assertion violation in the catch block
  //   we add the current statement to a dummy handler, and then detect
  //   when we're inside that dummy handler to only emit the violation
  //
  // This should have some other representation, but I don't want to eagerly
  // build all these nodes in the AST.

  auto *CatchStmt =
      CompoundStmt::Create(Ctx, BodyStmts, FPOptionsOverride(), Loc, Loc);
  auto *Catch =
      new (Ctx) CXXCatchStmt(Loc, /*exDecl=*/nullptr, /*block=*/CatchStmt);
  auto *TryBody =
      CompoundStmt::Create(Ctx, BodyStmts, FPOptionsOverride(), Loc, Loc);
  return CXXTryStmt::Create(Ctx, Loc, TryBody, Catch);
}

void CodeGenFunction::EmitContractStmtAsFullStmt(const ContractStmt &S) {
  assert(CurContract() == nullptr);
  // FIXME(EricWF): We recursively call EmitContractStmt to build the catch
  // block that reports contract violations that have thrown. In order to do
  // this without building additional AST nodes, use this Stmt as the body
  // of the catch block, detecting when we're inside the catch block to only
  // emit the violation.

  ContractEvaluationSemantic Semantic = S.getSemantic(getContext());

  // FIXME(EricWF): I think there's a lot more to do that simply this.
  if (Semantic == Ignore)
    return;

  const auto Style = [&]() {
    switch (Semantic) {
    case Enforce: {
      if (!getLangOpts().Exceptions)
        return Shared;
    }
      LLVM_FALLTHROUGH;
    case Observe:
      return Inline;
    case QuickEnforce:
      return Shared;
    case ContractEvaluationSemantic::Ignore:
    
      llvm_unreachable("unhandled semantic");
    }
  }();

  auto Violation = [&]() {
    switch (Style) {
    case Inline:
      return createBasicBlock("contract.violation");
    case Shared:
      assert(Semantic == Enforce || Semantic == QuickEnforce);
      return Semantic == Enforce ? GetSharedContractViolationEnforceBlock()
                                  : GetSharedContractViolationTrapBlock();
    }
  }();

  llvm::BasicBlock *End = createBasicBlock("contract.end");

  llvm::Constant *ViolationInfo = nullptr;
  if (Semantic != ContractEvaluationSemantic::QuickEnforce) {
    ViolationInfo = CGM.GetAddrOfUnnamedGlobalConstantDecl(
                           getContext().BuildViolationObject(
                               &S, dyn_cast_or_null<FunctionDecl>(CurFuncDecl)), "contract.loc")
                        .getPointer();
  }

  CurrentContractRAII CurContractRAII(*this,
                                      {.Contract = &S,
                                       .Style = Style,
                                       .Checkpoint = EmittingContract,
                                       .Semantic = Semantic,
                                       .Violation = Violation,
                                       .End = End,
                                       .ViolationInfoGV = ViolationInfo});

  llvm::Value *BranchOn;
  if (getLangOpts().Exceptions && getLangOpts().ContractExceptions && StmtCanThrow(S.getCond())) {

    CurContract()->EHPredicateStore = CreateTempAlloca(
        Builder.getInt1Ty(), CharUnits::One(), "contract.pred.value");
    // Set the initial value to true. If the contract throws, we'll see the true
    // value after the catch block is done handling the exception.
    Builder.CreateStore(Builder.getTrue(), CurContract()->EHPredicateStore);

    assert(Builder.GetInsertBlock());
    EnsureInsertPoint();

    auto *Try = BuildTryCatch(S, *this);
    EnterCXXTryStmt(*Try);
    CurContract()->Checkpoint = EmittingTryBody;
    EmitStmt(Try->getTryBlock());
    CurContract()->Checkpoint = EmittingCatchBody;
    ExitCXXTryStmt(*Try);
    CurContract()->Checkpoint = EmittingContract;

    BranchOn = Builder.CreateLoad(CurContract()->EHPredicateStore);
  } else {
    BranchOn = EmitScalarExpr(S.getCond());
  }

  if (Style == Shared && Semantic == Enforce) {
    //assert(!getLangOpts().Exceptions);
    EnsureInsertPoint();
    ContractData->GetSharedEnforceBlock(*this).IncomingPHI->addIncoming(
        ViolationInfo, Builder.GetInsertBlock());
  }

  Builder.CreateCondBr(BranchOn, End, Violation);

  // If we're creating a trap, the violation block will be created once for the
  // function. Otherwise, we need to create a call to the violation handler.
  if (Style == Inline) {
    EmitBlock(CurContract()->Violation);
    Builder.SetInsertPoint(CurContract()->Violation);
    EmitHandleContractViolationCall(CreateConstantInt(*this, Semantic),
                                    CreateConstantInt(*this, PredicateFailed),
                                    CurContract()->ViolationInfoGV,
                                    /*IsNoReturn=*/Semantic == Enforce);
    if (Semantic != Enforce)
      Builder.CreateBr(End);
  }

  EmitBlock(End);
}
