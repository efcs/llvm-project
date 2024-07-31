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
#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ScopedPrinter.h"
#include <algorithm>
#include <cstdio>
#include <optional>

using namespace clang;
using namespace CodeGen;

enum ContractCheckpoint {
  EmittingContract,
  EmittingTryBody,
  EmittingCatchBody,
};

struct clang::CodeGen::CGContractData {
  bool isTrap() const {
    return Semantic == ContractEvaluationSemantic::QuickEnforce;
  }

  bool isEnforce() const {
    return Semantic == ContractEvaluationSemantic::Enforce;
  }

  bool isObserve() const {
    return Semantic == ContractEvaluationSemantic::Observe;
  }

  const ContractStmt *Contract;
  ContractCheckpoint Checkpoint = EmittingContract;
  ContractEvaluationSemantic Semantic;

  llvm::BasicBlock *Violation = nullptr;
  llvm::BasicBlock *End = nullptr;
  llvm::Constant *ViolationInfoGV = nullptr;
  Address EHPredicateStore = Address::invalid();
};

void CGContractDataDeleter::operator()(CGContractData *Data) const {
  delete Data;
}

CodeGenContracts::CodeGenContracts(CodeGenFunction &CGF)
    : CGF(CGF), CGM(CGF.CGM), Ctx(CGF.getContext()) {}

ContractViolationBlock::ContractViolationBlock(CodeGen::CodeGenFunction &CGF,
                                               ContractEvaluationSemantic Sem)
    : Semantic(Sem) {}

llvm::Constant *CodeGenFunction::EmitContractArgumentConstant(
    const ContractStmt *S, ContractEvaluationSemantic Semantic,
    ContractViolationDetection DetectMode) {

  llvm::Constant *SemanticValue =
      llvm::ConstantInt::get(IntTy, (int)S->getSemantic(getLangOpts()));
  llvm::Constant *DetectModeValue =
      llvm::ConstantInt::get(IntTy, (int)DetectMode);
  ConstantAddress LocArg =
      CGM.GetAddrOfUnnamedGlobalConstantDecl(getContext().BuildViolationObject(
          S, dyn_cast_or_null<FunctionDecl>(this->CurFuncDecl)));
  llvm::Constant *Args[] = {SemanticValue, DetectModeValue,
                            LocArg.getPointer()};
  return llvm::ConstantStruct::getAnon(Args);
}

void CodeGenFunction::AddContractViolationIncomingBlock(
    llvm::BasicBlock *Inc, ContractStmt const *CS,
    ContractEvaluationSemantic Semantic, ContractViolationDetection Mode) {
  assert(ContractViolationBlock);

  UnnamedGlobalConstantDecl *ViolationObj =
      CGM.getContext().BuildViolationObject(
          CS, dyn_cast_or_null<FunctionDecl>(this->CurFuncDecl));

  llvm::Constant *DetectionConstant = llvm::ConstantInt::get(IntTy, (int)Mode);

  ConstantAddress ArgValue =
      CGM.GetAddrOfUnnamedGlobalConstantDecl(ViolationObj, ".contract.info");
  assert(ContractViolationPhi);
  ContractViolationPhi->addIncoming(ArgValue.getPointer(), Inc);
  ContractViolationDetectionPhi->addIncoming(DetectionConstant, Inc);
  assert(Semantic == ContractEvaluationSemantic::Enforce);
}

[[maybe_unused]] static void
CreateContractViolationBlockForFunction(CodeGenFunction &CGF) {
  //  CGBuilderTy::InsertPoint SavedIP = Builder.saveAndClearIP();

  auto &Builder = CGF.Builder;
  auto &CGM = CGF.CGM;
  Builder.SetInsertPoint(CGF.ContractViolationBlock);

  CGF.ContractViolationPhi = Builder.CreatePHI(CGF.VoidPtrTy, 4);
  CGF.ContractViolationDetectionPhi = Builder.CreatePHI(CGF.IntTy, 4);
  llvm::Value *SemanticValue = llvm::ConstantInt::get(
      CGF.IntTy, (int)ContractEvaluationSemantic::Enforce);

  CanQualType ArgTypes[3] = {CGF.getContext().UnsignedIntTy,
                             CGF.getContext().UnsignedIntTy,
                             CGF.getContext().VoidPtrTy};
  llvm::Value *Args[3] = {SemanticValue, CGF.ContractViolationDetectionPhi,
                          CGF.ContractViolationPhi};
  const CGFunctionInfo &VFuncInfo =
      CGM.getTypes().arrangeBuiltinFunctionDeclaration(CGF.getContext().VoidTy,
                                                       ArgTypes);

  llvm::FunctionType *VFTy = CGM.getTypes().GetFunctionType(VFuncInfo);
  llvm::FunctionCallee VFunc =
      CGM.CreateRuntimeFunction(VFTy, "__handle_contract_violation_v3");

  CGF.EmitNoreturnRuntimeCallOrInvoke(VFunc, Args);

  Builder.ClearInsertionPoint();
}

llvm::BasicBlock *CodeGenFunction::GetContractViolationBlock() {
  if (ContractViolationBlock)
    return ContractViolationBlock;

  assert(!ContractViolationPhi);

  CGBuilderTy::InsertPoint SavedIP = Builder.saveAndClearIP();

  // Set up the terminate handler.  This block is inserted at the very
  // end of the function by FinishFunction.
  ContractViolationBlock = createBasicBlock("contract.violation.handler");
  CreateContractViolationBlockForFunction(*this);

  Builder.restoreIP(SavedIP);
  return ContractViolationBlock;
}

llvm::BasicBlock *CodeGenFunction::GetContractViolationTrapBlock() {
  if (ContractViolationTrapBlock)
    return ContractViolationTrapBlock;

  CGBuilderTy::InsertPoint SavedIP = Builder.saveAndClearIP();
  // Set up the terminate handler.  This block is inserted at the very
  // end of the function by FinishFunction.
  ContractViolationTrapBlock =
      createBasicBlock("contract.violation.trap.handler");
  Builder.SetInsertPoint(ContractViolationTrapBlock);

  llvm::CallInst *TrapCall = EmitTrapCall(llvm::Intrinsic::trap);
  TrapCall->setDoesNotReturn();
  TrapCall->setDoesNotThrow();

  Builder.CreateUnreachable();
  Builder.ClearInsertionPoint();

  Builder.restoreIP(SavedIP);
  return ContractViolationTrapBlock;
}

void CodeGenFunction::EmitHandleContractViolationCall(
    const ContractStmt &S, ContractEvaluationSemantic Semantic,
    ContractViolationDetection ViolationDetectionMode) {
  assert(CurContractData && CurContractData->Contract == &S);

  assert(Semantic == ContractEvaluationSemantic::Enforce ||
         Semantic == ContractEvaluationSemantic::Observe);

  llvm::Constant *SemanticVal = llvm::ConstantInt::get(IntTy, (int)Semantic);
  llvm::Constant *DetectionModeVal =
      llvm::ConstantInt::get(IntTy, (int)ViolationDetectionMode);
  assert(CurContractData->ViolationInfoGV);
  return EmitHandleContractViolationCall(
      S, SemanticVal, DetectionModeVal,
      CurContractData->ViolationInfoGV->getPointer(),
      /*IsNoReturn=*/Semantic == ContractEvaluationSemantic::Enforce);
}

void CodeGenFunction::EmitHandleContractViolationCall(
    const ContractStmt &S, llvm::Constant *EvalSemantic,
    llvm::Constant *DetectionMode, llvm::Constant *ViolationInfoGV,
    bool IsNoReturn) {
  auto &Ctx = getContext();

  CanQualType ArgTypes[3] = {Ctx.IntTy, Ctx.IntTy, Ctx.VoidPtrTy};

  const CGFunctionInfo &VFuncInfo =
      CGM.getTypes().arrangeBuiltinFunctionDeclaration(getContext().VoidTy,
                                                       ArgTypes);
  StringRef TargetFuncName = "__handle_contract_violation_v3";
  llvm::FunctionType *VFTy = CGM.getTypes().GetFunctionType(VFuncInfo);
  llvm::FunctionCallee VFunc = CGM.CreateRuntimeFunction(VFTy, TargetFuncName);

  if (IsNoReturn) {
    llvm::Value *Args[3] = {EvalSemantic, DetectionMode, ViolationInfoGV};
    EmitNoreturnRuntimeCallOrInvoke(VFunc, Args);
  } else {
    CallArgList Args;
    Args.add(RValue::get(EvalSemantic), getContext().UnsignedIntTy);
    Args.add(RValue::get(DetectionMode), getContext().UnsignedIntTy);
    Args.add(RValue::get(ViolationInfoGV), getContext().VoidPtrTy);
    EmitCall(VFuncInfo, CGCallee::forDirect(VFunc), ReturnValueSlot(), Args);
  }

  //
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

template <class OnEnd> struct ScopeGuard {
  OnEnd End;
  template <class OnStart> ScopeGuard(OnStart Start, OnEnd End) : End(End) {
    Start();
  }
  bool Enabled = true;
  void Disable() { Enabled = false; }

  ~ScopeGuard() {
    if (Enabled) {
      End();
    }
  }
};

template <class OnStart, class OnEnd>
ScopeGuard(OnStart, OnEnd) -> ScopeGuard<OnEnd>;

void HandleContractStmtCatchBlock(CodeGenFunction &CGF, const ContractStmt &S) {
  // FIXME(EricWF): This is a terrible hack to allow easy generation of
  // a contract violation in a catch block.
  assert(CGF.InContractCatchBlock == true);
  ContractViolationDetection DetectMode =
      ContractViolationDetection::ExceptionRaised;
  CGF.EmitHandleContractViolationCall(S, DetectMode);
  return;
}

// Emit the contract expression.
void CodeGenFunction::EmitContractStmt(const ContractStmt &S) {
  // FIXME(EricWF): We recursively call EmitContractStmt to build the catch
  // block that reports contract violations that have thrown. In order to do
  // this without building additional AST nodes, use this Stmt as the body
  // of the catch block, detecting when we're inside the catch block to only
  // emit the violation.
  if (&S == CurContract) {
    return HandleContractStmtCatchBlock(*this, S);
  } else {
    assert(CurContract == nullptr);
  }

  const Expr *Expr = S.getCond();
  ContractEvaluationSemantic Semantic = S.getSemantic(getLangOpts());

  // FIXME(EricWF): I think there's a lot more to do that simply this.
  if (Semantic == LangOptions::ContractEvaluationSemantic::Ignore)
    return;

  assert(Builder.GetInsertBlock());
  EnsureInsertPoint();
  llvm::BasicBlock *End = createBasicBlock("contract.end");

  llvm::BasicBlock *Violation = nullptr;
  bool IsObserving = Semantic == ContractEvaluationSemantic::Observe;
  if (Semantic == ContractEvaluationSemantic::Enforce) {
    Violation = GetContractViolationBlock();
    assert(Violation);
  } else if (Semantic == ContractEvaluationSemantic::QuickEnforce) {

    Violation = GetContractViolationTrapBlock();
    assert(Violation);
  } else {
    assert(IsObserving);
    Violation = createBasicBlock("contract.violation.observe");
  }

  CXXTryStmt *TryStmt = nullptr;

  llvm::Value *BranchOn = nullptr;
  llvm::Value *ContractStateVal = nullptr;
  if (getLangOpts().ContractExceptions && StmtCanThrow(S.getCond())) {
    RawAddress ContractState =
        CreateIRTemp(getContext().UnsignedIntTy, "cond.state");
    Builder.CreateStore(
        llvm::ConstantInt::get(Int32Ty,
                               (int)ContractViolationDetection::NoViolation),
        ContractState);

    auto Loc = S.getCond()->getExprLoc();
    llvm::SmallVector<Stmt *> CatchStmts;
    // FIXME(EricWF): THIS IS A TERRIBLE HACK.
    //   In order to emit the contract assertion violation in the catch block
    //   we add the current statement to a dummy handler, and then detect
    //   when we're inside that dummy handler to only emit the violation
    //
    // This should have some other representation, but I don't want to eagerly
    // build all these nodes in the AST.
    CatchStmts.push_back(const_cast<ContractStmt *>(&S));
    auto *CatchStmt = CompoundStmt::Create(getContext(), CatchStmts,
                                           FPOptionsOverride(), Loc, Loc);
    auto *Catch = new (getContext())
        CXXCatchStmt(Loc, /*exDecl=*/nullptr, /*block=*/CatchStmt);
    llvm::SmallVector<Stmt *> Stmts;

    auto *TryBody = CompoundStmt::Create(getContext(), Stmts,
                                         FPOptionsOverride(), Loc, Loc);
    TryStmt = CXXTryStmt::Create(getContext(), Loc, TryBody, Catch);
    assert(TryStmt);
    {
      ScopeGuard EnterContractStmt(
          [&]() {
            CurContract = &S;
            InContractCatchBlock = true;
          },
          [&]() {
            CurContract = nullptr;
            InContractCatchBlock = false;
          });

      EnterCXXTryStmt(*TryStmt);

      llvm::Value *ArgValue = EmitScalarExpr(Expr);
      auto SelectedVal = Builder.CreateSelect(
          ArgValue,
          Builder.getInt32((int)ContractViolationDetection::NoViolation),
          Builder.getInt32((int)ContractViolationDetection::PredicateFailed));
      Builder.CreateStore(SelectedVal, ContractState);

      ExitCXXTryStmt(*TryStmt);
    }
    Builder.SetInsertPoint(createBasicBlock("after.foo"));
    ContractStateVal = Builder.CreateLoad(ContractState);
    BranchOn = Builder.CreateICmpEQ(
        ContractStateVal,
        Builder.getInt32((int)ContractViolationDetection::NoViolation));
    Builder.CreateCondBr(BranchOn, End, Violation);
  } else {

    BranchOn = EmitScalarExpr(Expr);

    assert(Builder.GetInsertBlock());
    if (Semantic == ContractEvaluationSemantic::Enforce)
      AddContractViolationIncomingBlock(
          Builder.GetInsertBlock(), &S, Semantic,
          ContractViolationDetection::PredicateFailed);
    // auto *Block = Builder.GetInsertBlock();
    Builder.CreateCondBr(BranchOn, End, Violation);
  }
  if (Semantic == ContractEvaluationSemantic::Observe) {

    EmitBlock(Violation);

    ContractViolationDetection DetectMode =
        ContractViolationDetection::PredicateFailed;
    EmitHandleContractViolationCall(S, &DetectMode);
    // Builder.CreateBr(End);
  }

  EmitBlock(End);
}

struct CodeGenFoo : CodeGenFunction {

  void EmitNonThrowingContract(const ContractStmt &S);
};

CodeGenFoo &GetFoo(CodeGenFunction &CGF) {
  return static_cast<CodeGenFoo &>(CGF);
}

void CodeGenFoo::EmitNonThrowingContract(const clang::ContractStmt &S) {}

// Emit the contract expression.
void CodeGenFunction::EmitContractStmtNew(const ContractStmt &S) {
  assert(!CurContractData);
  if (!CurContractData) {
    return EmitContractStmtAsFullStmt(S);
  } else if (CurContractData->Checkpoint == EmittingTryBody) {
    return EmitContractStmtAsTryBody(S);
  } else if (CurContractData->Checkpoint == EmittingCatchBody) {
    return EmitContractStmtAsCatchBody(S);
  } else {
    llvm_unreachable("Invalid checkpoint");
  }
}

void CodeGenFunction::EmitContractStmtAsTryBody(const ContractStmt &S) {
  assert(CurContractData && CurContractData->Contract == &S &&
         CurContractData->Checkpoint == EmittingTryBody);
  Builder.CreateStore(EmitScalarExpr(S.getCond()),
                      CurContractData->EHPredicateStore);
}

void CodeGenFunction::EmitContractStmtAsCatchBody(const ContractStmt &S) {
  assert(CurContractData && CurContractData->Contract == &S &&
         CurContractData->Checkpoint == EmittingCatchBody);
  EmitHandleContractViolationCall(S, CurContractData->Semantic,
                                  ContractViolationDetection::ExceptionRaised);
}

static void EmitSimpleContract(CodeGenFunction &CGF, llvm::Value *PredValue) {
  auto &Ctx = CGF.getContext();
  auto &Builder = CGF.Builder;
  auto [Contract, Checkpoint, Semantic, Violation, End, _1, _2] =
      *CGF.CurContractData;
  CGF.EmitBranchOnBoolExpr(PredValue, End, Violation);
  Builder.SetInsertPoint(Violation);
  CGF.EmitHandleContractViolationCall(
      *Contract, Semantic, ContractViolationDetection::PredicateFailed);
  Builder.CreateBr(End);
  CGF.EmitBlock(End);
}

void CodeGenFunction::EmitContractStmtAsFullStmt(const ContractStmt &S) {
  assert(CurContractData == nullptr);
  using CES = ContractEvaluationSemantic;

  // FIXME(EricWF): We recursively call EmitContractStmt to build the catch
  // block that reports contract violations that have thrown. In order to do
  // this without building additional AST nodes, use this Stmt as the body
  // of the catch block, detecting when we're inside the catch block to only
  // emit the violation.

  ContractEvaluationSemantic Semantic = S.getSemantic(getLangOpts());

  // FIXME(EricWF): I think there's a lot more to do that simply this.
  if (Semantic == CES::Ignore)
    return;

  bool ContractCanThrow = StmtCanThrow(S.getCond());
  const bool EmitAsThrowing =
      ContractCanThrow && getLangOpts().ContractExceptions;

  CurContractData.reset(
      new CGContractData{.Contract = &S,
                         .Checkpoint = EmittingContract,
                         .Semantic = Semantic,
                         .Violation = createBasicBlock("contract.violation"),
                         .End = createBasicBlock("contract.end")});
  if (Semantic != ContractEvaluationSemantic::QuickEnforce) {
    CurContractData->ViolationInfoGV =
        CGM.GetAddrOfUnnamedGlobalConstantDecl(
               getContext().BuildViolationObject(
                   &S, dyn_cast_or_null<FunctionDecl>(CurFuncDecl)))
            .getPointer();
  }

  if (EmitAsThrowing) {
    return EmitContractStmtAsThrowing(S);
  } else {
    return EmitSimpleContract(*this, EmitScalarExpr(S.getCond()));
  }

  llvm_unreachable("Done handling things");
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

void CodeGenFunction::EmitContractStmtAsThrowing(const ContractStmt &S) {
  assert(CurContractData && CurContractData->Contract == &S &&
         CurContractData->Checkpoint == EmittingContract);

  CurContractData->EHPredicateStore = CreateTempAlloca(
      Builder.getInt1Ty(), CharUnits::One(), "contract.pred.value");
  // Set the initial value to true. If the contract throws, we'll see the true
  // value after the catch block is done handling the exception.
  Builder.CreateStore(Builder.getTrue(), CurContractData->EHPredicateStore);

  assert(Builder.GetInsertBlock());
  EnsureInsertPoint();

  auto *Try = BuildTryCatch(S, *this);
  EnterCXXTryStmt(*Try);
  CurContractData->Checkpoint = EmittingTryBody;
  EmitStmt(Try->getTryBlock());
  CurContractData->Checkpoint = EmittingCatchBody;
  ExitCXXTryStmt(*Try);
  CurContractData->Checkpoint = EmittingContract;

  auto *PredVal = Builder.CreateLoad(CurContractData->EHPredicateStore);
  EmitSimpleContract(*this, PredVal);
}
