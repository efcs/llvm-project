//===--- CGContracts.cpp - Emit LLVM Code for C++ contracts ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit C++26 contract assertions.
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
#include "clang/AST/ASTLambda.h"
#include "clang/AST/Attr.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/CodeGen/ConstantInitBuilder.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Module.h"
#include <algorithm>
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

// The evaluation semantic and detection mode are passed to the violation
// handler entry point as 'unsigned char', matching the underlying type of
// std::contracts::evaluation_semantic and std::contracts::detection_mode used
// by the library's definition of __handle_contract_violation_v3.
template <class T>
static llvm::Constant *CreateConstantInt(CodeGenFunction &CGF, T Sem) {
  static_assert(std::is_same_v<T, ContractEvaluationSemantic> ||
                std::is_same_v<T, ContractDetectionMode>);
  return llvm::ConstantInt::get(CGF.Int8Ty, (unsigned)Sem);
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

void CodeGenFunction::EmitHandleContractViolationCall(
    llvm::Constant *EvalSemantic, llvm::Constant *DetectionMode,
    llvm::Value *ViolationInfoGV, bool IsNoReturn) {
  auto &Ctx = getContext();

  CanQualType ArgTypes[3] = {Ctx.UnsignedCharTy, Ctx.UnsignedCharTy,
                             Ctx.VoidPtrTy};

  const CGFunctionInfo &VFuncInfo =
      CGM.getTypes().arrangeBuiltinFunctionDeclaration(getContext().VoidTy,
                                                       ArgTypes);

  // This must match the definition in the runtime library:
  //   extern "C" void __handle_contract_violation_v3(
  //       evaluation_semantic, detection_mode, void*);
  // where both enumerations have 'unsigned char' as their underlying type.
  StringRef TargetFuncName = "__handle_contract_violation_v3";
  llvm::FunctionType *VFTy = CGM.getTypes().GetFunctionType(VFuncInfo);
  llvm::FunctionCallee VFunc = CGM.CreateRuntimeFunction(VFTy, TargetFuncName);
  if (auto *F = dyn_cast<llvm::Function>(VFunc.getCallee());
      F && F->isDeclaration())
    CGM.SetLLVMFunctionAttributes(GlobalDecl(), VFuncInfo, F,
                                  /*IsThunk=*/false);

  CallArgList Args;
  Args.add(RValue::get(EvalSemantic), Ctx.UnsignedCharTy);
  Args.add(RValue::get(DetectionMode), Ctx.UnsignedCharTy);
  Args.add(RValue::get(ViolationInfoGV), Ctx.VoidPtrTy);

  llvm::CallBase *CallOrInvoke = nullptr;
  EmitCall(VFuncInfo, CGCallee::forDirect(VFunc), ReturnValueSlot(), Args,
           &CallOrInvoke);

  if (IsNoReturn) {
    CallOrInvoke->setDoesNotReturn();
    // For an invoke, the insertion point is now the normal destination, which
    // is unreachable.
    EnsureInsertPoint();
    Builder.CreateUnreachable();
    Builder.ClearInsertionPoint();
  }
}

// Check if function can throw based on prototype noexcept, also works for
// destructors which are implicitly noexcept but can be marked noexcept(false).
static bool FunctionCanThrow(const FunctionDecl *D) {
  if (!D)
    return false;
  if (D->hasAttr<NoThrowAttr>())
    return false;
  const auto *Proto = D->getType()->getAs<FunctionProtoType>();
  if (!Proto) {
    // Function proto is not found, we conservatively assume throwing.
    return true;
  }
  return !isNoexceptExceptionSpec(Proto->getExceptionSpecType()) ||
         Proto->canThrow() != CT_Cannot;
}

static bool DestructorCanThrow(QualType T) {
  if (const auto *RD = T->getBaseElementTypeUnsafe()->getAsCXXRecordDecl())
    if (const CXXDestructorDecl *Dtor = RD->getDestructor())
      return FunctionCanThrow(Dtor);
  return false;
}

/// Determine whether evaluating \p S can exit via an exception. This is a
/// conservative analysis: it returns true unless the expression is known not
/// to throw.
static bool StmtCanThrow(const Stmt *S) {
  if (!S)
    return false;

  switch (S->getStmtClass()) {
  case Stmt::CXXThrowExprClass:
  case Stmt::ObjCMessageExprClass:
  case Stmt::ObjCAtThrowStmtClass:
    return true;

  // Unevaluated operands.
  case Stmt::CXXNoexceptExprClass:
    return false;
  case Stmt::UnaryExprOrTypeTraitExprClass:
    // The operand of sizeof is unevaluated unless it has a VLA type, in which
    // case we conservatively look at it.
    if (!cast<UnaryExprOrTypeTraitExpr>(S)->getTypeOfArgument()
             ->isVariablyModifiedType())
      return false;
    break;

  case Stmt::ConstantExprClass:
    // A constant expression that has been evaluated is emitted as a constant.
    if (cast<ConstantExpr>(S)->hasAPValueResult())
      return false;
    break;

  case Stmt::CXXTypeidExprClass: {
    const auto *TE = cast<CXXTypeidExpr>(S);
    if (!TE->isPotentiallyEvaluated())
      return false;
    // typeid of a dereferenced null pointer throws std::bad_typeid.
    if (TE->hasNullCheck())
      return true;
    break;
  }

  case Stmt::CXXDynamicCastExprClass:
    // A dynamic_cast to a reference type throws std::bad_cast on failure.
    if (!cast<CXXDynamicCastExpr>(S)->getType()->isPointerType())
      return true;
    break;

  case Stmt::CallExprClass:
  case Stmt::CXXMemberCallExprClass:
  case Stmt::CXXOperatorCallExprClass:
  case Stmt::UserDefinedLiteralClass:
  case Stmt::CUDAKernelCallExprClass: {
    const auto *CE = cast<CallExpr>(S);
    if (const FunctionDecl *Callee = CE->getDirectCallee()) {
      if (FunctionCanThrow(Callee))
        return true;
    } else {
      // Indirect call. We can only tell from the type of the callee.
      QualType CalleeTy = CE->getCallee()->getType();
      if (const auto *PT = CalleeTy->getAs<PointerType>())
        CalleeTy = PT->getPointeeType();
      else if (const auto *MPT = CalleeTy->getAs<MemberPointerType>())
        CalleeTy = MPT->getPointeeType();
      const auto *Proto = CalleeTy->getAs<FunctionProtoType>();
      if (!Proto || !Proto->isNothrow())
        return true;
    }
    break;
  }

  case Stmt::CXXConstructExprClass:
  case Stmt::CXXTemporaryObjectExprClass:
    if (FunctionCanThrow(cast<CXXConstructExpr>(S)->getConstructor()))
      return true;
    break;

  case Stmt::CXXInheritedCtorInitExprClass:
    if (FunctionCanThrow(cast<CXXInheritedCtorInitExpr>(S)->getConstructor()))
      return true;
    break;

  case Stmt::CXXNewExprClass: {
    const auto *NE = cast<CXXNewExpr>(S);
    // An array new with a non-constant bound may throw
    // std::bad_array_new_length.
    if (NE->isArray())
      return true;
    if (FunctionCanThrow(NE->getOperatorNew()))
      return true;
    break;
  }

  case Stmt::CXXDeleteExprClass: {
    const auto *DE = cast<CXXDeleteExpr>(S);
    if (FunctionCanThrow(DE->getOperatorDelete()))
      return true;
    QualType DT = DE->getDestroyedType();
    if (!DT.isNull() && DestructorCanThrow(DT))
      return true;
    break;
  }

  case Stmt::CXXBindTemporaryExprClass:
    // The destructor call of the temporary is not represented in children(),
    // so check it explicitly.
    if (FunctionCanThrow(
            cast<CXXBindTemporaryExpr>(S)->getTemporary()->getDestructor()))
      return true;
    break;

  case Stmt::CXXDefaultArgExprClass:
    return StmtCanThrow(cast<CXXDefaultArgExpr>(S)->getExpr());
  case Stmt::CXXDefaultInitExprClass:
    return StmtCanThrow(cast<CXXDefaultInitExpr>(S)->getExpr());
  case Stmt::OpaqueValueExprClass:
    return StmtCanThrow(cast<OpaqueValueExpr>(S)->getSourceExpr());

  case Stmt::LambdaExprClass: {
    // Creating the closure object only evaluates the capture initializers;
    // the body is not evaluated.
    for (const Expr *Init : cast<LambdaExpr>(S)->capture_inits())
      if (StmtCanThrow(Init))
        return true;
    return false;
  }

  default:
    break;
  }

  for (const Stmt *Child : S->children())
    if (StmtCanThrow(Child))
      return true;

  return false;
}

/// Returns true if \p FD has a function contract assertion (of kind \p Kind,
/// if given) which is evaluated with a checking semantic.
static bool hasCheckedContracts(const FunctionDecl *FD,
                                std::optional<ContractKind> Kind) {
  // Function contract specifiers may be omitted on redeclarations.
  if (!FD->hasContracts()) {
    const FunctionDecl *WithContracts = nullptr;
    for (const FunctionDecl *Redecl : FD->redecls()) {
      if (Redecl->hasContracts()) {
        WithContracts = Redecl;
        break;
      }
    }
    if (!WithContracts)
      return false;
    FD = WithContracts;
  }
  const ASTContext &Ctx = FD->getASTContext();
  for (const ContractStmt *S : FD->contracts())
    if ((!Kind || S->getContractKind() == *Kind) &&
        S->getSemantic(Ctx) != Ignore)
      return true;
  return false;
}

bool CodeGenFunction::hasCheckedFunctionContracts(const FunctionDecl *FD) {
  return hasCheckedContracts(FD, std::nullopt);
}

bool CodeGenFunction::hasCheckedPreconditions(const FunctionDecl *FD) {
  return hasCheckedContracts(FD, ContractKind::Pre);
}

bool CodeGenFunction::hasCheckedPostconditions(const FunctionDecl *FD) {
  return hasCheckedContracts(FD, ContractKind::Post);
}

bool CodeGenFunction::shouldEmitFunctionContracts(GlobalDecl GD) {
  const auto *FD = cast<FunctionDecl>(GD.getDecl());
  const auto *MD = dyn_cast<CXXMethodDecl>(FD);

  if (!hasCheckedFunctionContracts(FD))
    return false;

  // The lambda static invoker forwards to the call operator, which evaluates
  // the contracts.
  if (MD && MD->isLambdaStaticInvoker())
    return false;

  // On targets using inalloca, the call operator of a lambda with a static
  // invoker forwards to a separate implementation function, which evaluates
  // the contracts.
  if (MD && isLambdaCallOperator(MD) && CurFnInfo &&
      !CurFnInfo->isDelegateCall() && MD->getParent()->getLambdaStaticInvoker() &&
      hasInAllocaArg(MD))
    return false;

  if (const auto *Ctor = dyn_cast<CXXConstructorDecl>(FD)) {
    // An inheriting constructor calls the inherited constructor, which
    // evaluates its own contracts.
    if (Ctor->isInheritingConstructor())
      return false;
    // The complete object constructor may simply delegate to the base object
    // constructor. See EmitConstructorBody.
    if (GD.getCtorType() == Ctor_Complete &&
        CGM.getTarget().getCXXABI().hasConstructorVariants() &&
        IsConstructorDelegationValid(Ctor))
      return false;
    return true;
  }

  if (const auto *Dtor = dyn_cast<CXXDestructorDecl>(FD)) {
    // These must stay in sync with EmitDestructorBody.
    switch (GD.getDtorType()) {
    case Dtor_Deleting:
    case Dtor_VectorDeleting:
      // Always delegates to the complete object destructor.
      return false;
    case Dtor_Complete:
      // Complete destructors of abstract classes are never called, and
      // otherwise the complete destructor delegates to the base destructor
      // unless the body is a function-try-block.
      if (Dtor->getParent()->isAbstract())
        return false;
      return isa_and_nonnull<CXXTryStmt>(Dtor->getBody());
    case Dtor_Base:
    case Dtor_Unified:
    case Dtor_Comdat:
      return true;
    }
    llvm_unreachable("unhandled destructor type");
  }

  return true;
}

void CodeGenFunction::EmitPreconditions(const FunctionDecl *FD) {
  for (const ContractStmt *S : FD->preconditions())
    EmitStmt(S);
}

void CodeGenFunction::BeginFunctionPostconditions() {
  assert(ContractsFD && !PostconditionsSavedReturnBlock.isValid());
  if (!hasCheckedPostconditions(ContractsFD))
    return;

  // [expr.call]p10, [stmt.return]p5: The postcondition assertions are
  // evaluated after the result object has been initialized and the variables
  // (and temporaries) of the function body have been destroyed, but before
  // the function parameters are destroyed.
  //
  // The only cleanups on the stack at this point are those of the parameters
  // which are destroyed in the callee, so make return statements branch to a
  // block inside the scope of these cleanups. FinishFunction evaluates the
  // postconditions there once the cleanups of the function body have been
  // popped.
  PostconditionsSavedReturnBlock = ReturnBlock;
  PostconditionsCleanupDepth = EHStack.stable_begin();
  ReturnBlock = getJumpDestInCurrentScope("contract.post");
}

void CodeGenFunction::EmitFunctionPostconditions(SourceLocation EndLoc) {
  assert(ContractsFD && PostconditionsSavedReturnBlock.isValid());
  assert(EHStack.stable_begin() == PostconditionsCleanupDepth &&
         "cleanups of the function body must have been popped");

  llvm::BasicBlock *PostBB = ReturnBlock.getBlock();
  ReturnBlock = PostconditionsSavedReturnBlock;
  PostconditionsSavedReturnBlock = JumpDest();

  // Avoid emitting a separate block in simple cases, like EmitReturnBlock.
  if (llvm::BasicBlock *CurBB = Builder.GetInsertBlock()) {
    assert(!CurBB->hasTerminator() && "Unexpected terminated block.");
    if (CurBB->empty() || PostBB->use_empty()) {
      PostBB->replaceAllUsesWith(CurBB);
      delete PostBB;
    } else {
      EmitBlock(PostBB);
    }
  } else if (PostBB->use_empty()) {
    // The function never returns normally, so the postcondition assertions
    // are never evaluated.
    delete PostBB;
    return;
  } else {
    auto *BI = PostBB->hasOneUse()
                   ? dyn_cast<llvm::BranchInst>(*PostBB->user_begin())
                   : nullptr;
    if (BI && BI->isUnconditional() && BI->getSuccessor(0) == PostBB) {
      Builder.SetInsertPoint(BI->getParent());
      BI->eraseFromParent();
      delete PostBB;
    } else {
      EmitBlock(PostBB);
    }
  }

  // If evaluating a postcondition exits via an exception (e.g. because the
  // violation handler throws), the behavior is as if the function body exited
  // via that exception ([basic.contract.eval]p17). The result object, which
  // has already been initialized, must be destroyed.
  EHScopeStack::stable_iterator CleanupDepth = EHStack.stable_begin();
  if (getLangOpts().Exceptions && ReturnValue.isValid() &&
      !FnRetTy->isReferenceType()) {
    if (QualType::DestructionKind DtorKind = FnRetTy.isDestructedType())
      pushDestroy(EHCleanup, ReturnValue, FnRetTy, getDestroyer(DtorKind),
                  /*useEHCleanupForArray=*/true);
  }

  for (const ContractStmt *S : ContractsFD->postconditions())
    EmitStmt(S);

  PopCleanupBlocks(CleanupDepth);

  // The remainder of the epilogue is not part of any postcondition.
  if (CGDebugInfo *DI = getDebugInfo())
    DI->EmitLocation(Builder, EndLoc);
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
