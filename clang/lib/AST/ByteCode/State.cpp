//===--- State.cpp - State chain for the VM and AST Walker ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "State.h"
#include "Frame.h"
#include "Source.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/OptionalDiagnostic.h"
#include "clang/AST/StmtCXX.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace clang;
using namespace clang::interp;

State::~State() {}

bool State::emitRelaxedDiag(SourceLocation Loc, diag::kind DiagId) {
  if (!Ctx.getLangOpts().MSVCCompat ||
      (!EvalStatus.ExtendedDiag && !InConstantContext))
    return false;

  switch (DiagId) {
  case diag::note_constexpr_invalid_cast_ptrtoint:
    addExtendedDiag(Loc, diag::warn_relaxed_constant_fold_cast);
    return true;
  case diag::note_constexpr_null_subobject:
    addExtendedDiag(Loc, diag::warn_relaxed_constant_fold_null);
    return true;
  default:
    return false;
  }
}

OptionalDiagnostic State::FFDiag(SourceLocation Loc, diag::kind DiagId,
                                 unsigned ExtraNotes) {
  return diag(Loc, DiagId, ExtraNotes, /*IsFFDiag=*/true);
}

OptionalDiagnostic State::FFDiag(const Expr *E, diag::kind DiagId,
                                 unsigned ExtraNotes) {
  EvalStatus.DiagEmitted = true;
  if (EvalStatus.Diag)
    return diag(E->getExprLoc(), DiagId, ExtraNotes, /*IsFFDiag=*/true);
  setActiveDiagnostic(false);
  return OptionalDiagnostic();
}

OptionalDiagnostic State::FFDiag(SourceInfo SI, diag::kind DiagId,
                                 unsigned ExtraNotes) {
  EvalStatus.DiagEmitted = true;
  if (EvalStatus.Diag)
    return diag(SI.getLoc(), DiagId, ExtraNotes, /*IsFFDiag=*/true);
  setActiveDiagnostic(false);
  return OptionalDiagnostic();
}

OptionalDiagnostic State::CCEDiag(SourceLocation Loc, diag::kind DiagId,
                                  unsigned ExtraNotes) {
  if (emitRelaxedDiag(Loc, DiagId)) {
    setActiveDiagnostic(false);
    return OptionalDiagnostic();
  }
  EvalStatus.DiagEmitted = true;
  // Don't override a previous diagnostic. Don't bother collecting
  // diagnostics if we're evaluating for overflow.
  if (!EvalStatus.Diag || !EvalStatus.Diag->empty()) {
    setActiveDiagnostic(false);
    return OptionalDiagnostic();
  }
  return diag(Loc, DiagId, ExtraNotes, /*IsFFDiag=*/false);
}

OptionalDiagnostic State::CCEDiag(const Expr *E, diag::kind DiagId,
                                  unsigned ExtraNotes) {
  return CCEDiag(E->getExprLoc(), DiagId, ExtraNotes);
}

OptionalDiagnostic State::CCEDiag(SourceInfo SI, diag::kind DiagId,
                                  unsigned ExtraNotes) {
  return CCEDiag(SI.getLoc(), DiagId, ExtraNotes);
}

OptionalDiagnostic State::Note(SourceLocation Loc, diag::kind DiagId) {
  if (!hasActiveDiagnostic())
    return OptionalDiagnostic();
  return OptionalDiagnostic(&addDiag(Loc, DiagId));
}

OptionalDiagnostic State::Note(SourceInfo SI, diag::kind DiagId) {
  if (!hasActiveDiagnostic())
    return OptionalDiagnostic();
  return OptionalDiagnostic(&addDiag(SI.getLoc(), DiagId));
}

DiagnosticBuilder State::report(SourceLocation Loc, diag::kind DiagId) {
  return Ctx.getDiagnostics().Report(Loc, DiagId);
}

/// Add a diagnostic to the diagnostics list.
PartialDiagnostic &State::addDiag(SourceLocation Loc, diag::kind DiagId) {
  PartialDiagnostic PD(DiagId, Ctx.getDiagAllocator());
  EvalStatus.Diag->push_back(std::make_pair(Loc, PD));
  return EvalStatus.Diag->back().second;
}

void State::addExtendedDiag(SourceLocation Loc, diag::kind DiagId) {
  if (!EvalStatus.ExtendedDiag)
    return;
  PartialDiagnostic PD(DiagId, Ctx.getDiagAllocator());
  EvalStatus.ExtendedDiag->push_back(std::make_pair(Loc, PD));
}

OptionalDiagnostic State::diag(SourceLocation Loc, diag::kind DiagId,
                               unsigned ExtraNotes, bool IsFFDiag) {
  if (EvalStatus.Diag) {
  EvalStatus.DiagEmitted = true;
    if (hasPriorDiagnostic()) {
      return OptionalDiagnostic();
    }

    unsigned CallStackNotes = getCallStackDepth() - 1;
    unsigned Limit = Ctx.getDiagnostics().getConstexprBacktraceLimit();
    if (Limit)
      CallStackNotes = std::min(CallStackNotes, Limit + 1);
    if (checkingPotentialConstantExpression())
      CallStackNotes = 0;

    setActiveDiagnostic(true);
    setFoldFailureDiagnostic(IsFFDiag);
    EvalStatus.Diag->clear();
    EvalStatus.Diag->reserve(1 + ExtraNotes + CallStackNotes);
    addDiag(Loc, DiagId);
    if (!checkingPotentialConstantExpression()) {
      addCallStack(Limit);
    }
    return OptionalDiagnostic(&(*EvalStatus.Diag)[0].second);
  }
  setActiveDiagnostic(false);
  return OptionalDiagnostic();
}

void State::addCallStack(unsigned Limit) {
  // Determine which calls to skip, if any.
  unsigned ActiveCalls = getCallStackDepth() - 1;
  unsigned SkipStart = ActiveCalls, SkipEnd = SkipStart;
  if (Limit && Limit < ActiveCalls) {
    SkipStart = Limit / 2 + Limit % 2;
    SkipEnd = ActiveCalls - Limit / 2;
  }

  // Walk the call stack and add the diagnostics.
  unsigned CallIdx = 0;
  const Frame *Top = getCurrentFrame();
  for (const Frame *F = Top; F->getCaller() != nullptr;
       F = F->getCaller(), ++CallIdx) {
    SourceRange CallRange = F->getCallRange();
    assert(CallRange.isValid());

    // Skip this call?
    if (CallIdx >= SkipStart && CallIdx < SkipEnd) {
      if (CallIdx == SkipStart) {
        // Note that we're skipping calls.
        addDiag(CallRange.getBegin(), diag::note_constexpr_calls_suppressed)
            << unsigned(ActiveCalls - Limit);
      }
      continue;
    }

    // Use a different note for an inheriting constructor, because from the
    // user's perspective it's not really a function at all.
    if (const auto *CD =
            dyn_cast_if_present<CXXConstructorDecl>(F->getCallee());
        CD && CD->isInheritingConstructor()) {
      addDiag(CallRange.getBegin(),
              diag::note_constexpr_inherited_ctor_call_here)
          << CD->getParent();
      continue;
    }

    SmallString<128> Buffer;
    llvm::raw_svector_ostream Out(Buffer);
    F->describe(Out);
    if (!Buffer.empty())
      addDiag(CallRange.getBegin(), diag::note_constexpr_call_here)
          << Out.str() << CallRange;
  }
}

bool State::hasPriorDiagnostic() {
  if (!EvalStatus.Diag->empty()) {
    switch (EvalMode) {
    case EvaluationMode::ConstantFold:
    case EvaluationMode::IgnoreSideEffects:
      if (!HasFoldFailureDiagnostic)
        break;
      // We've already failed to fold something. Keep that diagnostic.
      [[fallthrough]];
    case EvaluationMode::ConstantExpression:
    case EvaluationMode::ConstantExpressionUnevaluated:
      setActiveDiagnostic(false);
      return true;
    }
  }
  return false;
}

bool State::shouldCheckContract(const ContractStmt *S) {
  if (S->getSemantic(Ctx) == ContractEvaluationSemantic::Ignore)
    return false;

  // Evaluating a contract assertion never makes an expression ineligible to
  // be a core constant expression, so contract assertions are irrelevant
  // when checking for a potential constant expression.
  if (checkingPotentialConstantExpression())
    return false;

  if (IgnoreContracts) {
    EvalStatus.HasIgnoredContractAssertion = true;
    return false;
  }
  return true;
}

State::ContractCheck::ContractCheck(State &S, const ContractStmt *CS)
    : S(S), CS(CS), OldDiag(S.EvalStatus.Diag),
      OldDiagEmitted(S.EvalStatus.DiagEmitted),
      OldHasActiveDiagnostic(S.HasActiveDiagnostic),
      OldHasFoldFailureDiagnostic(S.HasFoldFailureDiagnostic),
      OldHasSideEffects(S.EvalStatus.HasSideEffects),
      OldHasUndefinedBehavior(S.EvalStatus.HasUndefinedBehavior) {
  // The notes explaining why the predicate is not a core constant expression
  // are only needed if the enclosing evaluation collects diagnostics: its own
  // or those of observed contract violations. Collecting them is expensive
  // (each one describes the whole call stack), and would happen for every
  // failing predicate of every expression that Sema merely tries to fold.
  // Whether the predicate produced a diagnostic is tracked regardless.
  S.EvalStatus.Diag = (OldDiag || S.EvalStatus.ContractDiag) ? &Notes : nullptr;
  S.EvalStatus.DiagEmitted = false;
  S.HasActiveDiagnostic = false;
  S.HasFoldFailureDiagnostic = false;
}

void State::ContractCheck::restoreDiagnostics() {
  if (Restored)
    return;
  S.EvalStatus.Diag = OldDiag;
  PredicateDiagEmitted = S.EvalStatus.DiagEmitted;
  // The diagnostics of the predicate have been collected separately; they
  // don't count as emitted for the enclosing evaluation.
  S.EvalStatus.DiagEmitted = OldDiagEmitted;
  S.HasActiveDiagnostic = OldHasActiveDiagnostic;
  S.HasFoldFailureDiagnostic = OldHasFoldFailureDiagnostic;
  Restored = true;
}

bool State::ContractCheck::finish(bool Evaluated, bool Value) {
  restoreDiagnostics();

  // C++26 [basic.contract.eval]p7:
  //   A contract violation occurs when
  //   - B is false,
  //   - the evaluation of the predicate exits via an exception, or
  //   - the evaluation of the predicate is performed in a context that is
  //     manifestly constant-evaluated and the predicate is not a core
  //     constant expression.
  //
  // An exception cannot escape from a predicate that is a core constant
  // expression. Outside of a manifestly constant-evaluated context we only
  // care whether the predicate could be folded.
  bool IsConstant = Evaluated && (!PredicateDiagEmitted ||
                                  !S.checkingContractsAtCompileTime());
  if (IsConstant && Value)
    return true;

  if (!S.handleContractViolation(CS, /*PredicateIsFalse=*/IsConstant, Notes))
    return false;

  // Evaluation continues after the contract violation. Don't let a predicate
  // that is not a core constant expression affect whether the enclosing
  // expression is one.
  if (!IsConstant && S.checkingContractsAtCompileTime()) {
    S.EvalStatus.HasSideEffects = OldHasSideEffects;
    S.EvalStatus.HasUndefinedBehavior = OldHasUndefinedBehavior;
  }
  return true;
}

bool State::beginFoldFailureDiagnostic() {
  if (!EvalStatus.Diag || hasPriorDiagnostic()) {
    setActiveDiagnostic(false);
    return false;
  }
  setActiveDiagnostic(true);
  setFoldFailureDiagnostic(true);
  EvalStatus.Diag->clear();
  return true;
}

void State::addContractViolation(SmallVectorImpl<PartialDiagnosticAt> &Diags,
                                 diag::kind DiagId, const ContractStmt *S,
                                 bool PredicateIsFalse,
                                 ArrayRef<PartialDiagnosticAt> PredicateNotes) {
  // addDiag() and addCallStack() append to EvalStatus.Diag.
  llvm::SaveAndRestore DiagRAII(EvalStatus.Diag, &Diags);

  const Expr *Cond = S->getCond();
  addDiag(Cond->getExprLoc(), DiagId)
      << S->getSemanticString(Ctx) << llvm::to_underlying(S->getContractKind())
      << S->getSourceText(Ctx) << !PredicateIsFalse << Cond->getSourceRange();

  // The notes for a predicate that is not a constant expression already end
  // with the call stack.
  if (!PredicateIsFalse && !PredicateNotes.empty())
    llvm::append_range(Diags, PredicateNotes);
  else
    addCallStack(Ctx.getDiagnostics().getConstexprBacktraceLimit());
}

bool State::handleContractViolation(
    const ContractStmt *S, bool PredicateIsFalse,
    ArrayRef<PartialDiagnosticAt> PredicateNotes) {
  ContractEvaluationSemantic Sem = S->getSemantic(Ctx);
  assert(Sem != ContractEvaluationSemantic::Ignore &&
         "ignored contract assertions are not checked");
  bool IsObserve = Sem == ContractEvaluationSemantic::Observe;

  if (IgnoreContractViolations)
    return true;

  // The predicate itself evaluated a contract assertion that was violated
  // with a terminating semantic. The program is ill-formed no matter what
  // the semantic of this contract assertion is.
  if (EvalStatus.HasContractViolation) {
    if (beginFoldFailureDiagnostic())
      llvm::append_range(*EvalStatus.Diag, PredicateNotes);
    return false;
  }

  if (!checkingContractsAtCompileTime()) {
    // This contract assertion is evaluated at run time, where a contract
    // violation invokes the contract-violation handler or terminates the
    // program. That can be treated as a side-effect, unless we know that
    // execution doesn't continue past the contract assertion.
    if ((!PredicateIsFalse || IsObserve) && noteSideEffect())
      return true;
    if (beginFoldFailureDiagnostic()) {
      if (PredicateIsFalse || PredicateNotes.empty())
        addContractViolation(*EvalStatus.Diag,
                             diag::note_constexpr_contract_violation, S,
                             PredicateIsFalse, PredicateNotes);
      else
        llvm::append_range(*EvalStatus.Diag, PredicateNotes);
    }
    return false;
  }

  // C++26 [basic.contract.eval]p8:
  //   If a contract violation occurs in a context that is manifestly
  //   constant-evaluated, and the evaluation semantic is a terminating
  //   semantic, the program is ill-formed.
  //   [Note: A diagnostic is produced if the evaluation semantic is
  //   observe.]
  if (IsObserve) {
    if (EvalStatus.ContractDiag)
      addContractViolation(*EvalStatus.ContractDiag,
                           diag::warn_constexpr_contract_violation, S,
                           PredicateIsFalse, PredicateNotes);
    return true;
  }

  EvalStatus.HasContractViolation = true;
  if (beginFoldFailureDiagnostic())
    addContractViolation(*EvalStatus.Diag,
                         diag::note_constexpr_contract_violation, S,
                         PredicateIsFalse, PredicateNotes);
  return false;
}

bool State::keepEvaluatingAfterFailure() const {
  uint64_t Limit = Ctx.getLangOpts().ConstexprStepLimit;
  if (Limit != 0 && !stepsLeft())
    return false;

  switch (EvalMode) {
  case EvaluationMode::ConstantExpression:
  case EvaluationMode::ConstantExpressionUnevaluated:
  case EvaluationMode::ConstantFold:
  case EvaluationMode::IgnoreSideEffects:
    return checkingPotentialConstantExpression() ||
           checkingForUndefinedBehavior();
  }
  llvm_unreachable("Missed EvalMode case");
}

bool State::keepEvaluatingAfterSideEffect() const {
  switch (EvalMode) {
  case EvaluationMode::IgnoreSideEffects:
    return true;

  case EvaluationMode::ConstantExpression:
  case EvaluationMode::ConstantExpressionUnevaluated:
  case EvaluationMode::ConstantFold:
    // By default, assume any side effect might be valid in some other
    // evaluation of this expression from a different context.
    return checkingPotentialConstantExpression() ||
           checkingForUndefinedBehavior();
  }
  llvm_unreachable("Missed EvalMode case");
}

bool State::keepEvaluatingAfterUndefinedBehavior() const {
  switch (EvalMode) {
  case EvaluationMode::IgnoreSideEffects:
  case EvaluationMode::ConstantFold:
    return true;

  case EvaluationMode::ConstantExpression:
  case EvaluationMode::ConstantExpressionUnevaluated:
    return checkingForUndefinedBehavior();
  }
  llvm_unreachable("Missed EvalMode case");
}
