//===--- SemaContract.cpp - Semantic Analysis for Contracts ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for contracts.
//
//===----------------------------------------------------------------------===//

#include "TreeTransform.h"
#include "TypeLocBuilder.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/AST/ASTLambda.h"
#include "clang/AST/ASTStructuralEquivalence.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/CharUnits.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/IgnoreExpr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtObjC.h"
#include "clang/AST/TypeLoc.h"
#include "clang/AST/TypeOrdering.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/EnterExpressionEvaluationContext.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/SemaCUDA.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/SemaObjC.h"
#include "clang/Sema/SemaOpenMP.h"
#include "clang/Sema/Template.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"

using namespace clang;
using namespace sema;
using llvm::DenseMap;
using llvm::DenseSet;

class clang::SemaContractHelper {
public:
  static SmallVector<const Attr *>
  buildAttributesWithDummyNode(Sema &S, ParsedAttributes &Attrs,
                               SourceLocation Loc) {
    // We shouldn't end up actually emitting any diagnostics pointing an the
    // dummy node, but we need to have a valid source location for any
    // diagnostics. (Pray they don't call getEndLoc()), which will attempt to
    // read past the end of the buffer.

    ContractStmt Dummy(Stmt::EmptyShell(), ContractKind::Pre, false, 0);
    Dummy.KeywordLoc = Loc;
    SmallVector<const Attr *> Result;
    S.ProcessStmtAttributes(&Dummy, Attrs, Result);
    return Result;
  }
};

namespace {
template <class T> struct ValueRAII {
  ValueRAII(T &XDest, T const &Value, bool Enter = true) : Dest(&XDest) {
    if (Enter)
      push(Value);
  }

  ~ValueRAII() { pop(); }

  ValueRAII(ValueRAII const &) = delete;
  ValueRAII &operator=(ValueRAII const &) = delete;

  void push(T const &Value) {
    assert(!Entered);
    OldValue = *Dest;
    *Dest = Value;
    Entered = true;
  }

  void pop() {
    if (Entered)
      *Dest = OldValue;
    Entered = false;
  }

private:
  bool Entered = false;
  T *Dest;
  T OldValue;
};

template <class T> ValueRAII(T &, T const &) -> ValueRAII<T>;

} // namespace

ExprResult Sema::ActOnContractAssertCondition(Expr *Cond)  {
  assert(isContractAssertionContext() && "Wrong context for statement");

  // Finish the full-expression while the contract assertion is still being
  // tracked, so that any captures it requires are attributed to it.
  if (Cond->isTypeDependent())
    return ActOnFinishFullExpr(Cond, /*DiscardedValue=*/false);

  ConditionResult Res = ActOnCondition(getCurScope(), Cond->getExprLoc(), Cond, Sema::ConditionKind::Boolean, /*MissingOK=*/false);
  if (Res.isInvalid())
    return ExprError();
  Cond = Res.get().second;
  assert(Cond);
  return Cond;
}

StmtResult Sema::BuildContractStmt(ContractKind CK, SourceLocation KeywordLoc,
                                   Expr *Cond, DeclStmt *RND,
                                   ArrayRef<const Attr *> Attrs,
                                   StringRef PredicateText,
                                   SourceRange ParenRange) {
  return ContractStmt::Create(Context, CK, KeywordLoc, Cond, RND, Attrs,
                              PredicateText, ParenRange.getBegin(),
                              ParenRange.getEnd());
}

std::string Sema::getContractPredicateText(SourceRange PredicateRange) const {
  if (PredicateRange.isInvalid())
    return std::string();

  const SourceManager &SM = getSourceManager();

  // Find the characters the predicate was written with. If its first and last
  // token are not written at the place of the contract assertion or in the
  // same macro argument, and do not start and end a macro expansion (i.e. if
  // part of the predicate comes from the replacement list of a macro), there
  // is no such range. Use the outermost macro invocation(s) instead.
  CharSourceRange Range = Lexer::makeFileCharRange(
      CharSourceRange::getTokenRange(PredicateRange), SM, getLangOpts());
  if (Range.isInvalid())
    Range = Lexer::getAsCharRange(SM.getExpansionRange(PredicateRange), SM,
                                  getLangOpts());
  if (Range.isInvalid())
    return std::string();

  FileIDAndOffset Begin = SM.getDecomposedLoc(Range.getBegin());
  FileIDAndOffset End = SM.getDecomposedLoc(Range.getEnd());
  if (Begin.first != End.first || End.second < Begin.second)
    return std::string();

  bool Invalid = false;
  StringRef Buffer = SM.getBufferData(Begin.first, &Invalid);
  if (Invalid || End.second > Buffer.size())
    return std::string();

  // Normalize the whitespace in the same way as the '#' operator: tokens are
  // separated by a single space if there was whitespace (including new-lines
  // and comments) between them.
  Lexer RawLexer(SM.getLocForStartOfFile(Begin.first), getLangOpts(),
                 Buffer.begin(), Buffer.begin() + Begin.second, Buffer.end());
  std::string Text;
  Token Tok;
  while (!RawLexer.LexFromRawLexer(Tok)) {
    if (SM.getFileOffset(Tok.getLocation()) >= End.second)
      break;
    if (!Text.empty() && (Tok.hasLeadingSpace() || Tok.isAtStartOfLine()))
      Text += ' ';
    Text += Lexer::getSpelling(Tok, SM, getLangOpts());
  }
  return Text;
}

StmtResult Sema::ActOnContractAssert(ContractKind CK, SourceLocation KeywordLoc,
                                     Expr *Cond, ResultNameDecl *RND,
                                     ParsedAttributes &ContractAttrs,
                                     SourceRange PredicateRange,
                                     SourceRange ParenRange) {

  // A result name is only permitted on a postcondition. We have already
  // diagnosed it on other kinds of contracts; don't attach it.
  if (RND && CK != ContractKind::Post) {
    assert(RND->isInvalidDecl() && "result name should have been diagnosed");
    RND = nullptr;
  }

  DeclStmt *RNDStmt = nullptr;
  if (RND) {
    StmtResult NewDeclStmt = ActOnDeclStmt(
        ConvertDeclToDeclGroup(RND), RND->getLocation(), RND->getLocation());
    if (NewDeclStmt.isInvalid())
      return StmtError();
    RNDStmt = NewDeclStmt.getAs<DeclStmt>();
  }

  StmtResult Res =
      BuildContractStmt(CK, KeywordLoc, Cond, RNDStmt,
                        SemaContractHelper::buildAttributesWithDummyNode(
                            *this, ContractAttrs, KeywordLoc),
                        getContractPredicateText(PredicateRange), ParenRange);

  if (Res.isInvalid())
    return StmtError();

  if (RND && RND->getType()->isUndeducedAutoType()) {
    return Res;
  }

  return ActOnFinishFullStmt(Res.get());
}

/// Declare the result binding introduced by the result-name-introducer of a
/// postcondition-specifier ([dcl.contract.res]).
ResultNameDecl *Sema::ActOnResultNameDeclarator(ContractKind CK, Scope *S,
                                                QualType RetType,
                                                SourceLocation IDLoc,
                                                IdentifierInfo *II,
                                                unsigned FunctionScopeDepth) {
  assert(II && "ResultName requires an identifier");

  bool IsInvalid = false;

  if (RetType->isVoidType()) {
    // Adjust the type of the result name to be int so we can actually produce a
    // node.
    RetType = Context.IntTy;
    Diag(IDLoc, diag::err_void_result_name) << II;
    IsInvalid = true;
  }

  // If the return type of the function is yet to be deduced, use a dependent
  // placeholder type for the result name. This only happens for a declaration
  // which is not a definition (which is ill-formed unless the function is
  // templated, see [dcl.contract.res]p2) and in the definition of a templated
  // function: otherwise, postconditions which name the result are parsed or
  // instantiated once the return type has been deduced.
  bool HasInventedPlaceholderTypes =
      RetType->isUndeducedType() && !RetType->isDependentType();
  if (HasInventedPlaceholderTypes)
    RetType = Context.getAutoType(DeducedKind::DeducedAsDependent, QualType(),
                                  AutoTypeKeyword::Auto);
  auto *New = ResultNameDecl::Create(Context, CurContext, IDLoc, II, RetType,
                                     nullptr, HasInventedPlaceholderTypes, FunctionScopeDepth);

  if (IsInvalid)
    New->setInvalidDecl();

  // [basic.scope.contract]p2: The result-name-introducer shall not
  // potentially conflict with a declaration in the function parameter scope
  // of the function.
  // FIXME: This should also apply to the lambda scope of the lambda-expression
  // the postcondition is associated with, if any (its init-captures).
  if (S) {
    LookupResult R(*this, II, IDLoc, LookupOrdinaryName,
                   RedeclarationKind::ForVisibleRedeclaration);
    LookupName(R, S);
    if (!R.empty()) {
      NamedDecl *PrevDecl = *R.begin();
      if (R.isSingleResult() && PrevDecl->isTemplateParameter()) {
        // Just pretend that we didn't see the previous declaration.
        PrevDecl = nullptr;
      }
      if (auto *PVD = dyn_cast_if_present<ParmVarDecl>(PrevDecl)) {
        Diag(IDLoc, diag::err_result_name_shadows_param) << II;
        Diag(PVD->getLocation(), diag::note_previous_declaration);
        New->setInvalidDecl(true);
      }
    }
  }

  if (CK != ContractKind::Post) {
    assert(II && "ResultName requires an identifier");

    Diag(IDLoc, diag::err_result_name_not_allowed) << II;
    New->setInvalidDecl(true);
  }

  // Add the parameter declaration into this scope.
  if (S)
    S->AddDecl(New);

  IdResolver.AddDecl(New);

  return New;
}

using ContractScopeRecord = Sema::ContractScopeRecord;

bool Sema::CheckEquivalentContractSequence(FunctionDecl *OldDecl,
                                           FunctionDecl *NewDecl) {
  // If the new declaration doesn't contain any contracts, that's fine, they can
  // be omitted.
  if (!NewDecl->hasContracts())
    return false;

  // For explicit specializations, we need to find the first declaration of the
  // explicit specialization itself that has spelled contracts, not the implicit
  // instantiation that may be in the redeclaration chain. The implicit
  // instantiation won't have contracts per [temp.expl.spec]p12.
  FunctionDecl *OrigDecl = nullptr;
  if (NewDecl->getTemplateSpecializationKind() == TSK_ExplicitSpecialization) {
    // If OldDecl is not an explicit specialization, then NewDecl is the first
    // explicit specialization declaration - no previous contracts to compare.
    if (OldDecl->getTemplateSpecializationKind() != TSK_ExplicitSpecialization)
      return false;

    // Find the first explicit specialization declaration that has spelled
    // contracts (i.e., contracts whose DeclContext matches the declaration).
    // Skip the implicit instantiation which won't have contracts.
    for (auto *Redecl : OldDecl->redecls()) {
      // The new declaration may already be part of the redeclaration chain.
      if (Redecl == NewDecl)
        continue;
      if (Redecl->getTemplateSpecializationKind() == TSK_ExplicitSpecialization &&
          Redecl->hasContracts() &&
          Redecl->getContracts()->getDeclContext() == Redecl) {
        OrigDecl = Redecl;
        break;
      }
    }
    // If we couldn't find an explicit specialization with spelled contracts,
    // NewDecl is the first one - nothing to compare.
    if (!OrigDecl)
      return false;
  } else {
    OrigDecl = OldDecl->getCanonicalDecl();
  }

  // If either sequence is invalid, we've already diagnosed it.
  if (NewDecl->getContracts()->isInvalidDecl())
    return false;
  if (OrigDecl->hasContracts() && OrigDecl->getContracts()->isInvalidDecl()) {
    NewDecl->getContracts()->setInvalidDecl(true);
    NewDecl->setInvalidDecl(true);
    return true;
  }
  ContractSpecifierDecl *OrigContractSpec = OrigDecl->getContracts();
  ContractSpecifierDecl *NewContractSpec = NewDecl->getContracts();
  ArrayRef<const ContractStmt *> OrigContracts, NewContracts;
  if (OrigContractSpec)
    OrigContracts = OrigContractSpec->contracts();

  NewContracts = NewContractSpec->contracts();

  if (OrigContractSpec && OrigContractSpec->isInvalidDecl()) {
    NewContractSpec->setInvalidDecl(true);
    NewDecl->setInvalidDecl(true);
    return true;
  }

  enum DifferenceKind {
    DK_None = -1,
    DK_OrigMissing,
    DK_NumContracts,
    DK_Kind,
    DK_ResultName,
    DK_Cond
  };
  unsigned ContractIndex = 0;
  bool ComparedLambdas = false;

  // [dcl.contract.func]p4:
  //   A declaration D of a function or function template f that is not a first
  //   declaration shall have either no function-contract-specifier-seq or the
  //   same function-contract-specifier-seq as any first declaration F
  //   reachable from D.
  const DifferenceKind DK = [&] {
    // Contracts may be omitted from following declarations.
    if (NewContracts.empty())
      return DK_None;

    // ... But if they exist, they must be present on the original declaration.
    if (OrigContracts.empty())
      return DK_OrigMissing;

    // ... And if they exist on the original declaration, they must be the same.
    ContractSpecifierDecl::Difference Diff =
        ContractSpecifierDecl::findFirstDifference(Context, OrigContractSpec,
                                                   NewContractSpec);
    ContractIndex = Diff.Index;
    ComparedLambdas = Diff.ComparedLambdas;
    switch (Diff.Kind) {
    case ContractSpecifierDecl::DifferenceKind::None:
      return DK_None;
    case ContractSpecifierDecl::DifferenceKind::NumContracts:
      return DK_NumContracts;
    case ContractSpecifierDecl::DifferenceKind::ContractKind:
      return DK_Kind;
    case ContractSpecifierDecl::DifferenceKind::ResultName:
      return DK_ResultName;
    case ContractSpecifierDecl::DifferenceKind::Predicate:
      return DK_Cond;
    }
    llvm_unreachable("unhandled difference kind");
  }();

  // Nothing to diagnose.
  if (DK == DK_None) {
    // [dcl.contract.func]p5: If this condition is not met solely due to the
    // comparison of two lambda-expressions that are contained within P1 and
    // P2, no diagnostic is required.
    //
    // Within a translation unit the closure types of two lambda-expressions
    // are always distinct. We nevertheless treat lambda-expressions which
    // consist of the same tokens as being the same, as is the case for
    // declarations in different translation units.
    bool InSameTU = !OrigDecl->isFromASTFile() || !OrigDecl->getOwningModule();
    if (ComparedLambdas && InSameTU)
      Diag(NewContracts.front()->getBeginLoc(),
           diag::ext_contract_redecl_repeats_lambda)
          << SourceRange(NewContracts.front()->getBeginLoc(),
                         NewContracts.back()->getEndLoc());
    return false;
  }

  NewContractSpec->setInvalidDecl(true);

  assert(!NewContracts.empty() && "Cannot diagnose empty contract sequence");
  SourceRange NewContractRange = SourceRange(
      NewContracts.front()->getBeginLoc(), NewContracts.back()->getEndLoc());
  SourceRange OrigContractRange =
      OrigContracts.empty()
          ? SourceRange(OrigDecl->getEndLoc(), OrigDecl->getEndLoc())
          : SourceRange(OrigContracts.front()->getBeginLoc(),
                        OrigContracts.back()->getEndLoc());

  // Otherwise, we're producing a diagnostic.
  Diag(NewDecl->getLocation(), diag::err_function_different_contract_seq)
      << isa<CXXMethodDecl>(NewDecl) << NewContractRange;

  if (DK == DK_NumContracts || DK == DK_OrigMissing) {
    int PluralSelect =
        OrigContracts.empty() + (OrigContracts.size() < NewContracts.size());
    Diag(OrigDecl->getLocation(), diag::note_contract_spec_seq_arity_mismatch)
        << PluralSelect << OrigContracts.size() << NewContracts.size()
        << OrigContractRange;
    return true;
  }

  auto *OC = OrigContracts[ContractIndex];
  auto *NC = NewContracts[ContractIndex];

  auto GetRangeForNote = [&](const ContractStmt *CS) {
    switch (DK) {
    case DK_Kind:
      return SourceRange(CS->getBeginLoc(), CS->getBeginLoc());
    case DK_ResultName:
      return CS->hasResultName() ? CS->getResultName()->getSourceRange()
                                 : CS->getCond()->getSourceRange();
    case DK_Cond:
      return CS->getCond()->getSourceRange();
    case DK_OrigMissing:
    case DK_NumContracts:
    case DK_None:
      llvm_unreachable("unhandled enum value");
    }
    llvm_unreachable("unhandled enum value");
  };

  Diag(NC->getBeginLoc(), diag::note_mismatched_contract)
      << GetRangeForNote(NC);
  Diag(OC->getBeginLoc(), diag::note_previous_contracts)
      << DK << (int)OC->getContractKind() << OC->hasResultName()
      << GetRangeForNote(OC);

  return true;
}

namespace {
/// A checker which white-lists certain expressions whose conversion
/// to or from retainable type would otherwise be forbidden in ARC.
struct ParamReferenceChecker : RecursiveASTVisitor<ParamReferenceChecker> {
  typedef RecursiveASTVisitor<ParamReferenceChecker> super;

private:
  Sema &Actions;
  const FunctionDecl *FD;

public:
  ContractStmt *CurrentContract = nullptr;
  ContractSpecifierDecl *CSD = nullptr;

public:
  ParamReferenceChecker(Sema &S, const FunctionDecl *FD)
      : Actions(S), FD(FD), CSD(FD->getContracts()) {
    assert(CSD);
  }

  bool TraverseContractStmt(ContractStmt *CS) {
    ValueRAII<ContractStmt *> SetCurrent(CurrentContract, CS,
                                         CurrentContract == nullptr);
    if (CS->getCond())
      TraverseStmt(CS->getCond());
    return true;
  }

  enum DiagSelector {
    DS_None = -1,
    DS_Array = 0,
    DS_Function = 1,
    DS_NotConst = 2
  };

  DiagSelector classifyDiagnosableParmVar(const ParmVarDecl *PVD,
                                          const DeclRefExpr *Usage) const {
    // We only care about parameter's for the function with the contracts we're
    // evaluating. Ensure this parameter belongs to that function.
    if (![&] {
          if (PVD->getFunctionScopeIndex() < FD->getNumParams())
            return FD->getParamDecl(PVD->getFunctionScopeIndex()) == PVD;
          return false;
        }())
      return DS_None;

    // We'll diagnose this when it's non-dependent
    if (PVD->getType()->isDependentType())
      return DS_None;

    // Skip diagnosing this parameter if we've already done it.
    if (DiagnosedDecls.count(PVD))
      return DS_None;

    // [dcl.contract.func] p2900r8 --
    //   If a  postcondition  odr-uses ([basic.def.odr]) a non-reference
    //   parameter...
    if (PVD->getType()->isReferenceType() || Usage->isNonOdrUse())
      return DS_None;

    QualType PVDType = PVD->getOriginalType();

    // that parameter shall not have an array type...
    if (PVDType->isArrayType() || PVDType->isArrayParameterType())
      return DS_Array;
    assert(!PVD->getOriginalType()->isArrayType());

    // or function type...
    if (PVDType->isFunctionPointerType())
      return DS_Function;

    // ...and that parameter shall be declared const
    if (!PVDType.isConstQualified())
      return DS_NotConst;

    return DS_None;
  }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    assert(CurrentContract && "No current contract to use in diagnostics?");

    // [dcl.contract.func] p2900r8 --
    //   If a  postcondition  odr-uses ([basic.def.odr]) a non-reference
    //   parameter, ... that parameter shall be declared const and shall not
    //   have array or function type. [ Note: This requirement applies even to
    //   declarations that do not specify the postcondition-specifier.]
    auto *PVD = dyn_cast_or_null<ParmVarDecl>(E->getDecl());
    if (!PVD || PVD->getType()->isDependentType() || DiagnosedDecls.count(PVD))
      return true;

    if (DiagSelector DiagSelect = classifyDiagnosableParmVar(PVD, E);
        DiagSelect != DS_None) {
      DiagnosedDecls.insert(PVD);
      CSD->setInvalidDecl(true);

      Actions.Diag(E->getLocation(),
                   diag::err_contract_postcondition_parameter_type_invalid)
          << PVD->getIdentifier() << DiagSelect
          << CurrentContract->getSourceRange();
      Actions.Diag(PVD->getTypeSpecStartLoc(), diag::note_parameter_type)
          << (DiagSelect == DS_NotConst ? PVD->getType()
                                        : PVD->getOriginalType())
          << PVD->getSourceRange();
    }

    return true;
  }

private:
  DenseSet<ParmVarDecl *> DiagnosedDecls;
};

} // namespace

static void diagnoseParamTypes(Sema &S, FunctionDecl *FD,
                               ContractSpecifierDecl *CSD) {

  // Check for post-conditions that reference non-const parameters.
  ParamReferenceChecker Checker(S, FD);
  for (auto *CS : CSD->postconditions())
    Checker.TraverseContractStmt(CS);
}

/// Diagnose parameters of the redeclaration \p Redecl which are not const, but
/// which are odr-used by a postcondition of the first declaration \p First.
static void diagnoseNonConstParamsOnRedeclaration(Sema &S, FunctionDecl *First,
                                                  FunctionDecl *Redecl) {
  if (First->getNumParams() != Redecl->getNumParams())
    return;

  struct Collector : RecursiveASTVisitor<Collector> {
    const FunctionDecl *FD;
    llvm::SmallSetVector<unsigned, 4> Indices;
    bool VisitDeclRefExpr(DeclRefExpr *E) {
      auto *PVD = dyn_cast<ParmVarDecl>(E->getDecl());
      if (PVD && E->isNonOdrUse() == NOUR_None &&
          PVD->getFunctionScopeIndex() < FD->getNumParams() &&
          FD->getParamDecl(PVD->getFunctionScopeIndex()) == PVD)
        Indices.insert(PVD->getFunctionScopeIndex());
      return true;
    }
  } C;
  C.FD = First;
  for (ContractStmt *CS : First->postconditions())
    C.TraverseStmt(CS->getCond());

  for (unsigned Idx : C.Indices) {
    ParmVarDecl *PVD = Redecl->getParamDecl(Idx);
    QualType T = PVD->getType();
    if (T->isDependentType() || T->isReferenceType() || T.isConstQualified())
      continue;
    // Array and function parameters were diagnosed on the first declaration.
    if (PVD->getOriginalType()->isArrayType() ||
        PVD->getOriginalType()->isFunctionPointerType())
      continue;
    S.Diag(PVD->getLocation(),
           diag::err_contract_postcondition_redecl_param_not_const)
        << PVD << T;
    S.Diag(First->getParamDecl(Idx)->getLocation(),
           diag::note_contract_postcondition_param_first_decl);
  }
}

void Sema::CheckFunctionContracts(FunctionDecl *FD, bool IsDefinition, bool IsInstantiation) {
  assert(FD && FD->hasContracts());

  diagnoseParamTypes(*this, FD, FD->getContracts());
}

/// Find the declaration whose spelled contracts should be instantiated for an
/// instantiation of \p Pattern. Instantiated member declarations don't share
/// the contracts of their pattern, so walk back through the member
/// instantiation chain, and through the redeclarations of the pattern (the
/// contracts may only appear on the first declaration).
static const FunctionDecl *findContractPattern(const FunctionDecl *Pattern) {
  while (Pattern) {
    if (Pattern->hasContracts())
      return Pattern;
    for (const FunctionDecl *Redecl : Pattern->redecls())
      if (Redecl->hasContracts())
        return Redecl;

    const FunctionDecl *Next = Pattern->getInstantiatedFromMemberFunction();
    if (!Next)
      if (FunctionTemplateDecl *FTD = Pattern->getDescribedFunctionTemplate())
        if (FunctionTemplateDecl *From = FTD->getInstantiatedFromMemberTemplate())
          Next = From->getTemplatedDecl();
    Pattern = Next;
  }
  return nullptr;
}

void Sema::InstantiateContractSpecifier(
    SourceLocation PointOfInstantiation, FunctionDecl *Instantiation,
    const FunctionDecl *Pattern,
    const MultiLevelTemplateArgumentList &TemplateArgs) {

  const FunctionDecl *ContractPattern = findContractPattern(Pattern);
  if (!ContractPattern)
    return;
  ContractSpecifierDecl *PatternCSD = ContractPattern->getContracts();
  bool IsInvalid = PatternCSD->isInvalidDecl();

  // The contracts are instantiated along with the definition of the function,
  // in its context and function scope (so that lambda-expressions within the
  // contracts can capture the parameters).
  assert(CurContext == Instantiation && getCurFunction() &&
         "contracts are instantiated with the definition of the function");

  LocalInstantiationScope Scope(*this, /*CombineWithOuterScope=*/true);

  // If the contracts were spelled on a different declaration than the
  // pattern definition, its parameters need to be mapped too.
  if (ContractPattern != Pattern &&
      ContractPattern->getNumParams() == Instantiation->getNumParams()) {
    for (unsigned I = 0, N = ContractPattern->getNumParams(); I != N; ++I) {
      const ParmVarDecl *PatternParam = ContractPattern->getParamDecl(I);
      if (PatternParam->isParameterPack())
        continue;
      Scope.InstantiatedLocal(PatternParam, Instantiation->getParamDecl(I));
    }
  }

  SmallVector<ContractStmt *> NewContracts;
  for (auto *CS : PatternCSD->contracts()) {
    StmtResult NewStmt = SubstStmt(CS, TemplateArgs);
    if (NewStmt.isInvalid())
      IsInvalid = true;
    else
      NewContracts.push_back(NewStmt.getAs<ContractStmt>());
  }

  ContractSpecifierDecl *NewCSD = BuildContractSpecifierDecl(
      NewContracts, Instantiation, PatternCSD->getLocation(), IsInvalid);
  assert(NewCSD);

  Instantiation->setContracts(NewCSD);
  if (!Instantiation->isDependentContext()) {
    // A function which became virtual by overriding a function of a
    // dependent base can only be diagnosed now.
    auto *PatternMD = dyn_cast<CXXMethodDecl>(ContractPattern);
    if (!PatternMD || !PatternMD->isVirtual())
      CheckFunctionContractPlacement(Instantiation);
    CheckFunctionContracts(Instantiation, /*IsDefinition=*/false,
                           /*IsInstantiation=*/true);
  }
}

ContractSpecifierDecl *
Sema::BuildContractSpecifierDecl(ArrayRef<ContractStmt *> Contracts,
                                 DeclContext *DC, SourceLocation Loc,
                                 bool IsInvalid) {
  ContractSpecifierDecl *CSD =
      ContractSpecifierDecl::Create(Context, DC, Loc, Contracts, IsInvalid);
  return CSD;
}

/// ActOnFinishContractSpecifierSequence - This is called after a
/// contract-specifier-seq has been parsed.
///
/// It's primary job is to set the canonical result name decl for each result
/// name.
ContractSpecifierDecl *
Sema::ActOnFinishContractSpecifierSequence(ArrayRef<ContractStmt *> Contracts,
                                           SourceLocation Loc, bool IsInvalid) {
  assert((!Contracts.empty() || IsInvalid) && "Expected at least one contract");

  return BuildContractSpecifierDecl(Contracts, CurContext, Loc, IsInvalid);
}

void Sema::ActOnContractsOnFinishFunctionDecl(FunctionDecl *D,
                                              bool IsDefinition) {

  FunctionDecl *FD;
  if (FunctionTemplateDecl *FunTmpl = dyn_cast<FunctionTemplateDecl>(D))
    FD = FunTmpl->getTemplatedDecl();
  else
    FD = D;

  // [temp.expl.spec]p12
  // ... and function-contract-specifier appearing in the declaration of a
  // template have no effect on an explicit specialization of
  // that template.
  const bool IsTemplateSpecialization = FD->getTemplateSpecializationKind() == TSK_ExplicitSpecialization;

  auto *First = FD->getFirstDecl();

  // If the new declaration/definition doesn't have contracts, and it's previous declarations didn't have
  // contracts or it's a specialiazation which doesn't inherit the contracts, then there's nothing to do.

  if (!FD->hasContracts() && !First->hasContracts()) {
    return;
  }



  // If the definition has omitted the contracts, but the first declaration has
  // them, we need to rebuild the contracts to refer to the parameters of the
  // definition.
  //
  // For function templates, we'll create a copy when we instantiate the definition.
  if (First->hasContracts() && !FD->hasContracts() && IsDefinition &&
      !FD->isTemplateInstantiation() && !IsTemplateSpecialization) {
    // Note: This case is mutually exclusive with the NonDependentPlaceholders
    // case, since we can't have a placeholder return type on a declaration that
    // isn't a definition.

    // TODO: I don't think we should be rebuilding for
    assert(FD->getTemplatedKind() != FunctionDecl::TK_FunctionTemplateSpecialization);
    assert(FD->getTemplatedKind() != FunctionDecl::TK_MemberSpecialization);

    ContractSpecifierDecl *NewCSD = RebuildContractSpecifierForDecl(First, FD);
    NewCSD->setOwningFunction(FD);
    FD->setContracts(NewCSD);
  }

  // [dcl.contract.func]p7: If a postcondition odr-uses a non-reference
  // parameter, that parameter shall have const type on all declarations,
  // including those that do not repeat the postconditions.
  if (!FD->hasContracts() && First != FD && First->hasContracts())
    diagnoseNonConstParamsOnRedeclaration(*this, First, FD);

  if (!FD->hasContracts())
    return;

  assert(FD->hasContracts());

  ContractSpecifierDecl *CSD = FD->getContracts();

  if (!D->isTemplateInstantiation()) {
    // Note: IsInstantiation here means whether we're calling during the instantiation of the contract specifier,
    // rather than whether the FD declares a function instantiation.
    CheckFunctionContracts(FD, IsDefinition, /*IsInstantiation=*/false);
  }

  // When the declared return type of a non-templated function contains a
  // placeholder type, a postcondition-specifier with a result-name-introducer
  // shall be present only on a definition.
  if (CSD->hasInventedPlaceholdersTypes() && !FD->isTemplateInstantiation() &&
      !FD->isTemplated()) {
    if (!IsDefinition && !FD->isThisDeclarationADefinition()) {
      Diag(CSD->getCanonicalResultName()->getLocation(),
           diag::err_auto_result_name_on_non_def_decl)
          << CSD->getCanonicalResultName();
      Diag(FD->getReturnTypeSourceRange().getBegin(),
           diag::note_function_return_type)
          << FD->getReturnTypeSourceRange();
      for (auto *RND : CSD->result_names())
        RND->setInvalidDecl(true);
    }
  }
}

bool Sema::CheckFunctionContractPlacement(FunctionDecl *FD) {
  ContractSpecifierDecl *CSD = FD->getContracts();
  if (!CSD || CSD->contracts().empty())
    return false;

  // [dcl.contract.func]p6
  //   A virtual function, a deleted function, or a function defaulted on its
  //   first declaration shall not have a function-contract-specifier-seq.
  enum { CPK_Virtual, CPK_Deleted, CPK_Defaulted } Kind;
  if (auto *MD = dyn_cast<CXXMethodDecl>(FD); MD && MD->isVirtual())
    Kind = CPK_Virtual;
  else if (FD->isDeletedAsWritten())
    Kind = CPK_Deleted;
  else if (FD->isExplicitlyDefaulted() && FD->isFirstDecl())
    Kind = CPK_Defaulted;
  else
    return false;

  Diag(CSD->contracts().front()->getBeginLoc(),
       diag::err_contract_on_function_kind)
      << Kind << FD;
  CSD->setInvalidDecl(true);
  return true;
}

void Sema::ActOnFinishLateParsedFunctionContracts(FunctionDecl *FD,
                                                  ContractSpecifierDecl *CSD,
                                                  bool IsDefinition) {
  assert(FD && "no function to attach contracts to");
  if (!CSD)
    return;

  FD->setContracts(CSD);
  IsDefinition |= FD->isThisDeclarationADefinition();

  CheckFunctionContractPlacement(FD);

  // [dcl.contract.func]p4: A declaration that is not a first declaration
  // shall have either no function-contract-specifier-seq or the same one as
  // the first declaration.
  if (FunctionDecl *Prev = FD->getPreviousDecl())
    CheckEquivalentContractSequence(Prev, FD);

  if (!FD->isInvalidDecl())
    ActOnContractsOnFinishFunctionDecl(FD, IsDefinition);
}

namespace {

// There's got to be a better way to do this.
struct RebuildFunctionContracts
    : public TreeTransform<RebuildFunctionContracts> {

  using Inherited = TreeTransform<RebuildFunctionContracts>;
  bool IsAlwaysRebuild = false;
  bool AlwaysRebuild() const { return IsAlwaysRebuild; }

  RebuildFunctionContracts(Sema &S, bool Rebuild = false)
      : TreeTransform<RebuildFunctionContracts>(S), IsAlwaysRebuild(Rebuild) {}
};

} // namespace

/// Rebuild the contract specifier written on one declaration in the context of
/// the definition. This rebinds parameters and result names as needed.
ContractSpecifierDecl *
Sema::RebuildContractSpecifierForDecl(FunctionDecl *First, FunctionDecl *Def) {

  assert(Def->getContracts() == First->getContracts() || !Def->hasContracts());
  Def->setContracts(nullptr);
  Sema::ContextRAII SavedContext(*this, Def);
  assert(FunctionScopes.empty());

  FunctionScopeRAII SavedFunctionContext(*this);
  PushFunctionScope();

  std::optional<Sema::CXXThisScopeRAII> ThisScope;
  if (auto *CXXMethod = dyn_cast<CXXMethodDecl>(Def);
      CXXMethod && CXXMethod->isImplicitObjectMemberFunction()) {
    ThisScope.emplace(*this, CXXMethod->getParent(),
                      CXXMethod->getMethodQualifiers(), /*Enabled=*/true);
  }
  RebuildFunctionContracts Rebuilder(*this, true);
  for (unsigned I = 0; I < First->getNumParams(); ++I) {
    Rebuilder.transformedLocalDecl(First->getParamDecl(I),
                                   Def->getParamDecl(I));
  }
  for (auto *RND : First->getContracts()->result_names()) {
    QualType Replacement = Def->getReturnType();
    auto *NewRND =
        ActOnResultNameDeclarator(ContractKind::Post, nullptr, Replacement,
                                  RND->getLocation(), RND->getIdentifier(),
                                  RND->getFunctionScopeDepth());
    Rebuilder.transformedLocalDecl(RND, NewRND);
  }
  SmallVector<ContractStmt *> NewContracts;
  bool IsInvalid = false;
  for (auto *CS : First->contracts()) {
    StmtResult NewStmt = Rebuilder.TransformContractStmt(CS);
    if (NewStmt.isInvalid())
      IsInvalid = true;
    else
      NewContracts.push_back(NewStmt.getAs<ContractStmt>());
  }

  auto *CSD = BuildContractSpecifierDecl(
      NewContracts, Def, First->getContracts()->getLocation(), IsInvalid);
  Def->setContracts(CSD);

  if (CSD->isInvalidDecl())
    Def->setInvalidDecl();
  return CSD;
}

namespace {
/// Diagnose odr-uses of non-reference parameters in the postconditions of a
/// coroutine.
struct CoroutineParamUseChecker
    : RecursiveASTVisitor<CoroutineParamUseChecker> {
  Sema &S;
  const FunctionDecl *FD;
  DenseSet<const ParmVarDecl *> Diagnosed;

  CoroutineParamUseChecker(Sema &S, const FunctionDecl *FD) : S(S), FD(FD) {}

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    auto *PVD = dyn_cast<ParmVarDecl>(E->getDecl());
    if (!PVD || E->isNonOdrUse() != NOUR_None ||
        PVD->getType()->isReferenceType() ||
        PVD->getFunctionScopeIndex() >= FD->getNumParams() ||
        FD->getParamDecl(PVD->getFunctionScopeIndex()) != PVD ||
        !Diagnosed.insert(PVD).second)
      return true;
    S.Diag(E->getLocation(), diag::err_coroutine_postcondition_uses_param)
        << PVD << E->getSourceRange();
    S.Diag(FD->getLocation(), diag::note_coroutine_postcondition_function)
        << FD;
    return true;
  }
};
} // namespace

static void diagnoseCoroutinePostconditionParams(Sema &S, FunctionDecl *FD) {
  CoroutineParamUseChecker Checker(S, FD);
  for (ContractStmt *CS : FD->postconditions())
    Checker.TraverseStmt(CS->getCond());
}

void Sema::ActOnReenterFunctionParameterScope(Scope *S, FunctionDecl *FD) {
  assert(CurContext == FD && "not in the context of the function");
  S->setEntity(FD);
  for (ParmVarDecl *Param : FD->parameters()) {
    if (Param->getIdentifier()) {
      S->AddDecl(Param);
      IdResolver.AddDecl(Param);
    }
  }
}

void Sema::ActOnContractsOnFinishFunctionBody(FunctionDecl *Def) {
  assert(Def);
  // [dcl.fct.def.coroutine]: A coroutine behaves as if the top-level
  // cv-qualifiers were removed from its parameters, so a postcondition of a
  // coroutine may not odr-use a non-reference parameter.
  if (Def->hasContracts() && getCurFunction() &&
      getCurFunction()->isCoroutine())
    diagnoseCoroutinePostconditionParams(*this, Def);
}

/// Return true if the entity declared in \p EntityDC at \p EntityLoc is
/// declared within the predicate of the contract assertion \p CSR, given that
/// it can be named from within that predicate.
static bool isDeclaredWithinContract(const Sema &S,
                                     const Sema::ContractScopeRecord &CSR,
                                     const DeclContext *EntityDC,
                                     SourceLocation EntityLoc) {
  const DeclContext *ContractDC = CSR.Context;
  if (!ContractDC->Encloses(EntityDC))
    return false;

  // An entity declared in a lambda-expression, block, requires-expression or
  // local class nested within the context of the contract assertion can only
  // be named from within the predicate if that construct is part of the
  // predicate.
  if (!ContractDC->Equals(EntityDC))
    return true;

  // The entity is declared in the same context as the contract assertion. It
  // is declared within the predicate if it is declared after the contract
  // keyword (e.g. in a statement-expression): declarations following the
  // predicate cannot be named from within it.
  return EntityLoc.isValid() && CSR.KeywordLoc.isValid() &&
         S.getSourceManager().isBeforeInTranslationUnit(CSR.KeywordLoc,
                                                        EntityLoc);
}

/// Return true if a lambda-expression whose function scope is at or above
/// index \p FirstScope captures (or would implicitly capture) the local
/// entity \p VD by copy, in which case an id-expression naming the entity in
/// the current context names the corresponding member of the closure type
/// rather than the entity itself ([expr.prim.id.unqual]p4).
static bool isCapturedByCopyInLambda(const Sema &S, unsigned FirstScope,
                                     const ValueDecl *VD) {
  ArrayRef<FunctionScopeInfo *> Scopes = S.FunctionScopes;
  auto *Entity = const_cast<ValueDecl *>(VD);
  for (unsigned I = FirstScope, E = Scopes.size(); I < E; ++I) {
    auto *LSI = dyn_cast<LambdaScopeInfo>(Scopes[I]);
    if (!LSI)
      continue;
    // Within the parameter-declaration-clause of a lambda-expression, the
    // id-expression names the entity itself.
    if (I + 1 == E && !LSI->AfterParameterList)
      continue;
    if (LSI->isCaptured(Entity)) {
      if (LSI->getCapture(Entity).isCopyCapture())
        return true;
      continue;
    }
    if (LSI->ImpCaptureStyle == CapturingScopeInfo::ImpCap_LambdaByval)
      return true;
  }
  return false;
}

bool Sema::isContractConstified(const ValueDecl *VD) const {
  assert(VD);
  const ContractScopeRecord *CSR = getEnclosingContractAssertion();

  // The first function scope which is nested within the predicate of the
  // innermost enclosing contract assertion.
  unsigned FirstNestedScope = FunctionScopesStart;
  if (CSR) {
    // If the function scopes don't describe the contexts nested within the
    // contract assertion (which happens when instantiating the call operator
    // of a generic lambda), we can't tell which lambda-expressions capture
    // the entity. The id-expressions for which that matters are rebuilt from
    // ones which already know whether they are const.
    if (CSR->FunctionScope &&
        (CSR->FunctionScopeIndex >= FunctionScopes.size() ||
         FunctionScopes[CSR->FunctionScopeIndex] != CSR->FunctionScope))
      FirstNestedScope = FunctionScopes.size();
    else if (CSR->FunctionScope)
      FirstNestedScope = CSR->FunctionScopeIndex + 1;
  }

  // [expr.prim.id.unqual]p6: An id-expression naming a result binding is
  // const, unless it names the member of a closure type declared for a
  // capture by copy of the result binding instead.
  if (isa<ResultNameDecl>(VD))
    return !isCapturedByCopyInLambda(*this, FirstNestedScope, VD);

  if (!CSR)
    return false;

  // [expr.prim.id.unqual]p7, [expr.prim.id.qual]p7:
  //   If the [id-expression] appears in the predicate of a contract assertion
  //   C and the entity is
  //    - a variable declared outside of C of object type T,
  //    - a variable or template parameter declared outside of C of type
  //      "reference to T", or
  //    - a structured binding of type T whose corresponding variable is
  //      declared outside of C,
  //   then the type of the expression is const T.
  if (const auto *NTTP = dyn_cast<NonTypeTemplateParmDecl>(VD))
    return NTTP->getType()->isReferenceType() &&
           !isDeclaredWithinContract(*this, *CSR, NTTP->getDeclContext(),
                                     NTTP->getLocation());

  const VarDecl *Var = nullptr;
  if (const auto *Binding = dyn_cast<BindingDecl>(VD))
    Var = dyn_cast_or_null<VarDecl>(Binding->getDecomposedDecl());
  else
    Var = dyn_cast<VarDecl>(VD);
  if (!Var)
    return false;

  if (isDeclaredWithinContract(*this, *CSR, Var->getDeclContext(),
                               Var->getLocation()))
    return false;

  // If a lambda-expression within the predicate captures the entity by copy,
  // the id-expression names the member of the closure type instead.
  if (Var->isLocalVarDeclOrParm() &&
      isCapturedByCopyInLambda(*this, FirstNestedScope, VD))
    return false;

  return true;
}

bool Sema::isCXXThisContractConstified(
    const CXXRecordDecl *CopyCapturingClosure) const {
  const ContractScopeRecord *CSR = getEnclosingContractAssertion();
  if (!CSR)
    return false;

  // [expr.prim.this]p3:
  //   If the expression this appears within the predicate of a contract
  //   assertion (including as the result of an implicit transformation and
  //   including in the bodies of nested lambda-expressions) and the current
  //   class encloses the contract assertion, const is combined with the
  //   cv-qualifier-seq used to generate the resulting type.
  //
  // The current class does not enclose the contract assertion if there is a
  // class (other than a closure type) between the current context and the
  // contract assertion.
  for (const DeclContext *DC = CurContext; DC && !CSR->Context->Equals(DC);
       DC = DC->getParent())
    if (const auto *RD = dyn_cast<CXXRecordDecl>(DC); RD && !RD->isLambda())
      return false;

  // If '*this' is captured by copy by a lambda-expression within the
  // predicate, 'this' refers to the copy.
  if (CopyCapturingClosure &&
      CSR->Context->Encloses(CopyCapturingClosure->getDeclContext()))
    return false;

  return true;
}

void Sema::noteCaptureReference(ValueDecl *Var) {
  if (!getLangOpts().Contracts)
    return;

  // [expr.prim.lambda.closure]p10:
  //   If all potential references to a local entity implicitly captured by a
  //   lambda-expression L occur within the function contract assertions of
  //   the call operator or operator template of L or within
  //   assertion-statements within the body of L, the program is ill-formed.
  //
  // Walk the enclosing capturing scopes from the innermost one, keeping track
  // of the first contract assertion found on the way: the reference is within
  // that contract assertion for all of the lambda-expressions visited from
  // there on. The function scopes are only trusted as long as they match the
  // declaration contexts: the function scopes of entities which are not
  // lexically enclosing may be present further down the stack.
  SourceLocation ContractLoc;
  DeclContext *DC = CurContext;
  for (unsigned I = FunctionScopes.size(); I-- > 0;) {
    FunctionScopeInfo *FSI = FunctionScopes[I];
    auto *CSI = dyn_cast<CapturingScopeInfo>(FSI);
    if (!CSI)
      break;

    DeclContext *ScopeDC = nullptr;
    if (auto *LSI = dyn_cast<LambdaScopeInfo>(CSI))
      ScopeDC = LSI->CallOperator;
    else if (auto *BSI = dyn_cast<BlockScopeInfo>(CSI))
      ScopeDC = BSI->TheDecl;
    else if (auto *RSI = dyn_cast<CapturedRegionScopeInfo>(CSI))
      ScopeDC = RSI->TheCapturedDecl;
    // While parsing the lambda-introducer or the parameter-declaration-clause
    // of a lambda-expression, the current context is still its parent.
    bool InLambdaIntroducer = false;
    if (auto *LSI = dyn_cast<LambdaScopeInfo>(CSI);
        LSI && !LSI->AfterParameterList && I + 1 == FunctionScopes.size())
      InLambdaIntroducer = true;
    if (!ScopeDC || (ScopeDC != DC && !InLambdaIntroducer))
      break;

    if (ContractLoc.isInvalid())
      if (const ContractScopeRecord *CSR =
              getContractAssertionInFunctionScope(FSI))
        ContractLoc = CSR->KeywordLoc;

    // A lambda-expression may not have captured the entity (yet) if it is
    // nested within a generic lambda; it will when that is instantiated.
    if (Var) {
      if (CSI->isCaptured(Var))
        CSI->getCapture(Var).noteReference(ContractLoc);
      else if (ScopeDC->Equals(Var->getDeclContext()))
        break;
    } else if (CSI->isCXXThisCaptured()) {
      CSI->getCXXThisCapture().noteReference(ContractLoc);
    }

    DC = getLambdaAwareParentOfDeclContext(ScopeDC);
  }
}

bool Sema::isContractAssertionContext() const {
  const ContractScopeRecord *CSR = getEnclosingContractAssertion();
  return CSR && CSR->Context->Equals(CurContext);
}

void Sema::PushContractScope(ContractKind Kind, SourceLocation KeywordLoc) {
  assert(CurContext && "contract assertion outside of any context?");
  ContractScopeStack.push_back(
      {Kind, KeywordLoc, CurContext, getCurFunction(),
       static_cast<unsigned>(FunctionScopes.empty()
                                 ? 0
                                 : FunctionScopes.size() - 1)});
}

void Sema::PopContractScope() {
  assert(!ContractScopeStack.empty() && "no contract scope to pop");
  ContractScopeStack.pop_back();
}

const Sema::ContractScopeRecord *Sema::getEnclosingContractAssertion() const {
  // The current context is within the predicate of a contract assertion if it
  // is the context in which the predicate appears, or if it is a
  // lambda-expression, block, or local class nested within that context which
  // is part of the predicate. The latter excludes, for example, a generic
  // lambda defined before the contract assertion whose call operator is
  // instantiated while parsing the predicate.
  for (const ContractScopeRecord &CSR : llvm::reverse(ContractScopeStack)) {
    const DeclContext *ContractDC = CSR.Context;
    if (!ContractDC->Encloses(CurContext))
      continue;
    if (ContractDC->Equals(CurContext))
      return &CSR;

    // Find the outermost context nested within the contract's context, and
    // check that it is part of the predicate.
    const DeclContext *Outermost = CurContext;
    while (Outermost->getParent() &&
           !ContractDC->Equals(Outermost->getParent()))
      Outermost = Outermost->getParent();
    SourceLocation Loc = cast<Decl>(Outermost)->getLocation();
    if (Loc.isValid() && CSR.KeywordLoc.isValid() &&
        SourceMgr.isBeforeInTranslationUnit(Loc, CSR.KeywordLoc))
      continue;
    return &CSR;
  }
  return nullptr;
}

const Sema::ContractScopeRecord *Sema::getContractAssertionInFunctionScope(
    const sema::FunctionScopeInfo *FSI) const {
  for (const ContractScopeRecord &CSR : llvm::reverse(ContractScopeStack))
    if (CSR.FunctionScope == FSI)
      return &CSR;
  return nullptr;
}
