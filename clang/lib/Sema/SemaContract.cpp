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
#include "llvm/ADT/MapVector.h"
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
  // The predicate is a full-expression: the cleanups it requires (e.g. for
  // the closure object of a lambda-expression) belong to it, not to the
  // contract assertion. ActOnCondition leaves them pending when the condition
  // is already a full-expression (an immediate invocation) or when it had to
  // be wrapped in a recovery expression.
  Cond = MaybeCreateExprWithCleanups(Cond);
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

  SmallVector<const Attr *, 2> Attrs;
  ProcessContractAttributes(ContractAttrs, Attrs);

  StmtResult Res =
      BuildContractStmt(CK, KeywordLoc, Cond, RNDStmt, Attrs,
                        getContractPredicateText(PredicateRange), ParenRange);

  if (Res.isInvalid())
    return StmtError();

  // -Wunused-variable: a result binding which the predicate does not refer to
  // is almost certainly a mistake (e.g. a stray result-name-introducer).
  if (RND && !RND->isInvalidDecl() && !RND->isReferenced() &&
      !RND->hasAttr<UnusedAttr>() && !Cond->containsErrors() &&
      !inTemplateInstantiation())
    Diag(RND->getLocation(), diag::warn_unused_result_name) << RND;

  return Res;
}

/// Declare the result binding introduced by the result-name-introducer of a
/// postcondition-specifier ([dcl.contract.res]).
ResultNameDecl *Sema::ActOnResultNameDeclarator(
    ContractKind CK, Scope *S, QualType RetType, SourceLocation IDLoc,
    IdentifierInfo *II, unsigned FunctionScopeDepth, ParsedAttributes *Attrs) {
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

  if (CK != ContractKind::Post) {
    Diag(IDLoc, diag::err_result_name_not_allowed)
        << II << (CK == ContractKind::Assert);
    New->setInvalidDecl(true);
  }

  // [basic.scope.contract]p2: The result-name-introducer shall not
  // potentially conflict with a declaration whose target scope is the
  // function parameter scope of the function or, for a lambda-expression,
  // its lambda scope (that is, its init-captures).
  if (S && CK == ContractKind::Post) {
    LookupResult R(*this, II, IDLoc, LookupOrdinaryName,
                   RedeclarationKind::ForVisibleRedeclaration);
    LookupName(R, S);
    if (!R.empty()) {
      NamedDecl *PrevDecl = *R.begin();
      if (R.isSingleResult() && PrevDecl->isTemplateParameter()) {
        // [temp.local]p6: The name of a template-parameter shall not be bound
        // to any following declaration whose locus is contained by the scope
        // to which the template-parameter belongs.
        DiagnoseTemplateParameterShadow(IDLoc, PrevDecl);
        New->setInvalidDecl(true);
        PrevDecl = nullptr;
      }
      auto *VD = dyn_cast_if_present<VarDecl>(PrevDecl);
      LambdaScopeInfo *LSI =
          VD && VD->isInitCapture() ? getCurLambda() : nullptr;
      if (VD && isa<ParmVarDecl>(VD) &&
          VD->getDeclContext()->Equals(CurContext)) {
        Diag(IDLoc, diag::err_result_name_shadows_param) << II << 0;
        Diag(VD->getLocation(), diag::note_previous_declaration);
        New->setInvalidDecl(true);
      } else if (LSI && LSI->isCaptured(VD) &&
                 LSI->getCapture(VD).isInitCapture()) {
        Diag(IDLoc, diag::err_result_name_shadows_param) << II << 1;
        Diag(VD->getLocation(), diag::note_previous_declaration);
        New->setInvalidDecl(true);
      } else if (PrevDecl && isa<VarDecl, FieldDecl, BindingDecl>(PrevDecl) &&
                 !Diags.isIgnored(diag::warn_decl_shadow, IDLoc)) {
        // -Wshadow: a result binding hides variables and fields like a local
        // variable does.
        CheckShadow(New, PrevDecl, R);
      }
    }
  }

  // The attributes of the attributed-identifier appertain to the result
  // binding; only [[maybe_unused]] is meaningful for it.
  if (Attrs) {
    for (const ParsedAttr &AL : *Attrs) {
      if (AL.isInvalid() || AL.getKind() == ParsedAttr::IgnoredAttribute)
        continue;
      if (AL.getKind() == ParsedAttr::UnknownAttribute) {
        DiagnoseUnknownAttribute(AL);
        continue;
      }
      if (AL.getKind() == ParsedAttr::AT_Unused) {
        if (AL.checkExactlyNumArgs(*this, 0))
          New->addAttr(UnusedAttr::Create(Context, AL));
        continue;
      }
      Diag(AL.getLoc(), diag::err_attribute_invalid_on_result_name)
          << AL << AL.isRegularKeywordAttribute() << AL.getRange();
    }
  }

  // Add the result name into the contract-assertion scope so that it is
  // visible to name lookup while the predicate is parsed. Popping that scope
  // removes it from the IdResolver again. When there is no scope, the result
  // name is being rebuilt (instantiation, redeclarations) and references to it
  // are rebound via TreeTransform instead of name lookup; registering it with
  // the IdResolver would leave a stray declaration that hides later
  // block-scope declarations with the same name.
  if (S) {
    S->AddDecl(New);
    IdResolver.AddDecl(New);
  }

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
  // explicit specialization itself that has contracts, not the implicit
  // instantiation that may be in the redeclaration chain. The implicit
  // instantiation won't have contracts per [temp.expl.spec]p12.
  FunctionDecl *OrigDecl = nullptr;
  if (NewDecl->getTemplateSpecializationKind() == TSK_ExplicitSpecialization) {
    // If OldDecl is not an explicit specialization, then NewDecl is the first
    // explicit specialization declaration - no previous contracts to compare.
    if (OldDecl->getTemplateSpecializationKind() != TSK_ExplicitSpecialization)
      return false;

    // Find the first explicit specialization declaration that has contracts.
    // Skip the implicit instantiation which won't have contracts.
    for (auto *Redecl : OldDecl->redecls()) {
      // The new declaration may already be part of the redeclaration chain.
      if (Redecl == NewDecl)
        continue;
      if (Redecl->getTemplateSpecializationKind() == TSK_ExplicitSpecialization &&
          Redecl->hasContracts()) {
        OrigDecl = Redecl;
        break;
      }
    }
    // If we couldn't find an explicit specialization with contracts, NewDecl
    // is the first one - nothing to compare.
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
/// Traverse the discarded-value expression \p E (the left operand of a comma
/// operator) with the visitor \p V, without visiting the id-expressions in
/// its set of potential results ([basic.def.odr]p3): a non-reference variable
/// named by one of them is not odr-used ([basic.def.odr]p5.3), as the
/// lvalue-to-rvalue conversion is not applied to a discarded-value expression
/// of non-volatile type ([expr.context]p2).
template <class Visitor>
bool traverseDiscardedValueExpr(Visitor &V, Expr *E) {
  while (true) {
    E = E->IgnoreParens();
    if (isa<DeclRefExpr>(E))
      return true;
    // The operand of a cast to void is a discarded-value expression itself.
    if (auto *CE = dyn_cast<ExplicitCastExpr>(E);
        CE && CE->getCastKind() == CK_ToVoid) {
      E = CE->getSubExpr();
      continue;
    }
    if (auto *ICE = dyn_cast<ImplicitCastExpr>(E)) {
      // An array operand of a subscript decays to a pointer. An
      // lvalue-to-rvalue conversion is applied to a volatile glvalue.
      if (ICE->getCastKind() != CK_ArrayToPointerDecay &&
          ICE->getCastKind() != CK_NoOp)
        break;
      E = ICE->getSubExpr();
    } else if (auto *CO = dyn_cast<ConditionalOperator>(E)) {
      return V.TraverseStmt(CO->getCond()) &&
             traverseDiscardedValueExpr(V, CO->getTrueExpr()) &&
             traverseDiscardedValueExpr(V, CO->getFalseExpr());
    } else if (auto *BO = dyn_cast<BinaryOperator>(E)) {
      if (BO->getOpcode() == BO_Comma) {
        if (!traverseDiscardedValueExpr(V, BO->getLHS()))
          return false;
        E = BO->getRHS();
      } else if (BO->getOpcode() == BO_PtrMemD) {
        if (!V.TraverseStmt(BO->getRHS()))
          return false;
        E = BO->getLHS();
      } else {
        break;
      }
    } else if (auto *ME = dyn_cast<MemberExpr>(E)) {
      if (ME->isArrow() || !isa<FieldDecl>(ME->getMemberDecl()))
        break;
      E = ME->getBase();
    } else if (auto *ASE = dyn_cast<ArraySubscriptExpr>(E)) {
      if (!ASE->getBase()->IgnoreImpCasts()->getType()->isArrayType())
        break;
      if (!V.TraverseStmt(ASE->getIdx()))
        return false;
      E = ASE->getBase();
    } else {
      break;
    }
  }
  return V.TraverseStmt(E);
}

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

  /// The left operand of a comma operator is a discarded-value expression.
  bool TraverseBinaryOperator(BinaryOperator *BO) {
    if (BO->getOpcode() != BO_Comma)
      return super::TraverseBinaryOperator(BO);
    return traverseDiscardedValueExpr(*this, BO->getLHS()) &&
           TraverseStmt(BO->getRHS());
  }

  /// Only the selected element of a pack is named by a pack indexing
  /// expression.
  bool TraversePackIndexingExpr(PackIndexingExpr *E) {
    if (E->isFullySubstituted() && !E->expandsToEmptyPack())
      return TraverseStmt(E->getSelectedExpr());
    return super::TraversePackIndexingExpr(E);
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

namespace {
/// Collects the parameters of the function \p FD which are odr-used by the
/// predicates visited, by their function scope index, and the first
/// id-expression that names each of them.
struct OdrUsedParamCollector : RecursiveASTVisitor<OdrUsedParamCollector> {
  const FunctionDecl *FD;
  llvm::SmallMapVector<unsigned, const DeclRefExpr *, 4> Uses;

  explicit OdrUsedParamCollector(const FunctionDecl *FD) : FD(FD) {}

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    auto *PVD = dyn_cast<ParmVarDecl>(E->getDecl());
    if (PVD && E->isNonOdrUse() == NOUR_None &&
        PVD->getFunctionScopeIndex() < FD->getNumParams() &&
        FD->getParamDecl(PVD->getFunctionScopeIndex()) == PVD)
      Uses.try_emplace(PVD->getFunctionScopeIndex(), E);
    return true;
  }
  // The discarded left operand of a comma expression is not odr-used
  // ([basic.def.odr]p5.3), and only the selected element of a pack indexing
  // expression is.
  bool TraverseBinaryOperator(BinaryOperator *BO) {
    if (BO->getOpcode() != BO_Comma)
      return RecursiveASTVisitor<OdrUsedParamCollector>::TraverseBinaryOperator(
          BO);
    return traverseDiscardedValueExpr(*this, BO->getLHS()) &&
           TraverseStmt(BO->getRHS());
  }
  bool TraversePackIndexingExpr(PackIndexingExpr *E) {
    if (E->isFullySubstituted() && !E->expandsToEmptyPack())
      return TraverseStmt(E->getSelectedExpr());
    return RecursiveASTVisitor<OdrUsedParamCollector>::TraversePackIndexingExpr(
        E);
  }
};
} // namespace

/// Diagnose parameters of the redeclaration \p Redecl which are not const, but
/// which are odr-used by a postcondition of the declaration \p Owner that
/// introduces the contracts of the function.
static void diagnoseNonConstParamsOnRedeclaration(Sema &S,
                                                  const FunctionDecl *Owner,
                                                  FunctionDecl *Redecl) {
  // The parameters of the owning declaration were diagnosed with it.
  if (Owner->getNumParams() != Redecl->getNumParams() ||
      Owner->getContracts()->isInvalidDecl())
    return;

  OdrUsedParamCollector C(Owner);
  for (ContractStmt *CS : Owner->postconditions())
    C.TraverseStmt(CS->getCond());

  for (auto [Idx, Use] : C.Uses) {
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
    S.Diag(Owner->getParamDecl(Idx)->getLocation(),
           diag::note_contract_postcondition_param_first_decl);
  }
}

void Sema::CheckFunctionContracts(FunctionDecl *FD, bool IsDefinition, bool IsInstantiation) {
  assert(FD && FD->hasContracts());

  diagnoseParamTypes(*this, FD, FD->getContracts());
}

/// [dcl.contract.func]p7: a non-reference parameter which is odr-used by a
/// postcondition of a function shall be const on all declarations of the
/// function. For a template specialization \p Owner (whose contracts were
/// just instantiated), the parameter types of all declarations of the pattern
/// are checked after substitution: the specialization only has the parameters
/// of one of them.
static void diagnoseNonConstParamsOnPatternRedecls(
    Sema &S, FunctionDecl *Owner, const FunctionDecl *PatternOwner,
    const MultiLevelTemplateArgumentList &TemplateArgs) {
  OdrUsedParamCollector C(Owner);
  for (ContractStmt *CS : Owner->postconditions())
    C.TraverseStmt(CS->getCond());
  if (C.Uses.empty())
    return;

  for (const FunctionDecl *Redecl : PatternOwner->redecls()) {
    if (Redecl->getNumParams() != Owner->getNumParams())
      continue;
    for (auto [Idx, Use] : C.Uses) {
      const ParmVarDecl *PatternParam = Redecl->getParamDecl(Idx);
      ParmVarDecl *Param = Owner->getParamDecl(Idx);
      // The parameters of the specialization were instantiated from (and
      // diagnosed with the type of) the declaration they share a location
      // with; skip parameter packs.
      if (PatternParam->getLocation() == Param->getLocation() ||
          PatternParam->isParameterPack() ||
          Param->getType()->isReferenceType())
        continue;
      QualType T = S.SubstType(PatternParam->getType(), TemplateArgs,
                               PatternParam->getLocation(),
                               PatternParam->getDeclName());
      if (T.isNull() || T->isDependentType() || T->isReferenceType() ||
          T.isConstQualified())
        continue;
      S.Diag(Use->getLocation(),
             diag::err_contract_postcondition_parameter_type_invalid)
          << Param->getIdentifier() << /*must be declared const*/ 2
          << Use->getSourceRange();
      S.Diag(PatternParam->getTypeSpecStartLoc(), diag::note_parameter_type)
          << T << PatternParam->getSourceRange();
      Owner->getContracts()->setInvalidDecl(true);
    }
  }
}

void Sema::InstantiateFunctionContracts(SourceLocation PointOfInstantiation,
                                        FunctionDecl *Function) {
  if (Function->isInvalidDecl() || !Function->hasUninstantiatedContracts())
    return;

  // The first declaration of the specialization owns its contracts, like the
  // first declaration of any function does.
  FunctionDecl *Owner = Function->getCanonicalDecl();
  const FunctionDecl *PatternOwner = Function->getContractInstantiationPattern();
  ContractSpecifierDecl *PatternCSD = PatternOwner->getContracts();
  assert(PatternCSD && PatternCSD->getDeclContext() == PatternOwner);

  // A result binding has the return type of the function. If that is yet to
  // be deduced, the contracts are instantiated once the definition has been
  // (see InstantiateFunctionDefinition).
  if (Owner->getReturnType()->isUndeducedType() &&
      PatternCSD->hasCanonicalResultName())
    return;

  RecursiveInstGuard AlreadyInstantiating(
      *this, Owner, RecursiveInstGuard::Kind::FunctionContracts);
  if (AlreadyInstantiating)
    return;

  // The contracts are not part of the immediate context of a substitution.
  NonSFINAEContext _(*this);

  // Attribute the instantiation to the function, unless its definition is
  // being instantiated (which already does).
  std::optional<InstantiatingTemplate> Inst;
  if (CodeSynthesisContexts.empty() ||
      CodeSynthesisContexts.back().Kind !=
          CodeSynthesisContext::TemplateInstantiation ||
      CodeSynthesisContexts.back().Entity != Function) {
    Inst.emplace(*this, PointOfInstantiation, Owner);
    if (Inst->isInvalid())
      return;
  }

  // Enter the context of the owning declaration, and push a function scope so
  // that lambda-expressions within the predicates can capture its parameters.
  ContextRAII SavedContext(*this, Owner);
  EnterExpressionEvaluationContext EvalContext(
      *this, ExpressionEvaluationContext::PotentiallyEvaluated);
  LocalInstantiationScope Scope(*this);
  MultiLevelTemplateArgumentList TemplateArgs = getTemplateInstantiationArgs(
      Owner, Owner->getLexicalDeclContext(), /*Final=*/false,
      /*Innermost=*/std::nullopt, /*RelativeToPrimary=*/true);

  std::optional<FunctionScopeRAII> FnScope;
  if (!isLambdaCallOperator(Owner)) {
    FnScope.emplace(*this);
    PushFunctionScope();
  }
  // The predicates could reference any lambda captures.
  LambdaScopeForCallOperatorInstantiationRAII PushLambdaCaptures(
      *this, Owner, TemplateArgs, Scope);

  std::optional<CXXThisScopeRAII> ThisScope;
  if (auto *MD = dyn_cast<CXXMethodDecl>(Owner);
      MD && MD->isImplicitObjectMemberFunction())
    ThisScope.emplace(*this, MD->getParent(), MD->getMethodQualifiers(),
                      /*Enabled=*/true);

  // Map the parameters of the declaration which introduces the contracts to
  // those of the specialization, expanding parameter packs. The names and
  // types of the latter are left alone: they were instantiated from one of
  // the declarations of the pattern, which need not be this one.
  bool IsInvalid = PatternCSD->isInvalidDecl();
  unsigned ParamIdx = 0;
  for (const ParmVarDecl *PatternParam : PatternOwner->parameters()) {
    if (!PatternParam->isParameterPack()) {
      if (ParamIdx >= Owner->getNumParams()) {
        IsInvalid = true;
        break;
      }
      Scope.InstantiatedLocal(PatternParam, Owner->getParamDecl(ParamIdx++));
      continue;
    }
    Scope.MakeInstantiatedLocalArgPack(PatternParam);
    UnsignedOrNone NumExpansions =
        getNumArgumentsInExpansion(PatternParam->getType(), TemplateArgs);
    for (unsigned I = 0, N = NumExpansions.value_or(0); I != N; ++I) {
      if (ParamIdx >= Owner->getNumParams()) {
        IsInvalid = true;
        break;
      }
      Scope.InstantiatedLocalPackArg(PatternParam,
                                     Owner->getParamDecl(ParamIdx++));
    }
  }

  SmallVector<ContractStmt *> NewContracts;
  if (!IsInvalid) {
    for (ContractStmt *CS : PatternCSD->contracts()) {
      StmtResult NewStmt = SubstStmt(CS, TemplateArgs);
      if (NewStmt.isInvalid())
        IsInvalid = true;
      else
        NewContracts.push_back(NewStmt.getAs<ContractStmt>());
    }
  }

  ContractSpecifierDecl *NewCSD = BuildContractSpecifierDecl(
      NewContracts, Owner, PatternCSD->getLocation(), IsInvalid);
  Owner->setContracts(NewCSD);

  if (!Owner->isDependentContext()) {
    // A function which became virtual by overriding a function of a
    // dependent base can only be diagnosed now.
    auto *PatternMD = dyn_cast<CXXMethodDecl>(PatternOwner);
    if (!PatternMD || !PatternMD->isVirtual())
      CheckFunctionContractPlacement(Owner);
    CheckFunctionContracts(Owner, /*IsDefinition=*/false,
                           /*IsInstantiation=*/true);
    if (!NewCSD->isInvalidDecl())
      diagnoseNonConstParamsOnPatternRedecls(*this, Owner, PatternOwner,
                                             TemplateArgs);
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

/// Diagnose function contract specifiers on a naked function: it has no
/// prologue or epilogue in which its contract assertions could be evaluated.
static void diagnoseContractsOnNakedFunction(Sema &S, FunctionDecl *FD) {
  // Diagnose on the declaration which introduces the attribute (the contracts
  // may be on an earlier declaration), once.
  const auto *NA = FD->getAttr<NakedAttr>();
  if (!NA || NA->isInherited())
    return;
  ContractSpecifierDecl *CSD = FD->getContractsInEffect();
  if (!CSD || CSD->contracts().empty())
    return;
  S.Diag(CSD->contracts().front()->getBeginLoc(),
         diag::err_contract_on_function_kind)
      << /*naked function*/ 3 << FD;
  S.Diag(NA->getLocation(), diag::note_attribute);
  if (FD->hasContracts())
    CSD->setInvalidDecl(true);
}

void Sema::ActOnContractsOnFinishFunctionDecl(FunctionDecl *FD,
                                              bool IsDefinition) {
  diagnoseContractsOnNakedFunction(*this, FD);

  // A declaration which omits the function-contract-specifier-seq: the
  // contracts introduced by the first declaration apply to it
  // ([dcl.contract.func]p3, p4), referring to the parameters of that
  // declaration. Nothing is copied onto this one.
  if (!FD->hasContracts()) {
    // The contracts of a function declared by instantiating a template are
    // needed when the function is defined ([dcl.contract.func]p9).
    if (IsDefinition && FD->hasUninstantiatedContracts())
      InstantiateFunctionContracts(FD->getLocation(), FD);

    ContractSpecifierDecl *CSD = FD->getContractsInEffect();
    if (!CSD)
      return;
    const auto *Owner = cast<FunctionDecl>(CSD->getDeclContext());
    if (Owner == FD)
      return;

    // [dcl.contract.func]p7: If a postcondition odr-uses a non-reference
    // parameter, that parameter shall have const type on all declarations,
    // including those that do not repeat the postconditions.
    diagnoseNonConstParamsOnRedeclaration(*this, Owner, FD);

    // The parameters that the predicates name are used by this declaration of
    // the function as well.
    if (IsDefinition && Owner->getNumParams() == FD->getNumParams())
      for (unsigned I = 0, N = FD->getNumParams(); I != N; ++I)
        if (Owner->getParamDecl(I)->isReferenced())
          FD->getParamDecl(I)->setReferenced();
    return;
  }

  ContractSpecifierDecl *CSD = FD->getContracts();

  if (!FD->isTemplateInstantiation()) {
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
/// Diagnose odr-uses of non-reference parameters in the postconditions of a
/// coroutine.
struct CoroutineParamUseChecker
    : RecursiveASTVisitor<CoroutineParamUseChecker> {
  Sema &S;
  /// The declaration that introduces the contracts, whose parameters the
  /// predicates refer to, and the definition of the coroutine.
  const FunctionDecl *Owner, *Def;
  DenseSet<const ParmVarDecl *> Diagnosed;

  CoroutineParamUseChecker(Sema &S, const FunctionDecl *Owner,
                           const FunctionDecl *Def)
      : S(S), Owner(Owner), Def(Def) {}

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    auto *PVD = dyn_cast<ParmVarDecl>(E->getDecl());
    if (!PVD || E->isNonOdrUse() != NOUR_None ||
        PVD->getType()->isReferenceType() ||
        PVD->getFunctionScopeIndex() >= Owner->getNumParams() ||
        Owner->getParamDecl(PVD->getFunctionScopeIndex()) != PVD ||
        !Diagnosed.insert(PVD).second)
      return true;
    S.Diag(E->getLocation(), diag::err_coroutine_postcondition_uses_param)
        << PVD << E->getSourceRange();
    S.Diag(Def->getLocation(), diag::note_coroutine_postcondition_function)
        << Def;
    return true;
  }
};
} // namespace

static void diagnoseCoroutinePostconditionParams(Sema &S, FunctionDecl *Def) {
  const ContractSpecifierDecl *CSD = Def->getContractsInEffect();
  if (!CSD)
    return;
  CoroutineParamUseChecker Checker(S, cast<FunctionDecl>(CSD->getDeclContext()),
                                   Def);
  for (ContractStmt *CS : CSD->postconditions())
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
  if (getCurFunction() && getCurFunction()->isCoroutine())
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
  // the id-expression names the member of the closure type instead. Only an
  // entity with automatic storage duration can be captured.
  if (Var->hasLocalStorage() &&
      isCapturedByCopyInLambda(*this, FirstNestedScope, VD))
    return false;

  return true;
}

bool Sema::DiagnoseImplicitMemberAccessInContract(
    const DeclarationNameInfo &NameInfo) {
  const ContractScopeRecord *CSR = getEnclosingContractAssertion();
  if (!CSR)
    return false;

  // [expr.prim.id.general]p2, [over.call.func]p3:
  //   If this transformation [into a class member access expression using
  //   (*this) as the object expression] occurs in the predicate of a
  //   precondition assertion of a constructor of X or a postcondition
  //   assertion of a destructor of X, the expression is ill-formed.
  bool IsCtorPre = CSR->Kind == ContractKind::Pre &&
                   isa<CXXConstructorDecl>(CSR->Context);
  bool IsDtorPost = CSR->Kind == ContractKind::Post &&
                    isa<CXXDestructorDecl>(CSR->Context);
  if (!IsCtorPre && !IsDtorPost)
    return false;

  // The standard's example allows an unevaluated operand (sizeof(b)), in
  // which the object is not accessed.
  if (isUnevaluatedContext())
    return false;

  // Within a class (other than a closure type) nested within the predicate,
  // 'this' does not refer to the object under construction or destruction.
  for (const DeclContext *DC = CurContext; DC && !CSR->Context->Equals(DC);
       DC = DC->getParent())
    if (const auto *RD = dyn_cast<CXXRecordDecl>(DC); RD && !RD->isLambda())
      return false;

  Diag(NameInfo.getLoc(), diag::err_contract_implicit_member_access_structor)
      << NameInfo.getName() << IsDtorPost;
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
