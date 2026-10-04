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

namespace {

/// Substitute the 'auto' specifier or deduced template specialization type
/// specifier within a type for a given replacement type.
class RebuildAutoResultName : public TreeTransform<RebuildAutoResultName> {
  QualType Replacement;
  using inherited = TreeTransform<RebuildAutoResultName>;

public:
  RebuildAutoResultName(Sema &SemaRef, QualType Replacement)
      : TreeTransform<RebuildAutoResultName>(SemaRef),
        Replacement(Replacement) {}

  QualType TransformAutoType(TypeLocBuilder &TLB, AutoTypeLoc TL) {
    // If we're building the type pattern to deduce against, don't wrap the
    // substituted type in an AutoType. Certain template deduction rules
    // apply only when a template type parameter appears directly (and not if
    // the parameter is found through desugaring). For instance:
    //   auto &&lref = lvalue;
    // must transform into "rvalue reference to T" not "rvalue reference to
    // auto type deduced as T" in order for [temp.deduct.call]p3 to apply.
    //
    // FIXME: Is this still necessary?

    QualType Result = SemaRef.Context.getAutoType(
        Replacement.isNull() ? DeducedKind::DeducedAsDependent
                             : DeducedKind::Deduced,
        Replacement, TL.getTypePtr()->getKeyword(),
        TL.getTypePtr()->getTypeConstraintConcept(),
        TL.getTypePtr()->getTypeConstraintArguments());
    auto NewTL = TLB.push<AutoTypeLoc>(Result);
    NewTL.copy(TL);
    return Result;
  }
};

} // namespace

ExprResult Sema::ActOnContractAssertCondition(Expr *Cond)  {
  assert(currentEvaluationContext().isContractAssertionContext() &&
         "Wrong context for statement");

  if (Cond->isTypeDependent())
    return Cond;

  ConditionResult Res = ActOnCondition(getCurScope(), Cond->getExprLoc(), Cond, Sema::ConditionKind::Boolean, /*MissingOK=*/false);
  if (Res.isInvalid())
    return ExprError();
  Cond = Res.get().second;
  assert(Cond);
  return Cond;
}

StmtResult Sema::BuildContractStmt(ContractKind CK, SourceLocation KeywordLoc,
                                   Expr *Cond, DeclStmt *RND,
                                   ArrayRef<const Attr *> Attrs) {
  return ContractStmt::Create(Context, CK, KeywordLoc, Cond, RND, Attrs);
}

StmtResult Sema::ActOnContractAssert(ContractKind CK, SourceLocation KeywordLoc,
                                     Expr *Cond, ResultNameDecl *RND,
                                     ParsedAttributes &ContractAttrs) {

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

    // FIXME(EricWF): Can this happen?
    if (NewDeclStmt.isInvalid())
      return StmtError();
    RNDStmt = NewDeclStmt.getAs<DeclStmt>();
  }

  StmtResult Res =
      BuildContractStmt(CK, KeywordLoc, Cond, RNDStmt,
                        SemaContractHelper::buildAttributesWithDummyNode(
                            *this, ContractAttrs, KeywordLoc));

  if (Res.isInvalid())
    return StmtError();

  if (RND && RND->getType()->isUndeducedAutoType()) {
    return Res;
  }

  return ActOnFinishFullStmt(Res.get());
}

/// ActOnResultNameDeclarator - Called from Parser::ParseFunctionDeclarator()
/// to introduce parameters into function prototype scope.
ResultNameDecl *Sema::ActOnResultNameDeclarator(ContractKind CK, Scope *S,
                                                QualType RetType,
                                                SourceLocation IDLoc,
                                                IdentifierInfo *II,
                                                unsigned FunctionScopeDepth) {
  // assert(S && S->isContractAssertScope() && "Invalid scope for result name");
  assert(II && "ResultName requires an identifier");

  bool IsInvalid = false;

  if (RetType->isVoidType()) {
    // Adjust the type of the result name to be int so we can actually produce a
    // node.
    RetType = Context.IntTy;
    Diag(IDLoc, diag::err_void_result_name) << II;
    IsInvalid = true;
  }

  // If needed, invent a fake placeholder type to represent the result name.
  // This is needed when we encounter a deduced return type on a non-template
  // function. We'll replace this once we've completed the function definition
  // (which must be attached to this declaration).
  bool HasInventedPlaceholderTypes =
      RetType->isUndeducedAutoType() && !RetType->isDependentType();
  if (HasInventedPlaceholderTypes)
    RetType = Context.getAutoType(DeducedKind::DeducedAsDependent, QualType(),
                                  AutoTypeKeyword::Auto);
  auto *New = ResultNameDecl::Create(Context, CurContext, IDLoc, II, RetType,
                                     nullptr, HasInventedPlaceholderTypes, FunctionScopeDepth);

  if (IsInvalid)
    New->setInvalidDecl();

  // Check for redeclaration of parameters, e.g. int foo(int x, int x);
  if (II) {
    LookupResult R(*this, II, IDLoc, LookupOrdinaryName,
                   RedeclarationKind::ForVisibleRedeclaration); // FIXME(EricWF)
    LookupName(R, S);
    if (!R.empty()) {
      NamedDecl *PrevDecl = *R.begin();
      if (R.isSingleResult() && PrevDecl->isTemplateParameter()) {
        // Maybe we will complain about the shadowed template parameter.
        // DiagnoseTemplateParameterShadow(D.getIdentifierLoc(), PrevDecl);
        // Just pretend that we didn't see the previous declaration.
        PrevDecl = nullptr;
      }
      // FIXME(EricWF): Diagnose lookup conflicts with lambda captures and
      // parameter declarations.
      if (auto *PVD = dyn_cast<ParmVarDecl>(PrevDecl)) {
        Diag(IDLoc, diag::err_result_name_shadows_param)
            << II; // FIXME(EricWF): Change the diagnostic here.
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

  assert(!S || S->isContractAssertScope());

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

  // p2900 [basic.contract.func]
  // A declaration E of a function f that is not a first declaration shall have
  // either no function contract-specifier-seq or the same
  // function-contract-specifier-seq as any first declaration D reachable from
  // E.
  const DifferenceKind DK = [&] {
    // Contracts may be omitted from following declarations.
    if (NewContracts.empty())
      return DK_None;

    // ... But if they exist, they must be present on the original declaration.
    if (OrigContracts.empty())
      return DK_OrigMissing;

    if (OrigContracts.size() != NewContracts.size())
      return DK_NumContracts;

    // ... And if they exist on the original declaration, they must be the same.
    for (; ContractIndex < OrigContracts.size(); ++ContractIndex) {
      auto *OC = OrigContracts[ContractIndex];
      auto *NC = NewContracts[ContractIndex];

      if (OC->getContractKind() != NC->getContractKind())
        return DK_Kind;
      if (OC->hasResultName() != NC->hasResultName())
        return DK_ResultName;
      if (!Context.hasSameExpr(OC->getCond(), NC->getCond()))
        return DK_Cond;
    }
    return DK_None;
  }();

  // Nothing to diagnose.
  if (DK == DK_None)
    return false;

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
  for (auto *CS : CSD->postconditions()) {
    Checker.TraverseContractStmt(CS);
    // FIXME(EricWF): DIagnose non-const function param types.
  }
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

  auto *Method = dyn_cast<CXXMethodDecl>(Instantiation);
  if (Method && !Method->isImplicitObjectMemberFunction())
    Method = nullptr;

  Sema::ContextRAII SavedContext(*this, Instantiation);
  // Lambdas within the contracts need an enclosing function scope.
  FunctionScopeRAII PopFunctionScope(*this);
  PushFunctionScope();
  Sema::CXXThisScopeRAII ThisScope(
      *this, Method ? Method->getParent() : nullptr,
      Method ? Method->getMethodQualifiers() : Qualifiers{}, Method != nullptr);

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
    Qualifiers MethodQuals = CXXMethod->getMethodQualifiers();
    if (LangOpts.ContractConstification)
      MethodQuals.addConst();
    ThisScope.emplace(*this, CXXMethod->getParent(),
                      MethodQuals,
                      /*IsLambda*/ false);
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

DeclResult Sema::RebuildContractsWithPlaceholderReturnType(FunctionDecl *FD) {

  ContractSpecifierDecl *CSD = FD->getContracts();
  assert(CSD && CSD->hasInventedPlaceholdersTypes() &&
         "Cannot rebuild contracts without placeholders");
  if (FD->isInvalidDecl()) {
    CSD->setInvalidDecl(true);
    return DeclResult(/*IsInvalid*/ true);
  }

  RebuildFunctionContracts Rebuilder(*this, true);

  QualType Replacement;
  auto CheckFunctionReturnType = [&](SourceLocation Loc) -> bool {
    if (!Replacement.isNull())
      return false;

    if (FD->getReturnType()->isUndeducedType()) {
      // If we fail to deduce the return type, the failure has already been
      // diagnosed.
      if (DeduceReturnType(FD, Loc, /*Diagnose=*/true) ||
          FD->getReturnType()->isUndeducedType())
        return true;
    }
    Replacement = FD->getReturnType();
    return false;
  };

  SmallVector<std::pair<ResultNameDecl *, ResultNameDecl *>> Transformed;

  for (auto *RND : FD->getContracts()->result_names()) {
    if (Replacement.isNull()) {
      if (CheckFunctionReturnType(RND->getLocation())) {
        FD->setInvalidDecl(true);
        return DeclResult(/*IsInvalid*/ true);
      }
    }
    assert(!Replacement.isNull());
    auto *NewRND =
        ActOnResultNameDeclarator(ContractKind::Post, nullptr, Replacement,
                                  RND->getLocation(), RND->getIdentifier(), RND->getFunctionScopeDepth());
    Transformed.emplace_back(RND, NewRND);
  }

  for (auto [K, V] : Transformed)
    Rebuilder.transformedLocalDecl(K, {V});

  SmallVector<ContractStmt *> NewContracts;
  bool IsInvalid = false;
  for (auto *CS : FD->contracts()) {
    StmtResult NewStmt = Rebuilder.TransformContractStmt(CS);
    if (NewStmt.isInvalid())
      IsInvalid = true;
    else
      NewContracts.push_back(NewStmt.getAs<ContractStmt>());
  }

  auto *NewCSD = BuildContractSpecifierDecl(
      NewContracts, FD, FD->getContracts()->getLocation(), IsInvalid);

  FD->setContracts(NewCSD);
  if (NewCSD->isInvalidDecl())
    FD->setInvalidDecl();

  return NewCSD;
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

// ActOnContractsOnFinishFunctionBody
//
// This function ensures there is a usable version of the
// function contracts attached to the definition declaration.
//
// This is already the case if the definition declaration was spelled with
// contracts.
//
// Otherwise, we might have contracts on the first declaration.
// If we do,
//
// (1) attach them to the defining declaration.
// (2) rebind any references to parameters contained within the contracts.
//
// This is important because the defining definitions parameters will
// be the ones evaluated by ExprConstant/CodeGen.
//
// This is probably BAD BAD NOT GOOD.
// But it works nicely.
void Sema::ActOnContractsOnFinishFunctionBody(FunctionDecl *Def) {
  assert(Def);
  // If we had a deduced return type on a non-template function, we can now
  // attempt to deduce the return type and rebuild the contracts with the
  // deduced return type.
  if (Def->hasContracts() &&
      Def->getContracts()->hasInventedPlaceholdersTypes()) {
    assert((Def->isThisDeclarationADefinition() ||
            Def->isTemplateInstantiation()) &&
           "Wrong declaration passed?");

    DeclResult Res = RebuildContractsWithPlaceholderReturnType(Def);
    if (Res.isInvalid()) {
      Def->setInvalidDecl(true);
      return;
    }
    auto *NewCSD = Res.getAs<ContractSpecifierDecl>();
    assert(NewCSD);
    NewCSD->setOwningFunction(Def);
    Def->setContracts(NewCSD);
    if (NewCSD->isInvalidDecl())
      Def->setInvalidDecl(true);
  }

  // [dcl.fct.def.coroutine]: A coroutine behaves as if the top-level
  // cv-qualifiers were removed from its parameters, so a postcondition of a
  // coroutine may not odr-use a non-reference parameter.
  if (Def->hasContracts() && getCurFunction() &&
      getCurFunction()->isCoroutine())
    diagnoseCoroutinePostconditionParams(*this, Def);
}

/// Return true if \p Var is declared within the predicate of the contract
/// assertion \p CSR (e.g. within a lambda-expression or statement-expression
/// contained in the predicate).
static bool isDeclaredWithinContract(const Sema &S,
                                     const Sema::ContractScopeRecord &CSR,
                                     const VarDecl *Var) {
  const DeclContext *VarDC = Var->getDeclContext();
  const DeclContext *ContractDC = CSR.ContextAtPush;
  if (!ContractDC->Encloses(VarDC))
    return false;

  // Declared in a lambda, block, or local class within the predicate.
  if (VarDC->getPrimaryContext() != ContractDC->getPrimaryContext())
    return true;

  // Declared in the same context as the contract assertion. It is declared
  // within the predicate only if it appears after the contract keyword (e.g.
  // in a statement-expression).
  if (isa<ParmVarDecl>(Var) || !Var->isLocalVarDecl())
    return false;
  return S.getSourceManager().isBeforeInTranslationUnit(CSR.KeywordLoc,
                                                        Var->getLocation());
}

/// Return true if a lambda-expression within the predicate of the contract
/// assertion \p CSR captures (or would implicitly capture) the local entity
/// \p VD by copy, in which case the id-expression names the corresponding
/// member of the closure type rather than the entity itself.
static bool isCapturedByCopyWithinContract(const Sema &S,
                                           const Sema::ContractScopeRecord &CSR,
                                           const ValueDecl *VD) {
  if (!CSR.FunctionScopeAtPush)
    return false;
  ArrayRef<FunctionScopeInfo *> Scopes = S.FunctionScopes;
  unsigned Start = CSR.FunctionIndex;
  if (Start >= Scopes.size() || Scopes[Start] != CSR.FunctionScopeAtPush)
    return false;

  for (unsigned I = Start + 1, E = Scopes.size(); I != E; ++I) {
    auto *LSI = dyn_cast<LambdaScopeInfo>(Scopes[I]);
    if (!LSI)
      continue;
    auto *Captured = const_cast<ValueDecl *>(VD);
    if (LSI->isCaptured(Captured)) {
      if (LSI->getCapture(Captured).isCopyCapture())
        return true;
      continue;
    }
    if (LSI->ImpCaptureStyle == CapturingScopeInfo::ImpCap_LambdaByval)
      return true;
  }
  return false;
}

/// [expr.prim.id.unqual], [expr.prim.id.qual]
///   If the [id-expression] appears in the predicate of a contract assertion C
///   and the entity is
///    - a variable declared outside of C of object type T,
///    - a variable declared outside of C of type "reference to T", or
///    - a structured binding of type T whose corresponding variable is
///      declared outside of C,
///   then the type of the expression is const T.
///
/// The same applies to a result binding ([dcl.contract.res]).
ContractConstification Sema::getContractConstification(const ValueDecl *VD) {
  if (!LangOpts.ContractConstification)
    return CC_None;

  assert(VD);
  const ContractScopeRecord *CSR = getActiveContractScope();
  if (!CSR)
    return CC_None;

  bool IsLocalEntity = false;
  if (isa<ResultNameDecl>(VD)) {
    IsLocalEntity = true;
  } else {
    // Find the variable that determines where the entity was declared.
    const VarDecl *Var = nullptr;
    if (const auto *Binding = dyn_cast<BindingDecl>(VD))
      Var = dyn_cast_or_null<VarDecl>(Binding->getDecomposedDecl());
    else
      Var = dyn_cast<VarDecl>(VD);
    if (!Var)
      return CC_None;

    if (isDeclaredWithinContract(*this, *CSR, Var))
      return CC_None;
    IsLocalEntity = Var->isLocalVarDeclOrParm();
  }

  // If a lambda within the predicate captures the entity by copy, the
  // id-expression names the member of the closure type instead.
  if (IsLocalEntity && isCapturedByCopyWithinContract(*this, *CSR, VD))
    return CC_None;

  if (isa<ResultNameDecl>(VD))
    return CC_ApplyConst;

  QualType T = VD->getType();
  if (T.isNull())
    return CC_None;
  if (T->isObjectType() ||
      (T->isReferenceType() && T.getNonReferenceType()->isObjectType()))
    return CC_ApplyConst;

  return CC_None;
}

static const DeclContext* walkUpDeclContextToFunction(const DeclContext *DC, bool AllowLambda = false) {
  while (true) {
    assert(DC);
    if (isa<BlockDecl>(DC) || isa<EnumDecl>(DC) || isa<CapturedDecl>(DC) ||
        isa<RequiresExprBodyDecl>(DC) || isa<LinkageSpecDecl>(DC) ||
        (isa<CXXRecordDecl>(DC) && cast<CXXRecordDecl>(DC)->isLambda())) {
      DC = DC->getParent();
    } else if (!AllowLambda && isa<CXXMethodDecl>(DC) &&
               cast<CXXMethodDecl>(DC)->getOverloadedOperator() == OO_Call &&
               cast<CXXRecordDecl>(DC->getParent())->isLambda()) {
      DC = DC->getParent()->getParent();
    } else
      break;
  }
  if (DC && !DC->isFunctionOrMethod())
    return nullptr;
  return DC;
}

QualType Sema::adjustCXXThisTypeForContracts(QualType QT) {
  if (!LangOpts.ContractConstification)
    return QT;
  const ContractScopeRecord *CSR = getActiveContractScope();
  if (!CSR)
    return QT;

  // 'this' is constified any time the `this` object that is captured by a lambda which exists fully
  // within a contract.
  //
  // We need to ensure that we haven't entered a nested member function context, because in that case we
  // don't want to constify the `this` object.
  // For example:
  // ```
  // struct A {
  //   void f() {
  //     [&]() {
  //        contract_assert([&] {
  //           struct B { int x; void f() { ++x; } };
  //            return true;
  //          }());
  //      }();
  //   }};
  // ```
  const DeclContext *ContractContext =
      walkUpDeclContextToFunction(CSR->ContextAtPush);
  if (!ContractContext)
    return QT;
  const DeclContext *QTContext = walkUpDeclContextToFunction(CurContext );
  if (!QTContext)
    return QT;
  if (!ContractContext->Equals(QTContext))
    return QT;

  return Context.getPointerType(QT->getPointeeType().withConst());
}

namespace {

struct CaptureUsage {
  clang::LambdaCapture Capture;
  const ContractStmt *UsedInContract = nullptr;
  const Expr *UsageExpr = nullptr;
  SourceLocation UsageLoc = SourceLocation();

  CaptureUsage(clang::LambdaCapture Capture) : Capture(Capture) {}
};

/// TODO(EricWF): Remove this and do it inline instead. We currently
/// do this as a RecursiveASTVisitor because the changes to do it during
/// the initial parsing pass are too invasive to do dur
struct LambdaCaptureChecker : RecursiveASTVisitor<LambdaCaptureChecker> {
  typedef RecursiveASTVisitor<LambdaCaptureChecker> super;

  Sema &Actions;
  const LambdaExpr *const CurLambda = nullptr;
  const ContractStmt *CurContract = nullptr;


  // Note: The value `nullptr` is used to denote a capture of CXXThis.
  DenseMap<const ValueDecl *, CaptureUsage> Captures;


  void observeUsage(const ValueDecl *VD, const Expr *E, SourceLocation Loc) {
    if (auto Pos = Captures.find(VD); Pos != Captures.end()) {
      if (!CurContract)
        Captures.erase(VD);
      else {
        auto& Usage = Pos->second;
        if (Usage.UsageExpr == nullptr || (isa<LambdaExpr>(Usage.UsageExpr) && !isa<LambdaExpr>(E))) {
          Usage.UsedInContract = CurContract;
          Usage.UsageExpr = E;
          Usage.UsageLoc = Loc;
        }
      }
    }
  }

private:
  LambdaCaptureChecker(Sema &S, LambdaExpr *LE) : Actions(S), CurLambda(LE) { Init(); }


  void Run() {
    // Traverse the lambdas function-level contracts and body to find the bad captures.
    FunctionDecl *FD = CurLambda->getCallOperator();
    assert(FD->getBody());
    if (FD->hasContracts())
      TraverseDecl(FD->getContracts());
    TraverseStmt(CurLambda->getBody());

    // Finally, diagnose any captures that still remain, since they do not have any non-contrac
    // usages.
    for (auto& [Var, Bad] : Captures) {
      // We likely didn't see the usage because there was a intervening lambda that captured by copy.
      if (Bad.UsedInContract == nullptr)
        continue;
      Actions.Diag(CurLambda->getCaptureDefaultLoc(),
                   diag::err_lambda_implicit_capture_in_contracts_only)
          << (int)Bad.Capture.capturesThis() << cast_or_null<NamedDecl>(Var);
      SourceLocation UsageLoc = Bad.UsageLoc;
      Actions.Diag(UsageLoc, diag::note_lambda_implicit_capture_in_contract_usage)
            << (int)Bad.Capture.capturesThis() << cast_or_null<NamedDecl>(Var);
      if (Bad.UsedInContract)
        Actions.Diag(Bad.UsedInContract->getBeginLoc(),
                    diag::note_contract_context);

    }
  }

  void Init() {
    // Collect all of the implicit captures of the lambda.
    // If the lambda capture hasn't been removed after traversing the tree then
    // that lambda capture is bad, and must be diagnosed as only being used inside
    // of a contract.
    for (auto C : CurLambda->captures()) {
      if (C.capturesThis() && C.isImplicit())
        Captures.insert({nullptr, {C}});
      if (!C.capturesVariable())
        continue;
      if (C.isExplicit())
        continue;
      Captures.insert({C.getCapturedVar(), {C}});
    }
  }

public:
  bool shouldVisitLambdaBody() const { return false; }

  bool TraverseContractStmt(ContractStmt *CS) {
    assert(CS->getCond());
    const ContractStmt *Prev = CurContract;
    CurContract = CS;
    TraverseStmt(CS->getCond());
    CurContract = Prev;
    return true;
  }

  bool TraverseLambdaCapture(LambdaExpr *LE, const LambdaCapture *C,
                             Expr *Init) {
    return true;
  }

  bool TraverseLambdaExpr(LambdaExpr *LE) {
    assert(LE != CurLambda && "Revisiting the root lambda?");
    // Iterate over the captures of the nested lambda, and mark any of our captures as having been seen
    // outside of a contract. This assumes that the inner lambda has a valid usage of the capture.
    // If it doesn't, we'll diagnose that separately.
    for (auto C : LE->captures()) {
      // FIXME(EricWF): Figure out how to deal with VLA captures here
      if (C.capturesVLAType())
        continue;

      assert(C.capturesThis() || C.capturesVariable());
      observeUsage(C.capturesThis() ? nullptr : C.getCapturedVar(), LE, C.getLocation());
    }
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    if (auto *VD = dyn_cast<ValueDecl>(E->getDecl()); VD && E->isNonOdrUse() != NOUR_Unevaluated)
      observeUsage(VD, E, E->getExprLoc());
    return true;
  }

  bool VisitMemberExpr(MemberExpr *E) {
    if (E->isNonOdrUse() != NOUR_Unevaluated)
      observeUsage(nullptr, E, E->getExprLoc());
    return true;
  }

  bool VisitCXXThisExpr(CXXThisExpr *E) {
    observeUsage(nullptr, E, E->getExprLoc());
    return true;
  }

public:
  static void Check(Sema &Actions, LambdaExpr *LE) {
    if (!Actions.LangOpts.ContractLambdaCaptureRestrictions)
      return;
    LambdaCaptureChecker Checker(Actions, LE);
    Checker.Run();
  }
};

} // namespace

void Sema::CheckLambdaCapturesForContracts(LambdaExpr *LE) {
  // Check the contracts on the function declaration.
  LambdaCaptureChecker::Check(*this, LE);
}

bool Sema::isContractAssertionContext() const {
  auto *CR = getCurrentContractEntry();
  if (!CR)
    return false;

  return CR->ContextAtPush->Equals(CurContext);

#if 0
  return getCurrentContractEntry() && (getCurrentContractEntry()->FunctionScopeAtPush == getCurFunction() ||
      (getCurrentContractEntry()));
  if (FunctionScopes.empty()) {
    auto *CR = getCurrentContractEntry();
    return CR != nullptr;
    return ExprEvalContexts.back().isContractAssertionContext() ||
           (getCurrentContractEntry() &&
            getCurrentContractEntry()->HadNoFunctionScope);

  }
  return FunctionScopes.back()->InContract;
#endif
}

ArrayRef<Sema::ContractScopeRecord> Sema::getAllContractScopes() const {
  return llvm::ArrayRef(ContractScopeStack.begin(), ContractScopeStack.end());
}
ArrayRef<Sema::ContractScopeRecord> Sema::getContractScopes() const {
  return getAllContractScopes();
#if 0
  unsigned Offset = 0;
  for (auto Pos = ContractScopeStack.begin(); Pos != ContractScopeStack.end();
       ++Pos) {
    if (Pos->FunctionScopeStartAtPush == FunctionScopesStart)
      break;
    ++Offset;
  }
  return llvm::ArrayRef(ContractScopeStack.begin() + Offset,
                        ContractScopeStack.end());
#endif
}

const DeclContext *
Sema::ContractScopeRecord::getFunctionContext(bool AllowLambda) const {
  if (!ContextAtPush->isFunctionOrMethod() ||
      (isLambdaCallOperator(ContextAtPush) && !AllowLambda))
    return walkUpDeclContextToFunction(ContextAtPush, AllowLambda);
  return ContextAtPush;
}

const Sema::ContractScopeRecord *Sema::getCurrentContractEntry() const {
  auto EntryList = getContractScopes();
  if (EntryList.empty())
    return nullptr;
  return &EntryList.back();
}

Sema::ContractScopeRAII::ContractScopeRAII(Sema &S, ContractKind CK, ContractScopeOffset ScopeOffset, SourceLocation Loc)
    : S(S) {
  S.PushContractScope(CK, ScopeOffset, Loc);
}

Sema::ContractScopeRAII::~ContractScopeRAII() {
  S.PopContractScope();
}


void Sema::PushContractScope(ContractKind Kind, ContractScopeOffset ScopeOffset, SourceLocation Loc) {
//  assert(!FunctionScopes.empty());

  ContractScopeRecord Record{
         .Index = static_cast<unsigned>(ContractScopeStack.size()),
         .Kind = Kind,
         .ScopeOffset = ScopeOffset,
         .KeywordLoc = Loc,
         .ContextAtPush = CurContext,
         .PreviousCXXThisType = CXXThisTypeOverride,
         .FunctionIndex = static_cast<unsigned>(
             FunctionScopes.empty() ? 0ul : FunctionScopes.size() - 1),
         .StartFunctionIndex = FunctionScopesStart,
         .FunctionScopeAtPush = getCurFunction(),
         .AddedConstToCXXThis = false,
         .WasInContractContext = ExprEvalContexts.back().InContractAssertion,
         .HadNoFunctionScope = FunctionScopes.empty(),
         .FunctionScopeStartAtPush = FunctionScopesStart};

    // Setup the constification context when building declref expressions.
    ExprEvalContexts.back().InContractAssertion = true;
  assert(CurContext);

    // P2900R8 [expr.prim.this]p2
    //   If the expression 'this' appears ... in a contract assertion
    //     (including as the result of the implicit transformation in the body of
    //     a non-static member function and including in the bodies of nested
    //     lambda-expressions),
    // ...
    //  const is combined with the cv-qualifier-seq used to generate the resulting
    //  type (see below
    if (!CXXThisTypeOverride.isNull()) {
      assert(CXXThisTypeOverride->isPointerType());
      QualType ClassType = CXXThisTypeOverride->getPointeeType();
      if ((not ClassType.isConstQualified()) && LangOpts.ContractConstification) {
        // If the 'this' object is const-qualified, we need to remove the
        // const-qualification for the contract check.
        ClassType.addConst();
        Record.AddedConstToCXXThis = true;
        CXXThisTypeOverride = Context.getPointerType(ClassType);
      }
    }

    //assert(!S.FunctionScopes.empty());


  // Contract assertions may be nested within the same function scope (e.g. a
  // contract_assert inside a statement-expression in a predicate), so
  // remember the enclosing one so it can be restored.
  if (Record.FunctionScopeAtPush) {
    Record.PreviousFunctionScopeContractIndex =
        Record.FunctionScopeAtPush->ContractScopeIndex;
    Record.FunctionScopeAtPush->ContractScopeIndex = Record.Index;
  }

    ContractScopeStack.push_back(Record);
}

ContractScopeRecord Sema::PopContractScope() {
  assert(!ContractScopeStack.empty());

  auto Record = ContractScopeStack.back();
  ContractScopeStack.pop_back();



  assert(ExprEvalContexts.back().InContractAssertion == true);
  ExprEvalContexts.back().InContractAssertion = Record.WasInContractContext;
  CXXThisTypeOverride = Record.PreviousCXXThisType;


  if (Record.FunctionScopeAtPush && !FunctionScopes.empty() &&
      FunctionScopes.back() == Record.FunctionScopeAtPush) {
    assert(FunctionScopes.back()->ContractScopeIndex == Record.Index);
    FunctionScopes.back()->ContractScopeIndex =
        Record.PreviousFunctionScopeContractIndex;
  }

  return Record;
}

const ContractScopeRecord *Sema::getActiveContractScope() const {
  // A contract assertion is active if the current context is (lexically)
  // within its predicate: either the context in which the contract was
  // pushed, or a lambda, block, or local class nested within it which is
  // defined within the predicate. The latter excludes, for example, a generic
  // lambda defined before the contract assertion whose call operator is
  // instantiated while parsing the predicate.
  for (const ContractScopeRecord &CSR : llvm::reverse(ContractScopeStack)) {
    const DeclContext *ContractDC = CSR.ContextAtPush;
    if (!ContractDC->Encloses(CurContext))
      continue;
    if (ContractDC->Equals(CurContext))
      return &CSR;

    // Find the outermost context nested within the contract's context.
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

SourceLocation Sema::getContractLocForFunctionScope(const sema::FunctionScopeInfo *FSI) const {
  assert(FSI->isInContract());
  assert(FSI->ContractScopeIndex < ContractScopeStack.size());
  return ContractScopeStack[FSI->ContractScopeIndex].KeywordLoc;
}
