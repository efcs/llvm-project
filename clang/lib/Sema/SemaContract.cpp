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
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"

using namespace clang;
using namespace sema;
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
        Replacement, TL.getTypePtr()->getKeyword(), Replacement.isNull(), false,
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
  //assert(getCurScope()->isContractAssertScope() && "Incorrect scope for contract assert");
  if (Cond->isTypeDependent())
    return Cond;

  ConditionResult Res = ActOnCondition(getCurScope(), Cond->getExprLoc(), Cond, Sema::ConditionKind::Boolean, /*MissingOK=*/false);
  if (Res.isInvalid())
    return ExprError();
  Cond = Res.get().second;
  assert(Cond);
  if (auto KnownValue = Res.getKnownValue(); KnownValue.has_value()) {
    Diag(Cond->getExprLoc(), diag::warn_ericwf_fixme) <<
      (std::string("Condition always evaluates to ") + (*KnownValue ? "true" : "false")) << Cond;
  }
  return Cond;
}

StmtResult Sema::BuildContractStmt(ContractKind CK, SourceLocation KeywordLoc,
                                   Expr *Cond, DeclStmt *RND,
                                   ArrayRef<const Attr *> Attrs) {

  return ContractStmt::Create(Context, CK, KeywordLoc, Cond, RND, Attrs);
}

StmtResult Sema::ActOnContractAssert(ContractKind CK, SourceLocation KeywordLoc,
                                     Expr *Cond, ResultNameDecl *RND,
                                     ParsedAttributes &CXX11Contracts) {

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
                            *this, CXX11Contracts, KeywordLoc));

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
                                                IdentifierInfo *II) {
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

  if (RetType->isUndeducedAutoType() && !RetType->isDependentType())
    RetType = Context.getAutoType(QualType(), AutoTypeKeyword::Auto, true,
                                  false, nullptr, {});

  auto *New = ResultNameDecl::Create(Context, CurContext, IDLoc, II, RetType);
  if (IsInvalid)
    New->isInvalidDecl();

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

bool Sema::CheckEquivalentContractSequence(FunctionDecl *OrigDecl,
                                           FunctionDecl *NewDecl) {

  ContractSpecifierDecl *OrigContractSpec = OrigDecl->getContracts();
  ContractSpecifierDecl *NewContractSpec = NewDecl->getContracts();
  ArrayRef<const ContractStmt *> OrigContracts, NewContracts;
  if (OrigContractSpec)
    OrigContracts = OrigContractSpec->contracts();
  if (NewContractSpec)
    NewContracts = NewContractSpec->contracts();

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
  bool DidDiagnose = false;

  ContractStmt *CurrentContract = nullptr;
  ContractSpecifierDecl *CSD = nullptr;

public:
  ParamReferenceChecker(Sema &S, const FunctionDecl *FD)
      : Actions(S), FD(FD), CSD(FD->getContracts()) {
    assert(CSD);
  }

  bool TraverseContractStmt(ContractStmt *CS) {
    bool SetCurrent = CurrentContract == nullptr;
    if (SetCurrent)
      CurrentContract = CS;
    if (CS->getCond())
      TraverseStmt(CS->getCond());
    if (SetCurrent)
      CurrentContract = nullptr;
    return true;
  }

  bool isRelevantParmVar(const ParmVarDecl *PVD) const {
    if (PVD->getFunctionScopeIndex() < FD->getNumParams())
      return FD->getParamDecl(PVD->getFunctionScopeIndex()) == PVD;
    return false;
  }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    assert(CurrentContract && "No current contract to use in diagnostics?");

    enum DiagSelector {
      DS_None = -1,
      DS_Array = 0,
      DS_Function = 1,
      DS_NotConst = 2
    };
    // [dcl.contract.func] p2900r8 --
    //   If a  postcondition  odr-uses ([basic.def.odr]) a non-reference
    //   parameter, ... that parameter shall be declared const and shall not
    //   have array or function type. [ Note: This requirement applies even to
    //   declarations that do not specify the postcondition-specifier.]
    [&]() {
      auto *PVD = dyn_cast_or_null<ParmVarDecl>(E->getDecl());
      if (!PVD || DiagnosedDecls.count(PVD) || !isRelevantParmVar(PVD))
        return;

      QualType PVDType = PVD->getOriginalType();
      if (PVDType->isReferenceType())
        return;

      DiagSelector DiagSelect = [&]() {
        if (PVDType->isArrayType() || PVDType->isArrayParameterType())
          return DS_Array;
        assert(!PVD->getOriginalType()->isArrayType());
        if (PVDType->isFunctionPointerType())
          return DS_Function;
        if (!PVDType.isConstQualified())
          return DS_NotConst;
        return DS_None;
      }();

      if (DiagSelect != DS_None) {
        CSD->setInvalidDecl(true);
        DiagnosedDecls.insert(PVD);
        DidDiagnose = true;
        Actions.Diag(E->getLocation(),
                     diag::err_contract_postcondition_parameter_type_invalid)
            << PVD->getIdentifier() << DiagSelect
            << CurrentContract->getSourceRange();

        Actions.Diag(PVD->getTypeSpecStartLoc(), diag::note_parameter_type)
            << PVD->getOriginalType() << PVD->getSourceRange();
      }
    }();

    return true;
  }

private:
  DenseSet<ParmVarDecl *> DiagnosedDecls;
};

} // namespace

void Sema::InstantiateContractSpecifier(
    SourceLocation PointOfInstantiation, FunctionDecl *Instantiation,
    const FunctionDecl *Pattern,
    const MultiLevelTemplateArgumentList &TemplateArgs) {
  llvm::errs() << "Instantiation: \n";
  Instantiation->dumpColor();
  llvm::errs() << "Pattern: \n";
  Pattern->dumpColor();
  ContractSpecifierDecl *PatternCSD = Pattern->getContracts();
  if (!PatternCSD)
    return;
  bool IsInvalid = false;

  LocalInstantiationScope Scope(*this, true);
  // if (addInstantiatedResultNamesToScope(Instantiation, Pattern, Scope,
  // TemplateArgs)) {
  //   Instantiation->setInvalidDecl(true);
  //   return;
  // }

  SmallVector<ContractStmt *> NewContracts;
  for (auto *CS : PatternCSD->contracts()) {
    if (CS->hasResultName()) {
    }
    StmtResult NewStmt = SubstStmt(CS, TemplateArgs);
    if (NewStmt.isInvalid())
      IsInvalid = true;
    else
      NewContracts.push_back(NewStmt.getAs<ContractStmt>());
  }

  ContractSpecifierDecl *NewCSD = ActOnFinishContractSpecifierSequence(
      NewContracts, PatternCSD->getLocation(), IsInvalid);
  NewCSD->setDeclContext(Instantiation);
  assert(NewCSD);

  if (NewCSD->isInvalidDecl())
    Instantiation->setInvalidDecl(true);
  Instantiation->setContracts(NewCSD);
}

ContractSpecifierDecl *
Sema::BuildContractSpecifierDecl(ArrayRef<ContractStmt *> Contracts,
                                 SourceLocation Loc, bool IsInvalid) {
  ContractSpecifierDecl *CSD = ContractSpecifierDecl::Create(
      Context, CurContext, Loc, Contracts, IsInvalid);
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

  ContractSpecifierDecl *CSD = ContractSpecifierDecl::Create(
      Context, CurContext, Loc, Contracts, IsInvalid);

  ResultNameDecl *CanonicalResult = nullptr;
  for (auto *RD : CSD->result_names()) {
    if (CanonicalResult)
      RD->setCanonicalResultNameDecl(CanonicalResult);
    else
      CanonicalResult = RD;
  }

  if (IsInvalid)
    CSD->setInvalidDecl(true);

  return CSD;
}

static void diagnoseParamTypes(Sema &S, FunctionDecl *FD,
                               ContractSpecifierDecl *CSD) {

  // Check for post-conditions that reference non-const parameters.
  ParamReferenceChecker Checker(S, FD);
  for (auto *CS : CSD->postconditions()) {
    Checker.TraverseContractStmt(CS);
    // FIXME(EricWF): DIagnose non-const function param types.
  }
  if (Checker.DidDiagnose)
    FD->setInvalidDecl(true);
}

static bool functionHasContracts(FunctionDecl *FD) {
  return FD->hasContracts() || FD->getFirstDecl()->hasContracts();
}

void Sema::CheckFunctionContractSpecifier(FunctionDecl *FD) {
  if (!functionHasContracts(FD))
    return;

#if 0
  // Find the declaration we want to diagnose contract mismatches against.
  // If the contracts are only present the first declaration, use that one.
  // Othewise, use the most recent declaration.
  auto DeclForDiagnosis = [&]() {
    FunctionDecl *PrevDecl = FD->getFirstDecl();
    if (PrevDecl && PrevDecl != FD && PrevDecl->hasContracts()) {
      if (FD->getPreviousDecl()->hasContracts())
        PrevDecl = FD->getPreviousDecl();
    }
    return PrevDecl == FD ? nullptr : PrevDecl;
  }();

  if (DeclForDiagnosis && CheckEquivalentContractSequence(DeclForDiagnosis, FD))
    FD->setInvalidDecl(true);
#endif

  ContractSpecifierDecl *CSD = FD->getContracts();
  if (!CSD)
    return;
}
void Sema::ActOnContractsOnStartOfFunctionDef(Scope *S, FunctionDecl *FD) {}

void Sema::ActOnContractsOnFinishFunctionDecl(FunctionDecl *D,
                                              bool IsDefinition) {
  if (!D->hasContracts() && !IsDefinition)
    return;

  FunctionDecl *FD;
  if (FunctionTemplateDecl *FunTmpl = dyn_cast<FunctionTemplateDecl>(D))
    FD = FunTmpl->getTemplatedDecl();
  else
    FD = cast<FunctionDecl>(D);

  auto *First = FD->getFirstDecl();
  if (!First->hasContracts())
    return;

  // If the definition has omitted the contracts, but the first declaration has
  // them, we need to rebuild the contracts to refer to the parameters of the
  // definition.
  if (First->hasContracts() && !FD->hasContracts() && IsDefinition &&
      !FD->isTemplateInstantiation()) {
    // Note: This case is mutually exclusive with the NonDependentPlaceholders
    // case, since we can't have a placeholder return type on a declaration that
    // isn't a definition.
    ContractSpecifierDecl *NewCSD = RebuildContractSpecifierForDecl(First, FD);
    if (NewCSD->isInvalidDecl())
      FD->setInvalidDecl(true);
    NewCSD->setDeclContext(FD);
    FD->setContracts(NewCSD);
  }
  assert(FD->hasContracts());

  if (FD->hasContracts()) {
    assert(FD->getContracts()->getDeclContext());
    // FIXME(EricWF): Remove this
    FD->getContracts()->getDeclContext()->dumpAsDecl();

    FD->getContracts()->setDeclContext(FD);
    assert(isa<FunctionDecl>(FD->getContracts()->getDeclContext()));
    assert(dyn_cast<FunctionDecl>(FD->getContracts()->getDeclContext()) == FD);
    diagnoseParamTypes(*this, FD, FD->getContracts());
  }
  ContractSpecifierDecl *CSD = FD->getContracts();

  if (FD->isThisDeclarationADefinition() != IsDefinition) {
    llvm::errs() << "FunctionDecl: " << FD->getName() << "\n";
    llvm::errs() << "IsDefinition: " << IsDefinition << "\n";
    llvm::errs() << "isThisDeclarationADefinition: "
                 << FD->isThisDeclarationADefinition() << "\n";
    llvm::errs() << "Orign Decl: \n";
    D->dumpColor();
    if (FD != D) {
      llvm::errs() << "Extracted Decl: \n";
      FD->dumpColor();
    }
    llvm::errs() << "First Decl: \n";
    First->dumpColor();
  }
  // When the declared return type of a non-templated function contains a
  // placeholder type, a postcondition-specifier with a result-name-introducer
  // shall be present only on a definition.
  if (CSD->hasCanonicalResultName() &&
      FD->getReturnType()->isUndeducedAutoType() && !FD->isTemplated()) {
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
  CheckFunctionContractSpecifier(FD);
}

namespace {

/// DiagnoseDeclRefVisitor - A terrible hack to re-bind (NOT rebuild) the
/// ParmVarDecl references in a contract to the new ParmVarDecls.
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
/// a the definition. This rebinds parameters and result names as needed.
ContractSpecifierDecl *
Sema::RebuildContractSpecifierForDecl(FunctionDecl *First, FunctionDecl *Def) {

  assert(Def->getContracts() == First->getContracts() || !Def->hasContracts());
  Def->setContracts(nullptr);

  RebuildFunctionContracts Rebuilder(*this, true);
  for (unsigned I = 0; I < First->getNumParams(); ++I) {
    Rebuilder.transformedLocalDecl(First->getParamDecl(I),
                                   Def->getParamDecl(I));
  }
  for (auto *RND : First->getContracts()->result_names()) {
    QualType Replacement = Def->getReturnType();
    auto *NewRND =
        ActOnResultNameDeclarator(ContractKind::Post, nullptr, Replacement,
                                  RND->getLocation(), RND->getIdentifier());
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

  auto *CSD = ActOnFinishContractSpecifierSequence(
      NewContracts, Def->getContracts()->getLocation(), IsInvalid);
  CSD->setDeclContext(Def);

  Def->setContracts(CSD);

  if (CSD->isInvalidDecl())
    Def->isInvalidDecl();
  return CSD;

  Def->setContracts(First->getContracts());
}

///
DeclResult
Sema::RebuildContractsWithPlaceholderReturnType(ContractSpecifierDecl *CSD) {
  assert(CSD->getDeclContext());
  FunctionDecl *FD = dyn_cast<FunctionDecl>(CSD->getDeclContext());
  assert(FD && "Expected a function declaration context");
  assert(CSD->hasNondependentPlaceholders() &&
         "Cannot rebuild contracts without placeholders");
  FD->dumpColor();

  RebuildFunctionContracts Rebuilder(*this, true);

  QualType Replacement;
  auto CheckFunctionReturnType = [&](SourceLocation Loc) -> bool {
    if (!Replacement.isNull())
      return false;

    if (FD->getReturnType()->isUndeducedType()) {
      if (DeduceReturnType(FD, Loc, true)) {
        Diag(CSD->getLocation(), diag::err_ericwf_unimplemented)
            << "IDK what's wrong";
        return true;
      }
    }
    assert(!FD->getReturnType()->isUndeducedType());
    Replacement = FD->getReturnType();
    return false;
  };

  SmallVector<std::pair<ResultNameDecl *, ResultNameDecl *>> Transformed;

  for (auto *RND : FD->getContracts()->result_names()) {
    if (Replacement.isNull()) {
      if (CheckFunctionReturnType(RND->getLocation())) {
        FD->setInvalidDecl(true);
        return (Decl *)nullptr;
      }
    }
    assert(!Replacement.isNull());
    auto *NewRND =
        ActOnResultNameDeclarator(ContractKind::Post, nullptr, Replacement,
                                  RND->getLocation(), RND->getIdentifier());
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

  auto *NewCSD = ActOnFinishContractSpecifierSequence(
      NewContracts, FD->getContracts()->getLocation(), IsInvalid);
  NewCSD->setDeclContext(FD);
  FD->setContracts(NewCSD);
  if (NewCSD->isInvalidDecl())
    FD->isInvalidDecl();

  return NewCSD;
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
  assert(Def->isThisDeclarationADefinition() && "Wrong declaration passed?");

  // If we had a deduced return type on a non-template function, we can now
  // attempt to deduce the return type and rebuild the contracts with the
  // deduced return type.
  if (Def->getContracts()->hasNondependentPlaceholders() ||
      (Def->getContracts() == Def->getFirstDecl()->getContracts() &&
       Def != Def->getFirstDecl())) {
    DeclResult Res =
        RebuildContractsWithPlaceholderReturnType(Def->getContracts());
    if (Res.isInvalid()) {
      Def->setInvalidDecl(true);
      return;
    }
    auto *NewCSD = Res.getAs<ContractSpecifierDecl>();
    NewCSD->setDeclContext(Def);
    Def->setContracts(NewCSD);
    if (NewCSD->isInvalidDecl())
      Def->setInvalidDecl(true);
  }

#if 0
  QualType Replacement;
  auto CheckFunctionReturnType = [&](SourceLocation Loc) -> bool {
    if (!Replacement.isNull())
      return false;

    if (Def->getReturnType()->isUndeducedType()) {
      if (DeduceReturnType(Def, Loc, true))
        return true;
    }
    assert(!Def->getReturnType()->isUndeducedType());
    Replacement = Def->getReturnType();
    return false;
  };



  if (!Transformed.empty()) {
    bool IsInvalid = false;
    SmallVector<ContractStmt*> NewContracts;

    auto Transformer = RebuildAutoResultName(*this, Replacement);
    for (auto [K, V] : Transformed)
      Transformer.transformedLocalDecl(K, {V});

    for (auto *CS : Def->contracts()) {
      if (!CS->hasResultName()) {
        NewContracts.push_back(CS);
      } else {
        StmtResult NewStmt = Transformer.TransformContractStmt(CS);
        if (NewStmt.isInvalid())
          IsInvalid = true;
        else
          NewContracts.push_back(NewStmt.getAs<ContractStmt>());
      }
    }

    ContractSpecifierDecl *Specifier = ActOnFinishContractSpecifierSequence(NewContracts, Def->getContracts()->getLocation(), IsInvalid);
    Def->setContracts(Specifier);
  }
#endif
}

void Sema::ActOnContractsOnMergeFunctionDecl(FunctionDecl *OrigDecl,
                                             FunctionDecl *NewDecl) {
  auto *OrigFD = dyn_cast<FunctionDecl>(OrigDecl);
  auto *NewFD = dyn_cast<FunctionDecl>(NewDecl);
  if (!OrigFD && !NewFD)
    return;
  if (OrigFD && NewFD) {
    CheckEquivalentContractSequence(OrigFD, NewFD);
    return;
  }
  assert(false);
}

Sema::ContractScopeRAII::ContractScopeRAII(Sema &S, bool OverrideThis)
    : S(S), OldValue(S.ExprEvalContexts.back().InContractAssertion),
      OldCXXThisType(S.CXXThisTypeOverride), OldScope(S.getCurScope()),
      OldInContractScope(OldScope ? OldScope->isContractScope() : false) {
  S.ExprEvalContexts.back().InContractAssertion = true;
  if (!S.CXXThisTypeOverride.isNull()) {
    assert(S.CXXThisTypeOverride->isPointerType());
    QualType ClassType = S.CXXThisTypeOverride->getPointeeType();
    if (not ClassType.isConstQualified()) {
      // If the 'this' object is const-qualified, we need to remove the
      // const-qualification for the contract check.
      ClassType.addConst();
      S.CXXThisTypeOverride = S.Context.getPointerType(ClassType);
    }
  }
  if (OldScope) {
    OldScope->setIsContractScope(true);
  }
}

Sema::ContractScopeRAII::~ContractScopeRAII() {
  assert(S.ExprEvalContexts.back().InContractAssertion == true);
  S.ExprEvalContexts.back().InContractAssertion = OldValue;
  S.CXXThisTypeOverride = OldCXXThisType;
  if (OldScope) {
    OldScope->setIsContractScope(OldInContractScope);
  }
}

#if 0
void Sema::InstantiateContractSpecifier(SourceLocation PointOfInstantiation,
                                    FunctionDecl *Decl) {
  const FunctionProtoType *Proto = Decl->getType()->castAs<FunctionProtoType>();
  if (Proto->getExceptionSpecType() != EST_Uninstantiated)
    return;

  InstantiatingTemplate Inst(*this, PointOfInstantiation, Decl,
                             InstantiatingTemplate::ExceptionSpecification());
  if (Inst.isInvalid()) {
    // We hit the instantiation depth limit. Clear the exception specification
    // so that our callers don't have to cope with EST_Uninstantiated.
    UpdateExceptionSpec(Decl, EST_None);
    return;
  }
  if (Inst.isAlreadyInstantiating()) {
    // This exception specification indirectly depends on itself. Reject.
    // FIXME: Corresponding rule in the standard?
    Diag(PointOfInstantiation, diag::err_exception_spec_cycle) << Decl;
    UpdateExceptionSpec(Decl, EST_None);
    return;
  }

  // Enter the scope of this instantiation. We don't use
  // PushDeclContext because we don't have a scope.
  Sema::ContextRAII savedContext(*this, Decl);
  LocalInstantiationScope Scope(*this);

  MultiLevelTemplateArgumentList TemplateArgs =
      getTemplateInstantiationArgs(Decl, Decl->getLexicalDeclContext(),
          /*Final=*/false, /*Innermost=*/std::nullopt,
          /*RelativeToPrimary*/ true);

  // FIXME: We can't use getTemplateInstantiationPattern(false) in general
  // here, because for a non-defining friend declaration in a class template,
  // we don't store enough information to map back to the friend declaration in
  // the template.
  FunctionDecl *Template = Proto->getExceptionSpecTemplate();
  if (addInstantiatedParametersToScope(Decl, Template, Scope, TemplateArgs)) {
    UpdateExceptionSpec(Decl, EST_None);
    return;
  }

  // The noexcept specification could reference any lambda captures. Ensure
  // those are added to the LocalInstantiationScope.
  LambdaScopeForCallOperatorInstantiationRAII PushLambdaCaptures(
      *this, Decl, TemplateArgs, Scope,
      /*ShouldAddDeclsFromParentScope=*/false);

  SubstExceptionSpec(Decl, Template->getType()->castAs<FunctionProtoType>(),
                     TemplateArgs);
}
#endif