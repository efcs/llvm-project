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
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/STLForwardCompat.h"
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
                                   Expr *Cond, DeclStmt *ResultNameDecl,
                                   ArrayRef<const Attr *> Attrs) {
  assert((CK == ContractKind::Post || ResultNameDecl == nullptr) &&
         "ResultNameDecl only allowed for postconditions");


  return ContractStmt::Create(Context, CK, KeywordLoc, Cond, ResultNameDecl,
                              Attrs);
}

StmtResult Sema::ActOnContractAssert(ContractKind CK, SourceLocation KeywordLoc,
                                     Expr *Cond, DeclStmt *ResultNameDecl,
                                     ParsedAttributes &CXX11Contracts) {

  StmtResult Res =
      BuildContractStmt(CK, KeywordLoc, Cond, ResultNameDecl,
                        SemaContractHelper::buildAttributesWithDummyNode(
                            *this, CXX11Contracts, KeywordLoc));

  if (Res.isInvalid())
    return StmtError();

  return ActOnFinishFullStmt(Res.get());
}

/// ActOnResultNameDeclarator - Called from Parser::ParseFunctionDeclarator()
/// to introduce parameters into function prototype scope.
StmtResult Sema::ActOnResultNameDeclarator(ContractKind CK, Scope *S,
                                           QualType RetType,
                                           SourceLocation IDLoc,
                                           IdentifierInfo *II) {
  // assert(S && S->isContractAssertScope() && "Invalid scope for result name");
  assert(II && "ResultName requires an identifier");

  if (CK != ContractKind::Post) {
    assert(II && "ResultName requires an identifier");

    Diag(IDLoc, diag::err_result_name_not_allowed) << II;
    return StmtError();
  }

  if (RetType->isVoidType())  {
    Diag(IDLoc, diag::err_void_result_name) << II;
    return StmtError();
  }

  if (RetType->isUndeducedAutoType()) {
    if (!getLangOpts().LateParsedContracts) {
      Diag(IDLoc, diag::err_ericwf_unimplemented)
          << "Undeduced Auto Result Name";
      return StmtError();

    }
    FunctionDecl *FD = dyn_cast_or_null<FunctionDecl>(CurContext);
    if (!FD) {
      Diag(IDLoc, diag::err_ericwf_unimplemented)
          << "Undeduced Auto Result Name";
      return StmtError();
    }
    if (!FD->isDependentContext()) {
      if (DeduceReturnType(FD, IDLoc, /*Diagose=*/true))
        return StmtError();

      RetType = FD->getReturnType();
      if (RetType->isUndeducedAutoType()) {
        Diag(IDLoc, diag::err_ericwf_unimplemented)
            << "Failed to deduce return type";
        return StmtError();
      }
    }
  }


  // Check for redeclaration of parameters, e.g. int foo(int x, int x);
  if (II) {
    LookupResult R(*this, II, IDLoc, LookupOrdinaryName,
                   RedeclarationKind::ForVisibleRedeclaration); // FIXME(EricWF)
    LookupName(R, S);
    if (!R.empty()) {
      NamedDecl *PrevDecl = *R.begin();
      if (R.isSingleResult() && PrevDecl->isTemplateParameter()) {
        // Maybe we will complain about the shadowed template parameter.
        //DiagnoseTemplateParameterShadow(D.getIdentifierLoc(), PrevDecl);
        // Just pretend that we didn't see the previous declaration.
        PrevDecl = nullptr;
      }
      // FIXME(EricWF): Diagnose lookup conflicts with lambda captures and parameter declarations.
      if (auto* PVD = dyn_cast<ParmVarDecl>(PrevDecl)) {
        Diag(IDLoc, diag::err_result_name_shadows_param) << II; // FIXME(EricWF): Change the diagnostic here.
        Diag(PVD->getLocation(), diag::note_previous_declaration);
      }
    }
  }
  auto *New = ResultNameDecl::Create(Context, CurContext,
                     IDLoc, II, RetType);

  assert(S->isContractAssertScope());

  // Add the parameter declaration into this scope.
  S->AddDecl(New);
  IdResolver.AddDecl(New);

  return ActOnDeclStmt(ConvertDeclToDeclGroup(New), IDLoc, IDLoc);
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

public:
  ParamReferenceChecker(Sema &S, const FunctionDecl *FD) : Actions(S), FD(FD) {}

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
      if (!PVD || DiagnosedDecls.count(PVD) || PVD->getDeclContext() != FD)
        return;

      QualType PVDType =
          getFunctionOrMethodParamType(FD, PVD->getFunctionScopeIndex());
      if (PVDType->isReferenceType())
        return;

      DiagSelector DiagSelect = [&]() {
        if (PVDType->isArrayType())
          return DS_Array;
        if (PVDType->isFunctionType())
          return DS_Function;
        if (!PVDType.isConstQualified())
          return DS_NotConst;
        return DS_None;
      }();

      if (DiagSelect != DS_None) {
        DiagnosedDecls.insert(PVD);
        DidDiagnose = true;
        Actions.Diag(E->getLocation(),
                     diag::err_contract_postcondition_parameter_type_invalid)
            << PVD->getIdentifier() << DiagSelect
            << CurrentContract->getSourceRange();
        SourceRange TypeRange(PVD->getTypeSpecStartLoc(),
                              PVD->getTypeSpecEndLoc());
        Actions.Diag(PVD->getLocation(), diag::note_parameter_with_name)
            << PVD->getIdentifier() << TypeRange;
      }
    }();

    return true;
  }

private:
  DenseSet<ParmVarDecl *> DiagnosedDecls;
};

} // namespace

DeclResult Sema::BuildContractSpecifierDecl(ArrayRef<ContractStmt *> Contracts,
                                            bool IsInvalid) {
  ContractSpecifierDecl *CSD =
      ContractSpecifierDecl::Create(Context, CurContext, Contracts, IsInvalid);
  return CSD;
}

/// ActOnFinishContractSpecifierSequence - This is called after a
/// contract-specifier-seq has been parsed.
///
/// It's primary job is to set the canonical result name decl for each result
/// name.
ContractSpecifierDecl *
Sema::ActOnFinishContractSpecifierSequence(ArrayRef<ContractStmt *> Contracts,
                                           bool IsInvalid) {
  assert(!Contracts.empty() && "Expected at least one contract");

  ContractSpecifierDecl *CSD =
      ContractSpecifierDecl::Create(Context, CurContext, Contracts, IsInvalid);

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

void Sema::CheckFunctionContractSpecifier(FunctionDecl *FD) {
  FunctionDecl *First = FD->getFirstDecl();
  if (!First->hasContracts() && !FD->hasContracts())
    return;

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
}

void Sema::ActOnContractsOnFinishFunctionDecl(FunctionDecl *FD) {
  ContractSpecifierDecl *CSD = FD->getContracts();
  if (!CSD)
    return;
  // Check for post-conditions that reference non-const parameters.
  ParamReferenceChecker Checker(*this, FD);
  for (auto *CS : CSD->postconditions()) {
    Checker.TraverseContractStmt(CS);
    // FIXME(EricWF): DIagnose non-const function param types.
  }
  if (Checker.DidDiagnose)
    FD->setInvalidDecl(true);
}

namespace {
/// DiagnoseDeclRefVisitor - A terrible hack to re-bind (NOT rebuild) the
/// ParmVarDecl references in a contract to the new ParmVarDecls.
struct RebuildFunctionContracts
    : RecursiveASTVisitor<RebuildFunctionContracts> {

  Sema &S;
  const FunctionDecl *OldDecl;
  FunctionDecl *NewDecl;

  RebuildFunctionContracts(Sema &S, const FunctionDecl *OldDecl,
                           FunctionDecl *NewDecl)
      : S(S), OldDecl(OldDecl), NewDecl(NewDecl) {}

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    if (ParmVarDecl *PVD = dyn_cast_or_null<ParmVarDecl>(E->getDecl())) {
      unsigned IDX = PVD->getFunctionScopeIndex();
      if (IDX < OldDecl->getNumParams() && PVD == OldDecl->getParamDecl(IDX)) {
        ParmVarDecl *NewPVD = NewDecl->getParamDecl(IDX);
        E->setDecl(NewPVD);
        S.MarkAnyDeclReferenced(E->getLocation(), NewPVD, /*OdrUse=*/true);
      }
    }
    return true;
  }
};
} // namespace

void Sema::RebuildFunctionContractsAfterReturnTypeDeduction(FunctionDecl *FD) {

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

  auto *First = Def->getFirstDecl();
  if (!Def->getFirstDecl()->hasContracts())
    return;

  if (First != Def && First && Def->isThisDeclarationADefinition()
    && First->hasContracts() && !Def->hasContracts()) {
    RebuildFunctionContracts Rebuilder(*this, First, Def);
    for (auto *CS : First->contracts()) {
      Rebuilder.TraverseStmt(CS);
    }
    Def->setContracts(First->getContracts());
  }
  assert(Def->hasContracts());

  for (auto *RND : Def->getContracts()->result_names()) {
    if (RND->getType()->isUndeducedAutoType()) {
      Diag(RND->getLocation(), diag::err_ericwf_unimplemented)
          << "Undeduced Auto Result Name";
    }
  }
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

namespace {} // end namespace
