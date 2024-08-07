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
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"

using namespace clang;
using namespace sema;

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
  //assert(getCurScope()->isContractAssertScope() && "Incorrect scope for contract assert");
  if (Cond->isTypeDependent())
    return Cond;

  ExprResult E = PerformContextuallyConvertToBool(Cond);
  if (E.isInvalid()) {
    return E;
  }
  return ActOnFinishFullExpr(E.get(), /*DiscardedValue=*/false);
}

StmtResult Sema::BuildContractStmt(ContractKind CK, SourceLocation KeywordLoc,
                                   Expr *Cond, DeclStmt *ResultNameDecl,
                                   ArrayRef<const Attr *> Attrs) {
  assert((CK == ContractKind::Post || ResultNameDecl == nullptr) &&
         "ResultNameDecl only allowed for postconditions");
  assert(currentEvaluationContext().isContractAssertionContext() &&
         "Wrong context for statement");
  ExprResult E = ActOnContractAssertCondition(Cond);
  if (E.isInvalid())
    return StmtError();

  return ContractStmt::Create(Context, CK, KeywordLoc, E.get(), ResultNameDecl,
                              Attrs);
}

static ResultNameDecl *extractResultName(DeclStmt *DS) {
  assert(DS && DS->isSingleDecl() && "Expected a single declaration");
  auto *D = DS->getSingleDecl();
  return cast<ResultNameDecl>(D);
}

StmtResult Sema::ActOnContractAssert(ContractKind CK, SourceLocation KeywordLoc,
                                     Expr *Cond, DeclStmt *ResultNameDecl,
                                     ParsedAttributes &CXX11Contracts) {

  if (CK != ContractKind::Post && ResultNameDecl) {
        auto RND = extractResultName(ResultNameDecl);
        auto *II = RND->getDeclName().getAsIdentifierInfo();
        assert(II && "ResultName requires an identifier");

        Diag(RND->getBeginLoc(), diag::err_result_name_not_allowed)
                << II;
        return StmtError();
  }

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
StmtResult Sema::ActOnResultNameDeclarator(Scope *S, QualType RetType,
                                           SourceLocation IDLoc,
                                           IdentifierInfo *II) {
  // assert(S && S->isContractAssertScope() && "Invalid scope for result name");
  assert(II && "ResultName requires an identifier");

  if (RetType->isVoidType())  {
    Diag(IDLoc, diag::err_void_result_name) << II;
    return StmtError();
  }

  if (RetType->isUndeducedAutoType()) {
    Diag(IDLoc, diag::err_ericwf_unimplemented) << "Undeduced Auto Result Name";
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
      } else {
        // FIXME(EricWF): Is this an error?
        Diag(IDLoc, diag::warn_shadow_field) << II;
        Diag(PrevDecl->getLocation(), diag::note_previous_declaration);
      }
    }
  }

  // Temporarily put parameter variables in the translation unit, not
  // the enclosing context.  This prevents them from accidentally
  // looking like class members in C++.

  auto *New = ResultNameDecl::Create(Context, CurContext,
                     IDLoc, II, RetType);

  // CheckExplicitObjectParameter(*this, New, ExplicitThisLoc);

  assert(S->isContractAssertScope());
  // assert(S->isFunctionPrototypeScope());
  // assert(S->getFunctionPrototypeDepth() >= 1);
  // New->setDeclContext(getScopeForContext(S->getFunctionPrototypeDepth() - 1),
  //                  S->getNextFunctionPrototypeIndex());

  // Add the parameter declaration into this scope.
  S->AddDecl(New);
  IdResolver.AddDecl(New);

  return ActOnDeclStmt(ConvertDeclToDeclGroup(New), IDLoc, IDLoc);
}

bool Sema::CheckEquivalentContractSequence(FunctionDecl *OrigDecl,
                                           FunctionDecl *NewDecl) {

  auto GetFuncTP = [&](auto FD) {
    if (FunctionTemplateDecl *FunTmpl = dyn_cast<FunctionTemplateDecl>(FD))
      return FunTmpl->getTemplatedDecl();
    else
      return cast<FunctionDecl>(FD);
  };
  ((void)GetFuncTP);
  auto OT = dyn_cast<FunctionTemplateDecl>(OrigDecl);
  auto NT = dyn_cast<FunctionTemplateDecl>(NewDecl);
  if (OT && NT && NT != OT) {
    assert(false);
  }

  ArrayRef<const ContractStmt *> OrigContracts = OrigDecl->getContracts();
  ArrayRef<const ContractStmt *> NewContracts = NewDecl->getContracts();

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

/// ActOnFinishContractSpecifierSequence - This is called after a
/// contract-specifier-seq has been parsed.
///
/// It's primary job is to set the canonical result name decl for each result
/// name.
void Sema::ActOnFinishContractSpecifierSequence(
    SmallVector<ContractStmt *> Contracts) {
  if (Contracts.empty())
    return;
  SmallVector<ContractStmt *, 4> PreContracts;
  SmallVector<ContractStmt *, 4> PostContracts;
  SmallVector<ResultNameDecl *, 4> ResultNames;

  for (auto *CS : Contracts) {
    if (CS->getContractKind() == ContractKind::Pre)
      PreContracts.push_back(CS);
    else {
      PostContracts.push_back(CS);
      if (CS->hasResultName())
        ResultNames.push_back(CS->getResultName());
    }
  }
  if (!ResultNames.empty()) {
    ResultNameDecl *Canon = ResultNames.front();
    auto Pos = ResultNames.begin() + 1;
    for (; Pos != ResultNames.end(); ++Pos) {
      (*Pos)->setCanonicalResultNameDecl(Canon);
    }
  }
}

namespace {
/// RebuildFunctionContracts - A terrible hack to re-bind (NOT rebuild) the
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
  if (First == Def || !First->hasContracts() || Def->hasContracts())
    return;

  assert(Def->hasBody() && Def->isThisDeclarationADefinition());

  RebuildFunctionContracts Rebuilder(*this, First, Def);
  for (auto *CS : First->getContracts()) {
    Rebuilder.TraverseStmt(CS);
  }
  Def->setContracts(First->getContracts());
}

void Sema::ActOnContractsOnMergeFunctionDecl(NamedDecl *OrigDecl,
                                             NamedDecl *NewDecl) {
  auto *OrigFD = dyn_cast<FunctionDecl>(OrigDecl);
  auto *NewFD = dyn_cast<FunctionDecl>(NewDecl);
  if (!OrigFD && !NewFD)
    return;
  if (OrigFD && NewFD) {
    CheckEquivalentContractSequence(OrigFD, NewFD);
    return;
  }
}

Sema::ContractScopeRAII::ContractScopeRAII(Sema &S) : S(S) {
  assert(S.ExprEvalContexts.back().InContractAssertion == false);
  S.ExprEvalContexts.back().InContractAssertion = true;
}

Sema::ContractScopeRAII::~ContractScopeRAII() {
  assert(S.ExprEvalContexts.back().InContractAssertion == true);
  S.ExprEvalContexts.back().InContractAssertion = false;
}

namespace {} // end namespace