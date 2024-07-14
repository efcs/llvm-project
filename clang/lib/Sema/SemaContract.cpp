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
  buildAttributesWithDummyNode(Sema &S, ParsedAttributes &Attrs) {
    ContractStmt Dummy(Stmt::EmptyShell(), ContractKind::Pre, false, 0);
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
  assert(currentEvaluationContext().InContractStatement &&
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

  StmtResult Res = BuildContractStmt(
      CK, KeywordLoc, Cond, ResultNameDecl,
      SemaContractHelper::buildAttributesWithDummyNode(*this, CXX11Contracts));

  if (Res.isInvalid())
    return StmtError();

  return ActOnFinishFullStmt(Res.get());
}

/* FIXME(EricWF): Is this needed?
 *
void Sema::ActOnStartContracts(Scope *S, Declarator &D) {
  if (!D.isFunctionDeclarator())
    return;
  auto &FTI = D.getFunctionTypeInfo();
  if (!FTI.Params)
    return;
  for (auto &Param : ArrayRef<DeclaratorChunk::ParamInfo>(FTI.Params,
                                                          FTI.NumParams)) {
    auto *ParamDecl = cast<NamedDecl>(Param.Param);
    if (ParamDecl->getDeclName())
      PushOnScopeChains(ParamDecl, S, false);//AddToContext=false);
  }
}
*/

/// ActOnResultNameDeclarator - Called from Parser::ParseFunctionDeclarator()
/// to introduce parameters into function prototype scope.
StmtResult Sema::ActOnResultNameDeclarator(Scope *S, Declarator &FuncDecl,
                                      SourceLocation IDLoc,
                                      IdentifierInfo *II)  {
  assert(S && S->isContractAssertScope() && "Invalid scope for result name");
  assert(II && "ResultName requires an identifier");
  //CheckFunctionOrTemplateParamDeclarator(S, D);

  TypeSourceInfo *TInfo = GetTypeForDeclarator(FuncDecl);
  assert(TInfo && TInfo->getType()->isFunctionType() && "no type from declarator in ActOnParamDeclarator");
  QualType RetType = TInfo->getType()->getAs<FunctionType>()->getReturnType();

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
      } else if (auto *CD = dyn_cast<CapturedDecl>(PrevDecl)) {
        Diag(IDLoc, diag::err_redefinition_different_kind) << II;
        Diag(CD->getLocation(), diag::note_previous_declaration);
      } else {
        Diag(IDLoc, diag::err_ericwf_fixme) << "Add A Diagnostic Here";
      }
    }
  }

  // Temporarily put parameter variables in the translation unit, not
  // the enclosing context.  This prevents them from accidentally
  // looking like class members in C++.

  auto *New = ResultNameDecl::Create(Context, CurContext,
                     IDLoc, II, RetType);

  if (FuncDecl.isInvalidType())
    New->setInvalidDecl();

  //CheckExplicitObjectParameter(*this, New, ExplicitThisLoc);
  assert(S->isContractAssertScope());
  assert(S->isFunctionPrototypeScope());
  assert(S->getFunctionPrototypeDepth() >= 1);
  //New->setDeclContext(getScopeForContext(S->getFunctionPrototypeDepth() - 1,
   //                 S->getNextFunctionPrototypeIndex());

  // Add the parameter declaration into this scope.
  S->AddDecl(New);
  IdResolver.AddDecl(New);

  return ActOnDeclStmt(ConvertDeclToDeclGroup(New), IDLoc, IDLoc);
}

Sema::ContractScopeRAII::ContractScopeRAII(Sema &SemaRef)
    : S(&SemaRef), OldCXXThisType(SemaRef.CXXThisTypeOverride),
      OldIsContractScope(SemaRef.ExprEvalContexts.back().InContractStatement) {
  QualType NewT = S->CXXThisTypeOverride;

  S->CXXThisTypeOverride = NewT;
  S->ExprEvalContexts.back().InContractStatement = true;
}

Sema::ContractScopeRAII::~ContractScopeRAII() {
  S->CXXThisTypeOverride = OldCXXThisType;
  S->ExprEvalContexts.back().InContractStatement = OldIsContractScope;
}
