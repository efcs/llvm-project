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

ExprResult Sema::ActOnContractAssertCondition(Expr *Cond) {
  return PerformContextuallyConvertToBool(Cond);
}

StmtResult Sema::BuildContractStmt(ContractKind CK, SourceLocation KeywordLoc,
                                   Expr *Cond, DeclStmt *ResultNameDecl) {
  return ContractStmt::Create(Context, CK, KeywordLoc, Cond, ResultNameDecl);
}

StmtResult Sema::ActOnContractAssert(SourceLocation KeywordLoc, Expr *Cond) {
  ExprResult CheckedCond = ActOnContractAssertCondition(Cond);
  if (CheckedCond.isInvalid())
    return StmtError();
  Cond = CheckedCond.get();

  return BuildContractStmt(ContractKind::Assert, KeywordLoc, Cond, nullptr);
}
StmtResult Sema::ActOnPreContractAssert(SourceLocation KeywordLoc, Expr *Cond) {
  ExprResult CheckedCond = ActOnContractAssertCondition(Cond);
  if (CheckedCond.isInvalid())
    return StmtError();
  Cond = CheckedCond.get();

  return BuildContractStmt(ContractKind::Pre, KeywordLoc, Cond, nullptr);
}

StmtResult Sema::ActOnPostContractAssert(SourceLocation KeywordLoc, Expr *Cond,
                                         DeclStmt *ResultNameDecl) {
  assert(ResultNameDecl == nullptr && "Result name decl not supported yet");

  ExprResult CheckedCond = ActOnContractAssertCondition(Cond);
  if (CheckedCond.isInvalid())
    return StmtError();
  Cond = CheckedCond.get();

  return BuildContractStmt(ContractKind::Post, KeywordLoc, Cond,
                           ResultNameDecl);
}
