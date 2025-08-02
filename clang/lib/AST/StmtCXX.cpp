//===--- StmtCXX.cpp - Classes for representing C++ statements ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the subclesses of Stmt class declared in StmtCXX.h
//
//===----------------------------------------------------------------------===//

#include "clang/AST/StmtCXX.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/ADT/StringExtras.h"

using namespace clang;

QualType CXXCatchStmt::getCaughtType() const {
  if (ExceptionDecl)
    return ExceptionDecl->getType();
  return QualType();
}

CXXTryStmt *CXXTryStmt::Create(const ASTContext &C, SourceLocation tryLoc,
                               CompoundStmt *tryBlock,
                               ArrayRef<Stmt *> handlers) {
  const size_t Size = totalSizeToAlloc<Stmt *>(handlers.size() + 1);
  void *Mem = C.Allocate(Size, alignof(CXXTryStmt));
  return new (Mem) CXXTryStmt(tryLoc, tryBlock, handlers);
}

CXXTryStmt *CXXTryStmt::Create(const ASTContext &C, EmptyShell Empty,
                               unsigned numHandlers) {
  const size_t Size = totalSizeToAlloc<Stmt *>(numHandlers + 1);
  void *Mem = C.Allocate(Size, alignof(CXXTryStmt));
  return new (Mem) CXXTryStmt(Empty, numHandlers);
}

CXXTryStmt::CXXTryStmt(SourceLocation tryLoc, CompoundStmt *tryBlock,
                       ArrayRef<Stmt *> handlers)
    : Stmt(CXXTryStmtClass), TryLoc(tryLoc), NumHandlers(handlers.size()) {
  Stmt **Stmts = getStmts();
  Stmts[0] = tryBlock;
  llvm::copy(handlers, Stmts + 1);
}

CXXForRangeStmt::CXXForRangeStmt(Stmt *Init, DeclStmt *Range,
                                 DeclStmt *BeginStmt, DeclStmt *EndStmt,
                                 Expr *Cond, Expr *Inc, DeclStmt *LoopVar,
                                 Stmt *Body, SourceLocation FL,
                                 SourceLocation CAL, SourceLocation CL,
                                 SourceLocation RPL)
    : Stmt(CXXForRangeStmtClass), ForLoc(FL), CoawaitLoc(CAL), ColonLoc(CL),
      RParenLoc(RPL) {
  SubExprs[INIT] = Init;
  SubExprs[RANGE] = Range;
  SubExprs[BEGINSTMT] = BeginStmt;
  SubExprs[ENDSTMT] = EndStmt;
  SubExprs[COND] = Cond;
  SubExprs[INC] = Inc;
  SubExprs[LOOPVAR] = LoopVar;
  SubExprs[BODY] = Body;
}

Expr *CXXForRangeStmt::getRangeInit() {
  DeclStmt *RangeStmt = getRangeStmt();
  VarDecl *RangeDecl = dyn_cast_or_null<VarDecl>(RangeStmt->getSingleDecl());
  assert(RangeDecl && "for-range should have a single var decl");
  return RangeDecl->getInit();
}

const Expr *CXXForRangeStmt::getRangeInit() const {
  return const_cast<CXXForRangeStmt *>(this)->getRangeInit();
}

VarDecl *CXXForRangeStmt::getLoopVariable() {
  Decl *LV = cast<DeclStmt>(getLoopVarStmt())->getSingleDecl();
  assert(LV && "No loop variable in CXXForRangeStmt");
  return cast<VarDecl>(LV);
}

const VarDecl *CXXForRangeStmt::getLoopVariable() const {
  return const_cast<CXXForRangeStmt *>(this)->getLoopVariable();
}

CoroutineBodyStmt *CoroutineBodyStmt::Create(
    const ASTContext &C, CoroutineBodyStmt::CtorArgs const &Args) {
  std::size_t Size = totalSizeToAlloc<Stmt *>(
      CoroutineBodyStmt::FirstParamMove + Args.ParamMoves.size());

  void *Mem = C.Allocate(Size, alignof(CoroutineBodyStmt));
  return new (Mem) CoroutineBodyStmt(Args);
}

CoroutineBodyStmt *CoroutineBodyStmt::Create(const ASTContext &C, EmptyShell,
                                             unsigned NumParams) {
  std::size_t Size = totalSizeToAlloc<Stmt *>(
      CoroutineBodyStmt::FirstParamMove + NumParams);

  void *Mem = C.Allocate(Size, alignof(CoroutineBodyStmt));
  auto *Result = new (Mem) CoroutineBodyStmt(CtorArgs());
  Result->NumParams = NumParams;
  auto *ParamBegin = Result->getStoredStmts() + SubStmt::FirstParamMove;
  std::uninitialized_fill(ParamBegin, ParamBegin + NumParams,
                          static_cast<Stmt *>(nullptr));
  return Result;
}

CoroutineBodyStmt::CoroutineBodyStmt(CoroutineBodyStmt::CtorArgs const &Args)
    : Stmt(CoroutineBodyStmtClass), NumParams(Args.ParamMoves.size()) {
  Stmt **SubStmts = getStoredStmts();
  SubStmts[CoroutineBodyStmt::Body] = Args.Body;
  SubStmts[CoroutineBodyStmt::Promise] = Args.Promise;
  SubStmts[CoroutineBodyStmt::InitSuspend] = Args.InitialSuspend;
  SubStmts[CoroutineBodyStmt::FinalSuspend] = Args.FinalSuspend;
  SubStmts[CoroutineBodyStmt::OnException] = Args.OnException;
  SubStmts[CoroutineBodyStmt::OnFallthrough] = Args.OnFallthrough;
  SubStmts[CoroutineBodyStmt::Allocate] = Args.Allocate;
  SubStmts[CoroutineBodyStmt::Deallocate] = Args.Deallocate;
  SubStmts[CoroutineBodyStmt::ResultDecl] = Args.ResultDecl;
  SubStmts[CoroutineBodyStmt::ReturnValue] = Args.ReturnValue;
  SubStmts[CoroutineBodyStmt::ReturnStmt] = Args.ReturnStmt;
  SubStmts[CoroutineBodyStmt::ReturnStmtOnAllocFailure] =
      Args.ReturnStmtOnAllocFailure;
  llvm::copy(Args.ParamMoves, const_cast<Stmt **>(getParamMoves().data()));
}

ContractStmt *ContractStmt::CreateEmpty(const ASTContext &C, ContractKind Kind,
                                        bool HasResultName, unsigned NumAttrs) {
  void *Mem = C.Allocate(
      totalSizeToAlloc<Stmt *, const Attr *>(1 + HasResultName, NumAttrs),
      alignof(ContractStmt));
  return new (Mem) ContractStmt(EmptyShell(), Kind, HasResultName);
}

ContractStmt *ContractStmt::Create(const ASTContext &C, ContractKind Kind,
                                   SourceLocation KeywordLoc, Expr *Condition,
                                   DeclStmt *ResultNameDecl,
                                   ArrayRef<const Attr *> Attrs) {
  assert((ResultNameDecl == nullptr || Kind == ContractKind::Post) &&
         "Only a postcondition can have a result name declaration");
  void *Mem = C.Allocate(totalSizeToAlloc<Stmt *, const Attr *>(
                             1 + (ResultNameDecl != nullptr), Attrs.size()),
                         alignof(ContractStmt));
  return new (Mem)
      ContractStmt(Kind, KeywordLoc, Condition, ResultNameDecl, Attrs);
}

ResultNameDecl *ContractStmt::getResultName() const {
  if (!hasResultName())
    return nullptr;
  DeclStmt* D = getResultNameDeclStmt();
  assert(D);
  return cast<ResultNameDecl>(D->getSingleDecl());
}

std::string ContractStmt::getMessage(const clang::ASTContext &Ctx) const {
  if (auto *A = getAttrAs<ContractMessageAttr>()) {
    if (A->getIncludeSourceText()) {
      return llvm::join_items("", getSourceText(Ctx), ": \"", A->getMessage(),
                              '"');
    }
    return llvm::join_items("", '"', A->getMessage(), '"');
  }
  return getSourceText(Ctx);
}

std::string ContractStmt::getSourceText(const ASTContext &Ctx) const {
  auto &SM = Ctx.getSourceManager();
  auto Begin = hasResultName() ? getResultName()->getBeginLoc()
                               : getCond()->getBeginLoc();
  auto End = getCond()->getEndLoc();
  CharSourceRange ExprRange = Lexer::getAsCharRange(
      SM.getExpansionRange(SourceRange(Begin, End)), SM, Ctx.getLangOpts());
  std::string AssertStr =
      Lexer::getSourceText(ExprRange, SM, Ctx.getLangOpts()).str();
  return AssertStr;
}

StringRef ContractStmt::ContractKindAsString(ContractKind K) {
  switch (K) {
  case ContractKind::Assert:
    return "contract_assert";
  case ContractKind::Pre:
    return "pre";
  case ContractKind::Post:
    return "post";
  }
  llvm_unreachable("Unknown contract kind");
}

StringRef ContractStmt::SemanticAsString(ContractEvaluationSemantic Sem) {
  switch (Sem) {
  case ContractEvaluationSemantic::Ignore:
    return "ignore";
  case ContractEvaluationSemantic::Observe:
    return "observe";
  case ContractEvaluationSemantic::Enforce:
    return "enforce";
  case ContractEvaluationSemantic::QuickEnforce:
    return "quick_enforce";
  }
  llvm_unreachable("Unknown contract kind");
}

ContractEvaluationSemantic
ContractStmt::getSemantic(const ASTContext &Ctx) const {
  if (auto *A = getAttrAs<ContractGroupAttr>(); A)
    return Ctx.getLangOpts().ContractOpts.getSemanticForGroup(A->getGroup());
  return Ctx.getLangOpts().ContractOpts.DefaultSemantic;
}
