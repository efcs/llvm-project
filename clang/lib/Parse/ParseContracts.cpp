
#include "clang/Parse/Parser.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/PrettyDeclStackTrace.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/EnterExpressionEvaluationContext.h"
#include "clang/Sema/Scope.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/TimeProfiler.h"
#include "llvm/ADT/ScopeExit.h"
#include <optional>

using namespace clang;

std::optional<ContractKind>
Parser::getContractKeyword(const Token &Token) const {
  // We offer the reserved keywords as identifiers in C++11 mode.
  if (!getLangOpts().CPlusPlus11 ||
      (Token.isNot(tok::identifier) && !Token.is(tok::kw_contract_assert)))
    return std::nullopt;

  // If we have a contract_assert keyword, we've may be using contract as an
  // extension, so don't check the language options.
  if (Token.is(tok::kw_contract_assert))
    return ContractKind::Assert;

  const IdentifierInfo *II = Token.getIdentifierInfo();
  assert(II && "Missing identifier info");

  if (!Ident_pre) {
    Ident_pre = &PP.getIdentifierTable().get("pre");
    Ident___pre = &PP.getIdentifierTable().get("__pre");

    Ident_post = &PP.getIdentifierTable().get("post");
    Ident___post = &PP.getIdentifierTable().get("__post");
  }

  if ((II == Ident_pre && getLangOpts().Contracts) || II == Ident___pre)
    return ContractKind::Pre;

  if ((II == Ident_post && getLangOpts().Contracts) || II == Ident___post)
    return ContractKind::Post;

  return std::nullopt;
}

void Parser::LateParseFunctionContractSpecifierSeq(CachedTokens &Toks) {
  while (isFunctionContractKeyword(Tok)) {
    if (!LateParseFunctionContractSpecifier(Toks)) {
      return;
    }
  }
}

static const char *getContractKeywordStr(ContractKind CK) {
  switch (CK) {
  case ContractKind::Pre:
    return "pre";
  case ContractKind::Post:
    return "post";
  case ContractKind::Assert:
    return "contract_assert";
  }
  llvm_unreachable("unhandled case");
}

bool Parser::LateParseFunctionContractSpecifier(CachedTokens &Toks) {
  assert(isFunctionContractKeyword(Tok) && "Not in a contract");
  ContractKind CK = getContractKeyword(Tok).value();
  const char *CKStr = getContractKeywordStr(CK);

  // Consume and cache the starting token.
  Token StartTok = Tok;
  SourceRange ContractRange = SourceRange(ConsumeToken());

  // Check for a '('.
  if (!Tok.is(tok::l_paren)) {
    // If this is a bare 'noexcept', we're done.

    Diag(Tok, diag::err_expected_lparen_after) << CKStr;
    return false;
  }

  // Cache the tokens for the exception-specification.

  Toks.push_back(StartTok);             // 'throw' or 'noexcept'
  Toks.push_back(Tok);                  // '('
  ContractRange.setEnd(ConsumeParen()); // '('

  ConsumeAndStoreUntil(tok::r_paren, Toks,
                       /*StopAtSemi=*/true,
                       /*ConsumeFinalToken=*/true);
  ContractRange.setEnd(Toks.back().getLocation());
  return true;
}

/// ParseContractAssertStatement
///
///  assertion-statement:
///     'contract_assert' attribute-specifier-seq[opt] '('
///     conditional-expression ')' ';'
///
StmtResult Parser::ParseContractAssertStatement() {
  assert((Tok.is(tok::kw_contract_assert)) &&
         "Not a contract asssert statement");
  bool IsInvalidTmp = false;
  return ParseFunctionContractSpecifierImpl({}, CSO_FunctionContext, IsInvalidTmp);
}

/// ParseFunctionContractSpecifierSeq - Parse a series of pre/post contracts on
/// a function declaration.
///
///   function-contract-specifier-seq :
///       function-contract-specifier function-contract-specifier-seq
//
///   function-contract-specifier:
///       precondition-specifier
///       postcondition-specifier
//
///   precondition-specifier:
///       pre attribute-specifier-seq[opt] ( conditional-expression )
///
///   postcondition-specifier:
///       post attribute-specifier-seq[opt] ( result-name-introducer[opt]
///       conditional-expression )
///
///   result-name-introducer:
///       attributed-identifier :
void Parser::ParseContractSpecifierSequence(Declarator &DeclarationInfo,
                                            bool EnterScope,
                                            QualType TrailingReturnType) {
  if (!isFunctionContractKeyword(Tok))
    return;

  std::optional<QualType> CachedType;
  auto ReturnTypeResolver = [&]() {
    if (!CachedType) {
      QualType ReturnType = TrailingReturnType;
      if (ReturnType.isNull()) {
        TypeSourceInfo *TInfo = Actions.GetTypeForDeclarator(DeclarationInfo);
        assert(TInfo && TInfo->getType()->isFunctionType());
        ReturnType = TInfo->getType()->getAs<FunctionType>()->getReturnType();
      }
      CachedType = ReturnType;
    }
    return CachedType.value();
  };
  std::optional<ParseScope> ParserScope;

  std::optional<Sema::CXXThisScopeRAII> ThisScope;
  std::optional<Sema::FunctionScopeRAII> PopFnContext;

  if (EnterScope) {
    ParserScope.emplace(this, Scope::DeclScope | Scope::FunctionPrototypeScope |
                                  Scope::FunctionDeclarationScope);

   // PopFnContext.emplace(Actions);
   // Actions.PushFunctionScope();

    auto FTI = DeclarationInfo.getFunctionTypeInfo();

    for (unsigned i = 0; i != FTI.NumParams; ++i) {
      ParmVarDecl *Param = cast<ParmVarDecl>(FTI.Params[i].Param);
      Actions.ActOnReenterCXXMethodParameter(getCurScope(), Param);
    }
  }

  InitCXXThisScopeForDeclaratorIfRelevant(DeclarationInfo,
                                          DeclarationInfo.getDeclSpec(),
                                          ThisScope);
  bool IsInvalid = false;
  SourceLocation StartLoc = Tok.getLocation();

  SmallVector<ContractStmt *, 4> Contracts;
  while (isFunctionContractKeyword(Tok)) {
    bool IsInvalidTmp = false;
    StmtResult Contract =
        ParseFunctionContractSpecifierImpl(ReturnTypeResolver, EnterScope ? CSO_ParentContext : CSO_FunctionContext, IsInvalidTmp);
    IsInvalid |= IsInvalidTmp;
    if (Contract.isUsable())
      Contracts.push_back(Contract.getAs<ContractStmt>());
  }
  ContractSpecifierDecl *Seq = Actions.ActOnFinishContractSpecifierSequence(
      Contracts, StartLoc, IsInvalid);

  assert(DeclarationInfo.Contracts == nullptr && "Already have contracts?");

  DeclarationInfo.Contracts = Seq;
}

StmtResult Parser::ParseFunctionContractSpecifierImpl(
    llvm::function_ref<QualType()> ReturnTypeResolver, ContractScopeOffset ScopeOffset, bool &IsInvalid) {
  assert(isAnyContractKeyword(Tok) && "Not a contract keyword?");
  ContractKind CK = getContractKeyword(Tok).value();
  assert((CK == ContractKind::Assert || ReturnTypeResolver) &&
         "Missing return type resolver for function contract sequence");
  assert((ScopeOffset == CSO_FunctionContext || CK != ContractKind::Assert) &&
         "Incorrect scope offset for contract assert");
  auto SetInvalidOnExit = llvm::make_scope_exit([&]() { IsInvalid = true; });

  const char *CKStr = getContractKeywordStr(CK);

  SourceLocation KeywordLoc = Tok.getLocation();
  ConsumeToken();

  ParsedAttributes CXX11Attrs(AttrFactory);
  MaybeParseCXX11Attributes(CXX11Attrs);

  if (Tok.isNot(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after) << CKStr;
    SkipUntil({tok::equal, tok::l_brace, tok::arrow, tok::kw_try, tok::comma,
               tok::l_paren},
              StopAtSemi | StopBeforeMatch);

    return StmtError();
  }

  BalancedDelimiterTracker T(*this, tok::l_paren);
  SourceLocation ExprLoc = Tok.getLocation();

  if (T.expectAndConsume(diag::err_expected_lparen_after, CKStr,
                         tok::r_paren)) {
    return StmtError();
  }

  ParseScope ContractScope(this, Scope::DeclScope | Scope::ContractAssertScope);
  EnterExpressionEvaluationContext EC(
      Actions, Sema::ExpressionEvaluationContext::PotentiallyEvaluated);

  ResultNameDecl *RND = nullptr;
  // FIXME(EricWF): We allow parsing the result name declarator in `pre` so we
  // can diagnose it but we don't do the same for contract assert... Should we?
  if ((CK != ContractKind::Assert) && Tok.is(tok::identifier) &&
      NextToken().is(tok::colon)) {
    // Let this parse for non-post contracts. We'll diagnose it later.

    IdentifierInfo *Id = Tok.getIdentifierInfo();
    SourceLocation IdLoc = ConsumeToken();

    ExprLoc = ConsumeToken();
    QualType ReturnType;
    if (ReturnTypeResolver) {
      ReturnType = ReturnTypeResolver();
    }

    RND = Actions.ActOnResultNameDeclarator(CK, getCurScope(), ReturnType,
                                            IdLoc, Id, getCurScope()->getFunctionPrototypeDepth());

    if (!RND)
      return StmtError();

    if (RND->isInvalidDecl())
      IsInvalid = true;
  }

  ExprResult Cond = [&]() {
    Sema::ContractScopeRAII ContractScope(Actions, CK, ScopeOffset, KeywordLoc);
    ExprResult CondResult = ParseConditionalExpression();
    if (CondResult.isInvalid())
      return CondResult;
    return Actions.ActOnContractAssertCondition(CondResult.get());
  }();
  SourceLocation EndLoc = Tok.getLocation();

  T.consumeClose();

  if (Cond.isInvalid()) {
    Cond =
        Actions.CreateRecoveryExpr(ExprLoc, EndLoc, {}, Actions.Context.BoolTy);
  } else {
    SetInvalidOnExit.release();
  }

  StmtResult Res =
      Actions.ActOnContractAssert(CK, KeywordLoc, Cond.get(), RND, CXX11Attrs);
  if (Res.isInvalid())
    IsInvalid = true;
  return Res;
}

static float EOFData = 0.0;

ContractSpecifierDecl *
Parser::ParseLexedFunctionContractsInScope(CachedTokens &ContractToks,
                                           QualType RetType) {
  // Add the 'stop' token.
  Token LastContractToken = ContractToks.back();
  Token ContractEnd;
  ContractEnd.startToken();
  ContractEnd.setKind(tok::eof);
  ContractEnd.setLocation(LastContractToken.getEndLoc());
  ContractEnd.setEofData(&EOFData);
  ContractToks.push_back(ContractEnd);

  // Parse the default argument from its saved token stream.
  ContractToks.push_back(Tok); // So that the current token doesn't get lost
  PP.EnterTokenStream(ContractToks, true, /*IsReinject*/ true);

  // Consume the previously-pushed token.
  ConsumeAnyToken();
  SourceLocation StartLoc = Tok.getLocation();
  auto ReturnTypeResolver = [&]() { return RetType; };

  bool IsInvalid = false;
  SmallVector<ContractStmt *> Contracts;

  while (isFunctionContractKeyword(Tok)) {
    assert(Actions.getCurFunction());
    bool IsInvalidTmp = false;
    StmtResult Contract =
        ParseFunctionContractSpecifierImpl(ReturnTypeResolver, CSO_FunctionContext, IsInvalidTmp);
    if (Contract.isUsable())
      Contracts.push_back(Contract.getAs<ContractStmt>());
    IsInvalid |= IsInvalidTmp;
  }
  DeclResult Req = Actions.ActOnFinishContractSpecifierSequence(
      Contracts, StartLoc, IsInvalid);

  // There could be leftover tokens (e.g. because of an error).
  // Skip through until we reach the original token position.
  while (Tok.isNot(tok::eof))
    ConsumeAnyToken();

  // Clean up the remaining EOF token.
  if (Tok.is(tok::eof) && Tok.getEofData() == &EOFData)
    ConsumeAnyToken();

  ContractToks.clear();

  if (Req.isInvalid())
    return nullptr;
  return Req.getAs<ContractSpecifierDecl>();
}

bool Parser::ParseLexedFunctionContracts(
    CachedTokens &ContractToks, Decl *FD,
    Parser::ContractEnterScopeKind ScopesToEnter) {

  // Add the 'stop' token.
  Token LastContractToken = ContractToks.back();
  Token ContractEnd;
  ContractEnd.startToken();
  ContractEnd.setKind(tok::eof);
  ContractEnd.setLocation(LastContractToken.getEndLoc());
  ContractEnd.setEofData(FD);
  ContractToks.push_back(ContractEnd);

  // Parse the default argument from its saved token stream.
  ContractToks.push_back(Tok); // So that the current token doesn't get lost
  PP.EnterTokenStream(ContractToks, true, /*IsReinject*/ true);

  // Consume the previously-pushed token.
  ConsumeAnyToken();

  // C++11 [expr.prim.general]p3:
  //   If a declaration declares a member function or member function
  //   template of a class X, the expression this is a prvalue of type
  //   "pointer to cv-qualifier-seq X" between the optional cv-qualifer-seq
  //   and the end of the function-definition, member-declarator, or
  //   declarator.
  CXXMethodDecl *Method;
  FunctionDecl *FunctionToPush;
  if (FunctionTemplateDecl *FunTmpl =
          dyn_cast<FunctionTemplateDecl>(FD))
    FunctionToPush = FunTmpl->getTemplatedDecl();
  else
    FunctionToPush = cast<FunctionDecl>(FD);

  std::optional<ParseScope> ParserScope;
  if (ScopesToEnter & ContractEnterScopeKind::CES_Prototype)
    ParserScope.emplace(this, Scope::DeclScope | Scope::FunctionPrototypeScope |
                                  Scope::FunctionDeclarationScope);

  if (ScopesToEnter & ContractEnterScopeKind::CES_Parameters) {
    for (auto *Param : FunctionToPush->parameters()) {
      Actions.ActOnReenterCXXMethodParameter(getCurScope(), Param);
    }
  }

  Method = dyn_cast<CXXMethodDecl>(FunctionToPush);

  std::optional<ParseScope> FnScope;
  std::optional<Sema::ContextRAII> FnContext;
  std::optional<Sema::FunctionScopeRAII> PopFnContext;
  if (ScopesToEnter & ContractEnterScopeKind::CES_Function) {
    FnScope.emplace(this, Scope::FnScope);
    FnContext.emplace(Actions, FunctionToPush, /*NewThisContext=*/true);
    PopFnContext.emplace(Actions);
    Actions.PushFunctionScope();
  }

  std::optional<Sema::CXXThisScopeRAII> ThisScope;
  if (ScopesToEnter & ContractEnterScopeKind::CES_CXXThis)
    ThisScope.emplace(Actions, Method ? Method->getParent() : nullptr,
                      Method ? Method->getMethodQualifiers() : Qualifiers{},
                      Method && getLangOpts().CPlusPlus11);

  // Parse the exception-specification.
  SmallVector<ContractStmt *> Contracts;
  assert(isFunctionContractKeyword(Tok));

  SourceLocation StartLoc = Tok.getLocation();

  auto ReturnTypeResolver = [&]() { return FunctionToPush->getReturnType(); };
  bool IsInvalid = false;
  while (isFunctionContractKeyword(Tok)) {
    assert(Actions.CurContext == FunctionToPush);
    bool IsInvalidTmp = false;
    StmtResult Contract =
        ParseFunctionContractSpecifierImpl(ReturnTypeResolver, CSO_FunctionContext, IsInvalidTmp);
    if (Contract.isUsable())
      Contracts.push_back(Contract.getAs<ContractStmt>());
    IsInvalid |= IsInvalidTmp;
  }
  ContractSpecifierDecl *Seq = Actions.ActOnFinishContractSpecifierSequence(
      Contracts, StartLoc, IsInvalid);
  FunctionToPush->setContracts(Seq);

  // There could be leftover tokens (e.g. because of an error).
  // Skip through until we reach the original token position.
  while (Tok.isNot(tok::eof))
    ConsumeAnyToken();

  // Clean up the remaining EOF token.
  if (Tok.is(tok::eof) && Tok.getEofData() == FD)
    ConsumeAnyToken();

  ContractToks.clear();
  return true;
}
