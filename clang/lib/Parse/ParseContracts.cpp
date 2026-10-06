
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
  // Contract keywords (including the reserved '__pre', '__post' and
  // '__contract_assert' spellings) are only recognized when contracts are
  // enabled.
  if (!getLangOpts().CPlusPlus11 || !getLangOpts().Contracts)
    return std::nullopt;

  if (Token.is(tok::kw_contract_assert))
    return ContractKind::Assert;

  if (Token.isNot(tok::identifier))
    return std::nullopt;

  const IdentifierInfo *II = Token.getIdentifierInfo();
  assert(II && "Missing identifier info");

  if (!Ident_pre) {
    Ident_pre = &PP.getIdentifierTable().get("pre");
    Ident___pre = &PP.getIdentifierTable().get("__pre");

    Ident_post = &PP.getIdentifierTable().get("post");
    Ident___post = &PP.getIdentifierTable().get("__post");
  }

  if (II == Ident_pre || II == Ident___pre)
    return ContractKind::Pre;

  if (II == Ident_post || II == Ident___post)
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

  // Cache the contract keyword.
  size_t StartSize = Toks.size();
  Toks.push_back(Tok);
  ConsumeToken();

  // Cache the optional attribute-specifier-seq.
  while (Tok.is(tok::l_square) && NextToken().is(tok::l_square)) {
    Toks.push_back(Tok);
    ConsumeBracket();
    ConsumeAndStoreUntil(tok::r_square, Toks,
                         /*StopAtSemi=*/true,
                         /*ConsumeFinalToken=*/true);
  }

  // Check for a '('.
  if (!Tok.is(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after) << CKStr;
    // Drop the partial contract so we don't try to parse it later.
    Toks.resize(StartSize);
    return false;
  }

  // Cache the tokens for the contract predicate.
  Toks.push_back(Tok); // '('
  ConsumeParen();

  ConsumeAndStoreUntil(tok::r_paren, Toks,
                       /*StopAtSemi=*/true,
                       /*ConsumeFinalToken=*/true);
  return true;
}

bool Parser::canHaveFunctionContracts(const Declarator &D) const {
  // [dcl.decl.general], [class.mem.general]: a function-contract-specifier-seq
  // shall be present only if the declarator declares a function.
  return D.isFunctionDeclarator() &&
         D.getDeclSpec().getStorageClassSpec() != DeclSpec::SCS_typedef;
}

void Parser::DiagnoseAndSkipMisplacedContractSpecifiers() {
  assert(isFunctionContractKeyword(Tok) && "Not in a contract");
  SourceLocation StartLoc = Tok.getLocation();
  CachedTokens Discarded;
  LateParseFunctionContractSpecifierSeq(Discarded);
  Diag(StartLoc, diag::err_contract_on_non_function)
      << SourceRange(StartLoc, PrevTokLocation);
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
  return ParseFunctionContractSpecifierImpl({}, IsInvalidTmp);
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
void Parser::ParseContractSpecifierSequence(Declarator &DeclarationInfo) {
  if (!isFunctionContractKeyword(Tok))
    return;

  // This is only used for lambda-declarators, where the contracts are parsed
  // once the call operator has been created and its context entered.
  assert(Actions.CurContext->isFunctionOrMethod() &&
         "contracts must be parsed in the context of their function");
  auto *FD = cast<FunctionDecl>(Actions.CurContext);
  auto ReturnTypeResolver = [&]() { return FD->getReturnType(); };

  bool IsInvalid = false;
  SourceLocation StartLoc = Tok.getLocation();

  SmallVector<ContractStmt *, 4> Contracts;
  while (isFunctionContractKeyword(Tok)) {
    bool IsInvalidTmp = false;
    StmtResult Contract =
        ParseFunctionContractSpecifierImpl(ReturnTypeResolver, IsInvalidTmp);
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
    llvm::function_ref<QualType()> ReturnTypeResolver, bool &IsInvalid) {
  assert(isAnyContractKeyword(Tok) && "Not a contract keyword?");
  ContractKind CK = getContractKeyword(Tok).value();
  assert((CK == ContractKind::Assert || ReturnTypeResolver) &&
         "Missing return type resolver for function contract sequence");
  llvm::scope_exit SetInvalidOnExit([&]() { IsInvalid = true; });

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

  // [basic.scope.contract]: Each contract assertion introduces a
  // contract-assertion scope.
  ParseScope ContractScope(this, Scope::DeclScope);
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
    if (ReturnTypeResolver)
      ReturnType = ReturnTypeResolver();

    // The result name is profiled (for the purpose of comparing contracts on
    // different declarations) like a parameter of the function, so give it
    // the same function scope depth as the function's parameters.
    unsigned FunctionScopeDepth = 0;
    if (auto *FD = dyn_cast<FunctionDecl>(Actions.CurContext);
        FD && FD->getNumParams())
      FunctionScopeDepth = FD->getParamDecl(0)->getFunctionScopeDepth();

    RND = Actions.ActOnResultNameDeclarator(CK, getCurScope(), ReturnType,
                                            IdLoc, Id, FunctionScopeDepth);

    if (!RND)
      return StmtError();

    if (RND->isInvalidDecl())
      IsInvalid = true;
  }

  ExprResult Cond = [&]() {
    Sema::ContractScopeRAII ContractScope(Actions, CK, KeywordLoc);
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

bool Parser::ParseLexedFunctionContracts(
    CachedTokens &ContractToks, Decl *FD,
    Parser::ContractEnterScopeKind ScopesToEnter) {
  assert(!ContractToks.empty() && "no contracts to parse");

  FunctionDecl *FunctionToPush = FD ? FD->getAsFunction() : nullptr;
  if (!FunctionToPush) {
    // The declaration was invalid or not a function; there is nothing to
    // attach the contracts to.
    ContractToks.clear();
    return false;
  }

  ParenBraceBracketBalancer BalancerRAIIObj(*this);

  // Add the 'stop' token.
  Token LastContractToken = ContractToks.back();
  Token ContractEnd;
  ContractEnd.startToken();
  ContractEnd.setKind(tok::eof);
  ContractEnd.setLocation(LastContractToken.getEndLoc());
  ContractEnd.setEofData(FD);
  ContractToks.push_back(ContractEnd);

  // Parse the contracts from their saved token stream.
  ContractToks.push_back(Tok); // So that the current token doesn't get lost
  PP.EnterTokenStream(ContractToks, true, /*IsReinject*/ true);

  // Consume the previously-pushed token.
  ConsumeAnyToken();

  CXXMethodDecl *Method = dyn_cast<CXXMethodDecl>(FunctionToPush);

  // Re-enter the function: a function scope whose entity is the function (so
  // that lookup proceeds through the function's semantic context, e.g. for
  // out-of-line members), containing the function parameters.
  std::optional<ParseScope> FnScope;
  bool ReenteredFunctionContext = false;
  if (ScopesToEnter & CES_ReenterFunction) {
    FnScope.emplace(this, Scope::FnScope | Scope::DeclScope);
    if (Actions.CurContext == FunctionToPush->getLexicalParent()) {
      Actions.ActOnReenterFunctionContext(getCurScope(), FunctionToPush);
      ReenteredFunctionContext = true;
    }
  }

  // Enter the context of the function, and push a function scope so that
  // lambdas within the contracts can capture the function parameters.
  std::optional<ParseScope> PlainFnScope;
  std::optional<Sema::ContextRAII> FnContext;
  std::optional<Sema::FunctionScopeRAII> PopFnContext;
  if (ScopesToEnter & (CES_Function | CES_ReenterFunction)) {
    if (!(ScopesToEnter & CES_ReenterFunction))
      PlainFnScope.emplace(this, Scope::FnScope);
    if (!ReenteredFunctionContext)
      FnContext.emplace(Actions, FunctionToPush, /*NewThisContext=*/false);
    PopFnContext.emplace(Actions);
    Actions.PushFunctionScope();
  }

  // C++11 [expr.prim.general]p3:
  //   If a declaration declares a member function or member function
  //   template of a class X, the expression this is a prvalue of type
  //   "pointer to cv-qualifier-seq X" between the optional cv-qualifer-seq
  //   and the end of the function-definition, member-declarator, or
  //   declarator.
  std::optional<Sema::CXXThisScopeRAII> ThisScope;
  if (ScopesToEnter & CES_CXXThis) {
    // 'this' may only be used in the contracts of an implicit object member
    // function.
    bool HasThis = Method && Method->isImplicitObjectMemberFunction();
    ThisScope.emplace(Actions, HasThis ? Method->getParent() : nullptr,
                      HasThis ? Method->getMethodQualifiers() : Qualifiers{},
                      HasThis && getLangOpts().CPlusPlus11);
  }

  SmallVector<ContractStmt *> Contracts;
  assert(isFunctionContractKeyword(Tok));

  SourceLocation StartLoc = Tok.getLocation();

  auto ReturnTypeResolver = [&]() { return FunctionToPush->getReturnType(); };
  bool IsInvalid = false;
  while (isFunctionContractKeyword(Tok)) {
    assert(Actions.CurContext == FunctionToPush);
    bool IsInvalidTmp = false;
    StmtResult Contract =
        ParseFunctionContractSpecifierImpl(ReturnTypeResolver, IsInvalidTmp);
    if (Contract.isUsable())
      Contracts.push_back(Contract.getAs<ContractStmt>());
    IsInvalid |= IsInvalidTmp;
  }

  ContractSpecifierDecl *Seq = Actions.ActOnFinishContractSpecifierSequence(
      Contracts, StartLoc, IsInvalid);

  // There could be leftover tokens (e.g. because of an error).
  // Skip through until we reach the original token position.
  while (Tok.isNot(tok::eof))
    ConsumeAnyToken();

  // Clean up the remaining EOF token.
  if (Tok.is(tok::eof) && Tok.getEofData() == FD)
    ConsumeAnyToken();

  ContractToks.clear();

  ThisScope.reset();
  PopFnContext.reset();
  FnContext.reset();
  PlainFnScope.reset();
  if (ReenteredFunctionContext)
    Actions.ActOnExitFunctionContext();
  FnScope.reset();

  Actions.ActOnFinishLateParsedFunctionContracts(
      FunctionToPush, Seq, bool(ScopesToEnter & CES_Definition));
  return true;
}
