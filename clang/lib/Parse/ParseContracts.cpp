
#include "clang/Parse/Parser.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/PrettyDeclStackTrace.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/EnterExpressionEvaluationContext.h"
#include "clang/Sema/Scope.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/TimeProfiler.h"
#include <optional>

using namespace clang;

Parser::ContractKeyword Parser::getContractKeyword(const Token &Token) const {
  // We offer the reserved keywords as identifiers in C++11 mode.
  if (!getLangOpts().CPlusPlus11 || Token.isNot(tok::identifier))
    return ContractKeyword::None;

  const IdentifierInfo *II = Token.getIdentifierInfo();
  assert(II && "Missing identifier info");

  if (!Ident_pre) {
    Ident_pre = &PP.getIdentifierTable().get("pre");
    Ident___pre = &PP.getIdentifierTable().get("__pre");

    Ident_post = &PP.getIdentifierTable().get("post");
    Ident___post = &PP.getIdentifierTable().get("__post");
  }

  if ((II == Ident_pre && getLangOpts().Contracts) || II == Ident___pre)
    return ContractKeyword::Pre;

  if ((II == Ident_post && getLangOpts().Contracts) || II == Ident___post)
    return ContractKeyword::Post;

  return ContractKeyword::None;
}

void Parser::LateParseFunctionContractSpecifierSeq(CachedTokens &Toks) {
  while (isContractKeyword(Tok)) {
    if (!LateParseFunctionContractSpecifier(Toks)) {
      return;
    }
  }
}

static std::pair<ContractKind, const char *>
getContractKeywordInfo(Parser::ContractKeyword CK) {
  switch (CK) {
  case Parser::ContractKeyword::Pre:
    return std::make_pair(ContractKind::Pre, "pre");
  case Parser::ContractKeyword::Post:
    return std::make_pair(ContractKind::Post, "post");
  default:
    llvm_unreachable("unhandled case");
  }
}

bool Parser::LateParseFunctionContractSpecifier(CachedTokens &Toks) {
  auto [CK, CKStr] = getContractKeywordInfo(getContractKeyword(Tok));

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

bool Parser::ParseContractSpecifierSequence(Declarator &DeclarationInfo,
                                            bool EnterScope,
                                            QualType TrailingReturnType) {
  if (!getLangOpts().CPlusPlus || !getLangOpts().Contracts ||
      !isContractKeyword(Tok))
    return true;

  QualType ReturnType = TrailingReturnType;
  if (ReturnType.isNull()) {
    TypeSourceInfo *TInfo = Actions.GetTypeForDeclarator(DeclarationInfo);
    assert(TInfo && TInfo->getType()->isFunctionType());
    ReturnType = TInfo->getType()->getAs<FunctionType>()->getReturnType();
  }

  std::optional<ParseScope> ParserScope;
  std::optional<Sema::CXXThisScopeRAII> ThisScope;

  if (EnterScope) {
    ParserScope.emplace(this, Scope::DeclScope | Scope::FunctionPrototypeScope |
                                  Scope::FunctionDeclarationScope);

    auto FTI = DeclarationInfo.getFunctionTypeInfo();

    for (unsigned i = 0; i != FTI.NumParams; ++i) {
      ParmVarDecl *Param = cast<ParmVarDecl>(FTI.Params[i].Param);
      Actions.ActOnReenterCXXMethodParameter(getCurScope(), Param);
    }
  }

  InitCXXThisScopeForDeclaratorIfRelevant(DeclarationInfo,
                                          DeclarationInfo.getDeclSpec(),
                                          ThisScope, /*AddConst=*/true);
  bool IsSuccess = true;

  SmallVector<ContractStmt *, 4> Contracts;
  while (isContractKeyword(Tok)) {
    StmtResult Contract = ParseFunctionContractSpecifierImpl(ReturnType);
    if (Contract.isUsable()) {
      DeclarationInfo.Contracts.push_back(Contract.getAs<ContractStmt>());
    } else {
      IsSuccess = false;
    }
  }
  if (IsSuccess) {
    Actions.ActOnFinishContractSpecifierSequence(DeclarationInfo.Contracts);
  }
  return IsSuccess;
}

StmtResult Parser::ParseFunctionContractSpecifierImpl(QualType RetType) {
  auto [CK, CKStr] = getContractKeywordInfo(getContractKeyword(Tok));

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
  if (T.expectAndConsume(diag::err_expected_lparen_after, CKStr,
                         tok::r_paren)) {
    return StmtError();
  }

  ParseScope ContractScope(this, Scope::DeclScope | Scope::ContractAssertScope);

  EnterExpressionEvaluationContext EC(
      Actions, Sema::ExpressionEvaluationContext::PotentiallyEvaluated);
  Actions.ExprEvalContexts.back().InContractAssertion = true;

  DeclStmt *ResultNameStmt = nullptr;
  if (Tok.is(tok::identifier) && NextToken().is(tok::colon)) {
    // Let this parse for non-post contracts. We'll diagnose it later.

    IdentifierInfo *Id = Tok.getIdentifierInfo();
    SourceLocation IdLoc = ConsumeToken();

    SourceLocation ColonLoc = ConsumeToken();
    ((void)ColonLoc);

    StmtResult RNStmt =
        Actions.ActOnResultNameDeclarator(getCurScope(), RetType, IdLoc, Id);
    if (RNStmt.isUsable())
      ResultNameStmt = cast<DeclStmt>(RNStmt.get());
    else
      return StmtError();
  }

  SourceLocation Start = Tok.getLocation();
  ExprResult Cond = ParseConditionalExpression();

  if (Cond.isUsable()) {
    Cond = Actions.CorrectDelayedTyposInExpr(Cond, /*InitDecl=*/nullptr,
                                             /*RecoverUncorrectedTypos=*/true);
  } else {
    if (!Tok.is(tok::r_paren))
      SkipUntil(tok::r_paren, StopAtSemi | StopBeforeMatch);
    Cond = Actions.CreateRecoveryExpr(
        Start, Start == Tok.getLocation() ? Start : PrevTokLocation, {},
        Actions.getASTContext().BoolTy);
  }

  T.consumeClose();

  StmtResult ContractStmt = Actions.ActOnContractAssert(
      CK, KeywordLoc, Cond.get(), ResultNameStmt, CXX11Attrs);
  if (!ContractStmt.isUsable()) {
    return StmtError();
  }

  return ContractStmt;
}

bool Parser::ParseLexedFunctionContracts(CachedTokens &ContractToks, FunctionDecl *FD) {

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
  Method = dyn_cast<CXXMethodDecl>(FunctionToPush);
  QualType RetType = FunctionToPush->getReturnType();
  ParseScope FnScope(this, Scope::FnScope);
  Sema::ContextRAII FnContext(Actions, FunctionToPush,
                              /*NewThisContext=*/false);

  Sema::CXXThisScopeRAII ThisScope(
      Actions, Method ? Method->getParent() : nullptr,
      Method ? Method->getMethodQualifiers() : Qualifiers{},
      Method && getLangOpts().CPlusPlus11);

  // Parse the exception-specification.
  SmallVector<ContractStmt *> Contracts;
  assert(FunctionToPush->getContracts().empty());

  while (isContractKeyword(Tok)) {
    StmtResult Contract = ParseFunctionContractSpecifierImpl(RetType);
    if (Contract.isUsable()) {
      Contracts.push_back(Contract.getAs<ContractStmt>());
    } else {
      FunctionToPush->setInvalidDecl(true);
    }
  }

  FunctionToPush->setContracts(Contracts);
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