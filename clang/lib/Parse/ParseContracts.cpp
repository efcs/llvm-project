
#include "clang/Parse/Parser.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/PrettyDeclStackTrace.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Basic/AttributeCommonInfo.h"
#include "clang/Basic/Attributes.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/OperatorKinds.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/EnterExpressionEvaluationContext.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/SemaCodeCompletion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/TimeProfiler.h"
#include <optional>

using namespace clang;

Parser::ContractKeyword Parser::getContractKeyword(const Token &Token) const {
  if (!getLangOpts().Contracts || Token.isNot(tok::identifier))
    return ContractKeyword::None;

  const IdentifierInfo *II = Token.getIdentifierInfo();
  assert(II && "Missing identifier info");

  if (!Ident_pre) {
    Ident_pre = &PP.getIdentifierTable().get("pre");
    Ident_post = &PP.getIdentifierTable().get("post");
  }

  if (II == Ident_pre)
    return ContractKeyword::Pre;

  if (II == Ident_post)
    return ContractKeyword::Post;

  return ContractKeyword::None;
}

void Parser::MaybeLateParseFunctionContractSpecifierSeq(
    Declarator &DeclaratorInfo) {
  if (!getLangOpts().CPlusPlus || !getLangOpts().Contracts ||
      !isContractKeyword(Tok))
    return;
  ContractKeyword CKK;
  while ((CKK = getContractKeyword(Tok)) != ContractKeyword::None) {
    CachedTokens Toks;
    if (!LateParseFunctionContractSpecifier(
            DeclaratorInfo, DeclaratorInfo.LateParsedContracts)) {
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

bool Parser::LateParseFunctionContractSpecifier(Declarator &DeclaratorInfo,
                                                CachedTokens &Toks) {
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
    assert(TInfo);
    ReturnType = TInfo->getType();
    assert(ReturnType->isFunctionType());

    ReturnType = ReturnType->getAs<FunctionType>()->getReturnType();
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
    // RetType = GetTypeForDeclarator(FuncDecl);
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

StmtResult
Parser::ParseFunctionContractSpecifierOld(QualType RetType, bool EnterScope,
                                          Declarator *DeclarationInfo) {
  assert(!EnterScope || DeclarationInfo);
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
#if 0

  ParseScope ContractScope(this, Scope::DeclScope |
                                  Scope::FunctionPrototypeScope |
                                  Scope::ContractAssertScope);
#endif
  EnterExpressionEvaluationContext EC(
      Actions, Sema::ExpressionEvaluationContext::PotentiallyEvaluated);

  // FIXME(EricWF): This comment is out of date, but is it correct?

  // Don't include the Scope::FunctionDeclarationScope, since it puts the
  // result name introducer into wrong scope, allowing it to be referenced
  // outside of the postcondition.

  ParseScope ContractScope(
      this, Scope::DeclScope | Scope::FunctionPrototypeScope |
                Scope::FunctionDeclarationScope | Scope::ContractAssertScope);

#if 0
  // C++23 [basic.scope.namespace]p1:
  //   For each non-friend redeclaration or specialization whose target scope
  //   is or is contained by the scope, the portion after the declarator-id,
  //   class-head-name, or enum-head-name is also included in the scope.
  // C++23 [basic.scope.class]p1:
  //   For each non-friend redeclaration or specialization whose target scope
  //   is or is contained by the scope, the portion after the declarator-id,
  //   class-head-name, or enum-head-name is also included in the scope.
  //
  // FIXME: We should really be calling ParseTrailingRequiresClause in
  // ParseDirectDeclarator, when we are already in the declarator scope.
  // This would also correctly suppress access checks for specializations
  // and explicit instantiations, which we currently do not do.
  CXXScopeSpec &SS = DeclaratorInfo.getCXXScopeSpec();
  DeclaratorScopeObj DeclScopeObj(*this, SS);
  if (SS.isValid() && Actions.ShouldEnterDeclaratorScope(getCurScope(), SS))
    DeclScopeObj.EnterDeclaratorScope();

  ParseScope ContractScope2(this, Scope::DeclScope |
                                  Scope::FunctionDeclarationScope |
                                  Scope::FunctionPrototypeScope |
                                  Scope::ContractAssertScope);
  auto& D = DeclaratorInfo;
  std::optional<Sema::CXXThisScopeRAII> ThisScope;
  InitCXXThisScopeForDeclaratorIfRelevant(D, D.getDeclSpec(), ThisScope);



  std::optional<Sema::CXXThisScopeRAII> ThisScope;
  InitCXXThisScopeForDeclaratorIfRelevant(DeclaratorInfo, DeclaratorInfo.getDeclSpec(), ThisScope);



  DeclaratorChunk::FunctionTypeInfo FTI = DeclaratorInfo.getFunctionTypeInfo();

  for (unsigned i = 0; i != FTI.NumParams; ++i) {
    ParmVarDecl *Param = cast<ParmVarDecl>(FTI.Params[i].Param);
    Actions.ActOnReenterCXXMethodParameter(getCurScope(), Param);
  }

#endif
  std::optional<DeclaratorScopeObj> DeclScopeObj;
  std::optional<Sema::CXXThisScopeRAII> ThisScope;

  if (EnterScope) {

    assert(DeclarationInfo);
    auto &D = *DeclarationInfo;
    CXXScopeSpec &SS = D.getCXXScopeSpec();
    DeclScopeObj.emplace(*this, SS);
    if (SS.isValid() && Actions.ShouldEnterDeclaratorScope(getCurScope(), SS))
      (*DeclScopeObj).EnterDeclaratorScope();

    InitCXXThisScopeForDeclaratorIfRelevant(D, D.getDeclSpec(), ThisScope);
    auto FTI = D.getFunctionTypeInfo();

    for (unsigned i = 0; i != FTI.NumParams; ++i) {
      ParmVarDecl *Param = cast<ParmVarDecl>(FTI.Params[i].Param);
      Actions.ActOnReenterCXXMethodParameter(getCurScope(), Param);
    }

  } else if (false) {
    auto *FnDecl = Actions.getCurFunctionDecl();

    assert(FnDecl);
    // InitCXXThisScopeForDeclaratorIfRelevant(D, D.getDeclSpec(), ThisScope);
    // auto FTI = D.getFunctionTypeInfo();
  }

  Sema::ContractScopeRAII ContractExpressionScope(Actions);

  DeclStmt *ResultNameStmt = nullptr;
  if (Tok.is(tok::identifier) && NextToken().is(tok::colon)) {
    // Let this parse for non-post contracts. We'll diagnose it later.

    IdentifierInfo *Id = Tok.getIdentifierInfo();
    SourceLocation IdLoc = ConsumeToken();

    SourceLocation ColonLoc = ConsumeToken();
    ((void)ColonLoc);
    // RetType = GetTypeForDeclarator(FuncDecl);
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
