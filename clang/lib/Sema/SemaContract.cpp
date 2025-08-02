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
#include "clang/Basic/EricWFDebug.h"
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
using llvm::DenseMap;
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
template <class T> struct ValueRAII {
  ValueRAII(T &XDest, T const &Value, bool Enter = true) : Dest(&XDest) {
    if (Enter)
      push(Value);
  }

  ~ValueRAII() { pop(); }

  ValueRAII(ValueRAII const &) = delete;
  ValueRAII &operator=(ValueRAII const &) = delete;

  void push(T const &Value) {
    assert(!Entered);
    OldValue = *Dest;
    *Dest = Value;
    Entered = true;
  }

  void pop() {
    if (Entered)
      *Dest = OldValue;
    Entered = false;
  }

private:
  bool Entered = false;
  T *Dest;
  T OldValue;
};

template <class T> ValueRAII(T &, T const &) -> ValueRAII<T>;

} // namespace

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
                                     ParsedAttributes &ContractAttrs) {

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
                            *this, ContractAttrs, KeywordLoc));

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
                                                IdentifierInfo *II,
                                                unsigned FunctionScopeDepth) {
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

  // If needed, invent a fake placeholder type to represent the result name.
  // This is needed when we encounter a deduced return type on a non-template
  // function. We'll replace this once we've completed the function definition
  // (which must be attached to this declaration).
  bool HasInventedPlaceholderTypes =
      RetType->isUndeducedAutoType() && !RetType->isDependentType();
  if (HasInventedPlaceholderTypes)
    RetType = Context.getAutoType(QualType(), AutoTypeKeyword::Auto, true,
                                  false, nullptr, {});
  auto *New = ResultNameDecl::Create(Context, CurContext, IDLoc, II, RetType,
                                     nullptr, HasInventedPlaceholderTypes, FunctionScopeDepth);

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

const char *ScopeKindToString(unsigned Val) {
  switch (Val) {
  case 0:
    return "Function";
  case 1:
    return "Block";
  case 2:
    return "Lambda";
  case 3:
    return "CapturingRegion";
  default:
    return "<INVALID>";
  }
}

void showContextChain(const DeclContext *DC, bool Lexical = false) {
  unsigned Depth = 0;
  while (DC != nullptr) {
    EricWFDump(std::string(Lexical ? "Lexical " : "") + "Ctx #" +
                   std::to_string(Depth++),
               DC);
    DC = Lexical ? DC->getLexicalParent() : DC->getParent();
  }
  llvm::errs() << "Found #" << Depth << " Contexts\n\n\n";
}

void debugIt(const Sema &S) {
  llvm::errs() << "\n\n";
  llvm::errs() << "Partial Scopes:\n";
  for (auto *FSI : S.getFunctionScopes()) {
    llvm::errs() << ScopeKindToString(FSI->Kind) << "\n";
  }
  llvm::errs() << "Full Scopes\n";
  for (auto *FSI : S.FunctionScopes) {
    llvm::errs() << ScopeKindToString(FSI->Kind) << "\n";
  }

  llvm::errs() << "\n\nDumping Context\n\n";

  showContextChain(S.CurContext);
  llvm::errs() << "\n\n\nDumping Lexical Context\n\n";

  showContextChain(S.CurContext, true);
  llvm::errs() << "\n\n\n";
}

using ContractScopeRecord = Sema::ContractScopeRecord;

struct ScopeEntry {
  const DeclContext *Ctx = nullptr;

  unsigned FunctionScopeIndex = -1;
  const FunctionScopeInfo *FSI = nullptr;

  unsigned ContractScopeIndex = 0;
  const ContractScopeRecord *CSR = nullptr;

  ScopeEntry(const DeclContext *DC, unsigned FSII, const FunctionScopeInfo *FSI,
             unsigned CSII, const ContractScopeRecord *CSR) :
        Ctx(DC), FunctionScopeIndex(FSII), FSI(FSI),
        ContractScopeIndex(CSII), CSR(CSR) {
  }

  bool capturesVariable(const ValueDecl *VD) const {
    return getCaptureIfCaptured(VD).has_value();
  }

  std::optional<Capture> getCaptureIfCaptured(const ValueDecl *VD) const {
    assert(FSI);
    auto *CSI = dyn_cast<CapturingScopeInfo>(FSI);
    if (!CSI)
      return std::nullopt;

    if (CSI->isCaptured(const_cast<ValueDecl *>(VD)))
      return CSI->getCapture(const_cast<ValueDecl *>(VD));
    return std::nullopt;
  }

  void dump() const {
    assert(Ctx != nullptr);
    assert(FSI != nullptr);
    llvm::errs() << "ScopeEntry " << Ctx->getDeclKindName() << " ";
    EricWFDump("Context is: ", Ctx);
    llvm::errs() << "FunctionScopeIndex: " << FunctionScopeIndex << " ";
    llvm::errs() << "ContractScopeIndex: " << ContractScopeIndex << " ";
  }
};

struct ScopeWalker {
  explicit ScopeWalker(const Sema &S)
      : S(S), CurCtx(S.CurContext), FunctionScopes(S.FunctionScopes),
        FunctionScopeIndex(FunctionScopes.size()),
        ContractScopeIndex(S.getContractScopes().size()) {
    for (auto &Item : S.getContractScopes())
      ContractScopes.push_back(&Item);
    assert(CurCtx);
  }

  ScopeWalker(ScopeWalker const &) = delete;
  ScopeWalker &operator=(ScopeWalker const &) = delete;

  FunctionScopeInfo *nextFuncScope() {
    --FunctionScopeIndex;
    ERICWF_FANCY_ASSERT(FunctionScopeIndex < FunctionScopes.size()) {
      DumpScopes();
    }
    return FunctionScopes[FunctionScopeIndex];
  }

  const ContractScopeRecord *nextContractScope() {
    --ContractScopeIndex;
    ERICWF_FANCY_ASSERT(ContractScopeIndex < ContractScopes.size()) {
      DumpScopes();
    }
    return ContractScopes[ContractScopeIndex];
  }

  void DumpScopes() const {

    llvm::errs() << "Have # of scopes: " << Scopes.size() << "\n";
    llvm::errs() << "FunctionScopes.size() " << FunctionScopes.size() << "\n";
    llvm::errs() << "ContractScopes.size() " << ContractScopes.size() << "\n";
    llvm::errs() << "Actual Number of FunctionScopes: "
                 << S.FunctionScopes.size() << "\n";
    llvm::errs() << "Start FunctionScopeIndex: " << S.FunctionScopesStart
                 << "\n";
    unsigned Idx = 0;

    for (auto SC : llvm::reverse(Scopes)) {
      llvm::errs() << "ScopeEntry " << Idx++ << " ";
      SC.dump();
    }

    debugIt(S);
  }

  SmallVector<ScopeEntry> doIt() {
    while (CurCtx) {
      auto *FSI = nextFuncScope();
      ERICWF_FANCY_ASSERT(FSI) { DumpScopes(); }
      const ContractScopeRecord *CSR = nullptr;
      if (FSI->ContractScopeIndex != unsigned(-1)) {
        CSR = &S.ContractScopeStack[FSI->ContractScopeIndex];
        ERICWF_FANCY_ASSERT(CSR && CSR->FunctionScopeAtPush == FSI) {
          DumpScopes();
        }
        Scopes.emplace_back(CurCtx, FunctionScopeIndex, FSI, ContractScopeIndex,
                            CSR);
      } else {
        Scopes.emplace_back(CurCtx, FunctionScopeIndex, FSI, 0, nullptr);
      }

      // We must be in a function declaration
      if (!CurCtx->isFunctionOrMethod()) {
        break;
      }


      CurCtx =
          getLambdaAwareParentOfDeclContext(const_cast<DeclContext *>(CurCtx));
      if (!CurCtx || !CurCtx->isFunctionOrMethod())
        break;
      if (Scopes.size() == FunctionScopes.size())
        break;
    }

    ERICWF_FANCY_ASSERT((Scopes.size() <= FunctionScopes.size() && Scopes.size() >= S.getFunctionScopes().size()) ||
                        (Scopes.size() == FunctionScopes.size() - 1 && CurCtx &&
                         CurCtx->isRecord())) {
      DumpScopes();
    }
    ERICWF_FANCY_ASSERT(Scopes.size() >= ContractScopes.size()) {
      DumpScopes();
    }
    SmallVector<ScopeEntry> Result{Scopes.rbegin(), Scopes.rend()};
    Scopes = std::move(Result);
    return Result;
  }

  const Sema &S;
  const DeclContext *CurCtx;

  SmallVector<FunctionScopeInfo *, 4> FunctionScopes;
  unsigned FunctionScopeIndex;

  SmallVector<const ContractScopeRecord*> ContractScopes;
  unsigned ContractScopeIndex;

  SmallVector<ScopeEntry> Scopes;
};

SmallVector<ScopeEntry> getScopeEntries(const Sema &S) {
  ScopeWalker Walker(S);
  return Walker.doIt();
}

SmallVector<ScopeEntry> getInterveningScopeEntries(const Sema &S,
                                                   const ValueDecl *Var) {
  VarDecl *VD = const_cast<VarDecl *>(dyn_cast<VarDecl>(Var));
  if (!VD)
    return {};
  if (!VD->isLocalVarDeclOrParm())
    return {};

  auto *OldVD = VD;
  VD = VD->getCanonicalDecl();

  SmallVector<ScopeEntry> Result = getScopeEntries(S);
  if (Result.empty())
    return Result;

  const DeclContext *VarCtx = VD->getDeclContext();
  ERICWF_FANCY_ASSERT(VarCtx && VarCtx->isFunctionOrMethod()) {
    EricWFDump(VarCtx);
  }

  ArrayRef ScopeEntries = Result;
  assert(std::any_of(Result.begin(), Result.end(),
                     [&](ScopeEntry Ent) { return Ent.Ctx->Equals(VarCtx); }));

  while (!ScopeEntries.empty() && !VarCtx->Equals(ScopeEntries.front().Ctx)) {
    ScopeEntries = ScopeEntries.drop_front(1);
  }

  std::optional<unsigned> LastCopyCaptureIdx;
  unsigned Idx = 0;
  for (auto Pos = ScopeEntries.begin(); Pos != ScopeEntries.end();
       ++Pos, ++Idx) {

    assert(VarCtx->Encloses(Pos->Ctx));
    auto *CSI = dyn_cast<CapturingScopeInfo>(Pos->FSI);
    if (!CSI)
      continue;
    assert(!CSI->isCaptured(OldVD));
    if (CSI && CSI->isCaptured(VD)) {
      Capture C = CSI->getCapture(VD);
      if (C.isCopyCapture()) {
        LastCopyCaptureIdx = Idx;
      }
    }
  }
  if (LastCopyCaptureIdx) {
    ScopeEntries = ScopeEntries.drop_front(LastCopyCaptureIdx.value() + 1);
  }
  return SmallVector<ScopeEntry>(ScopeEntries.begin(), ScopeEntries.end());
}

const ContractScopeRecord *getInterveningContractEntry(const Sema &S,
                                                       const ValueDecl *VD) {
  auto Entries = getInterveningScopeEntries(S, VD);
  for (auto &Ent : Entries) {
    if (Ent.CSR)
      return Ent.CSR;
  }
  return nullptr;
}

bool Sema::CheckEquivalentContractSequence(FunctionDecl *OrigDecl,
                                           FunctionDecl *NewDecl) {
  // If the new declaration doesn't contain any contracts, that's fine, they can
  // be omitted.
  if (!NewDecl->hasContracts())
    return false;

  assert(!NewDecl->getContracts()->isInvalidDecl());
  if (OrigDecl->hasContracts() && OrigDecl->getContracts()->isInvalidDecl()) {
    NewDecl->getContracts()->setInvalidDecl(true);
    NewDecl->setInvalidDecl(true);
    return true;
  }
  ContractSpecifierDecl *OrigContractSpec = OrigDecl->getContracts();
  ContractSpecifierDecl *NewContractSpec = NewDecl->getContracts();
  ArrayRef<const ContractStmt *> OrigContracts, NewContracts;
  if (OrigContractSpec)
    OrigContracts = OrigContractSpec->contracts();

  NewContracts = NewContractSpec->contracts();

  if (OrigContractSpec && OrigContractSpec->isInvalidDecl()) {
    NewContractSpec->setInvalidDecl(true);
    NewDecl->setInvalidDecl(true);
    return true;
  }

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
  ContractStmt *CurrentContract = nullptr;
  ContractSpecifierDecl *CSD = nullptr;

public:
  ParamReferenceChecker(Sema &S, const FunctionDecl *FD)
      : Actions(S), FD(FD), CSD(FD->getContracts()) {
    assert(CSD);
  }

  bool TraverseContractStmt(ContractStmt *CS) {
    ValueRAII<ContractStmt *> SetCurrent(CurrentContract, CS,
                                         CurrentContract == nullptr);
    if (CS->getCond())
      TraverseStmt(CS->getCond());
    return true;
  }

  enum DiagSelector {
    DS_None = -1,
    DS_Array = 0,
    DS_Function = 1,
    DS_NotConst = 2
  };

  DiagSelector classifyDiagnosableParmVar(const ParmVarDecl *PVD,
                                          const DeclRefExpr *Usage) const {
    // We only care about parameter's for the function with the contracts we're
    // evaluating. Ensure this parameter belongs to that function.
    if (![&] {
          if (PVD->getFunctionScopeIndex() < FD->getNumParams())
            return FD->getParamDecl(PVD->getFunctionScopeIndex()) == PVD;
          return false;
        }())
      return DS_None;

    // We'll diagnose this when it's non-dependent
    if (PVD->getType()->isDependentType())
      return DS_None;

    // Skip diagnosing this parameter if we've already done it.
    if (DiagnosedDecls.count(PVD))
      return DS_None;

    // [dcl.contract.func] p2900r8 --
    //   If a  postcondition  odr-uses ([basic.def.odr]) a non-reference
    //   parameter...
    if (PVD->getType()->isReferenceType() || Usage->isNonOdrUse())
      return DS_None;

    QualType PVDType = PVD->getOriginalType();

    // that parameter shall not have an array type...
    if (PVDType->isArrayType() || PVDType->isArrayParameterType())
      return DS_Array;
    assert(!PVD->getOriginalType()->isArrayType());

    // or function type...
    if (PVDType->isFunctionPointerType())
      return DS_Function;

    // ...and that parameter shall be declared const
    if (!PVDType.isConstQualified())
      return DS_NotConst;

    return DS_None;
  }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    assert(CurrentContract && "No current contract to use in diagnostics?");

    // [dcl.contract.func] p2900r8 --
    //   If a  postcondition  odr-uses ([basic.def.odr]) a non-reference
    //   parameter, ... that parameter shall be declared const and shall not
    //   have array or function type. [ Note: This requirement applies even to
    //   declarations that do not specify the postcondition-specifier.]
    auto *PVD = dyn_cast_or_null<ParmVarDecl>(E->getDecl());
    if (!PVD || PVD->getType()->isDependentType() || DiagnosedDecls.count(PVD))
      return true;

    if (DiagSelector DiagSelect = classifyDiagnosableParmVar(PVD, E);
        DiagSelect != DS_None) {
      DiagnosedDecls.insert(PVD);
      CSD->setInvalidDecl(true);

      Actions.Diag(E->getLocation(),
                   diag::err_contract_postcondition_parameter_type_invalid)
          << PVD->getIdentifier() << DiagSelect
          << CurrentContract->getSourceRange();
      Actions.Diag(PVD->getTypeSpecStartLoc(), diag::note_parameter_type)
          << (DiagSelect == DS_NotConst ? PVD->getType()
                                        : PVD->getOriginalType())
          << PVD->getSourceRange();
    }

    return true;
  }

private:
  DenseSet<ParmVarDecl *> DiagnosedDecls;
};

} // namespace

static void diagnoseParamTypes(Sema &S, FunctionDecl *FD,
                               ContractSpecifierDecl *CSD) {

  // Check for post-conditions that reference non-const parameters.
  ParamReferenceChecker Checker(S, FD);
  for (auto *CS : CSD->postconditions()) {
    Checker.TraverseContractStmt(CS);
    // FIXME(EricWF): DIagnose non-const function param types.
  }
}

void Sema::CheckFunctionContracts(FunctionDecl *FD, bool IsDefinition, bool IsInstantiation) {
  assert(FD && FD->hasContracts());

  diagnoseParamTypes(*this, FD, FD->getContracts());
}

void Sema::InstantiateContractSpecifier(
    SourceLocation PointOfInstantiation, FunctionDecl *Instantiation,
    const FunctionDecl *Pattern,
    const MultiLevelTemplateArgumentList &TemplateArgs) {

  ContractSpecifierDecl *PatternCSD = Pattern->getContracts();
  if (!PatternCSD)
    return;
  bool IsInvalid = false;

  auto *Method = const_cast<CXXMethodDecl *>(
      dyn_cast_if_present<CXXMethodDecl>(Instantiation));

  Sema::ContextRAII SavedContext(*this, Instantiation);
  Sema::CXXThisScopeRAII ThisxScope(
      SemaRef, Method ? Method->getParent() : nullptr,
      Method ? Method->getMethodQualifiers().withConst() : Qualifiers{},
      Method != nullptr);

  LocalInstantiationScope Scope(*this, true);

  SmallVector<ContractStmt *> NewContracts;
  for (auto *CS : PatternCSD->contracts()) {
    StmtResult NewStmt = SubstStmt(CS, TemplateArgs);
    if (NewStmt.isInvalid())
      IsInvalid = true;
    else
      NewContracts.push_back(NewStmt.getAs<ContractStmt>());
  }

  ContractSpecifierDecl *NewCSD = BuildContractSpecifierDecl(
      NewContracts, Instantiation, PatternCSD->getLocation(), IsInvalid);
  assert(NewCSD);

  Instantiation->setContracts(NewCSD);
  if (!Instantiation->isDependentContext())
    CheckFunctionContracts(Instantiation, /*IsDefinition=*/false, /*IsInstantiation=*/true);
}

ContractSpecifierDecl *
Sema::BuildContractSpecifierDecl(ArrayRef<ContractStmt *> Contracts,
                                 DeclContext *DC, SourceLocation Loc,
                                 bool IsInvalid) {
  ContractSpecifierDecl *CSD =
      ContractSpecifierDecl::Create(Context, DC, Loc, Contracts, IsInvalid);
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

  return BuildContractSpecifierDecl(Contracts, CurContext, Loc, IsInvalid);
}

void Sema::ActOnContractsOnFinishFunctionDecl(FunctionDecl *D,
                                              bool IsDefinition) {
  FunctionDecl *FD;
  bool WasTemplate = false;
  if (FunctionTemplateDecl *FunTmpl = dyn_cast<FunctionTemplateDecl>(D)) {
    WasTemplate = true;
    ((void)WasTemplate);
    FD = FunTmpl->getTemplatedDecl();
  } else
    FD = D;
  if (D != FD && FD->getContracts() == D->getContracts())
    D->setContracts(nullptr);

  if (!FD->hasContracts() && !FD->getFirstDecl()->hasContracts())
    return;

  auto *First = FD->getFirstDecl();

  // If the definition has omitted the contracts, but the first declaration has
  // them, we need to rebuild the contracts to refer to the parameters of the
  // definition.
  //
  // For function templates, we'll create a copy when we instantiate the definition.
  if (First->hasContracts() && !FD->hasContracts() && IsDefinition &&
      !FD->isTemplateInstantiation()) {
    // Note: This case is mutually exclusive with the NonDependentPlaceholders
    // case, since we can't have a placeholder return type on a declaration that
    // isn't a definition.

    ContractSpecifierDecl *NewCSD = RebuildContractSpecifierForDecl(First, FD);
    NewCSD->setOwningFunction(FD);
    FD->setContracts(NewCSD);
  }
  assert(FD->hasContracts());

  ContractSpecifierDecl *CSD = FD->getContracts();

  if (!D->isTemplateInstantiation()) {
    // Note: IsInstantiation here means whether we're calling during the instantiation of the contract specifier,
    // rather than whether the FD declares a function instantiation.
    CheckFunctionContracts(FD, IsDefinition, /*IsInstantiation=*/false);
  }

  // When the declared return type of a non-templated function contains a
  // placeholder type, a postcondition-specifier with a result-name-introducer
  // shall be present only on a definition.
  if (CSD->hasInventedPlaceholdersTypes() && !FD->isTemplateInstantiation() &&
      !FD->isTemplated()) {
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
}

namespace {

// There's got to be a better way to do this.
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
/// the definition. This rebinds parameters and result names as needed.
ContractSpecifierDecl *
Sema::RebuildContractSpecifierForDecl(FunctionDecl *First, FunctionDecl *Def) {

  assert(Def->getContracts() == First->getContracts() || !Def->hasContracts());
  Def->setContracts(nullptr);
  Sema::ContextRAII SavedContext(*this, Def);
  assert(FunctionScopes.empty());

  FunctionScopeRAII SavedFunctionContext(*this);
  PushFunctionScope();
  std::optional<Sema::CXXThisScopeRAII> ThisScope;
  if (auto *CXXMethod = dyn_cast<CXXMethodDecl>(Def)) {
    ThisScope.emplace(*this, CXXMethod->getParent(),
                      CXXMethod->getMethodQualifiers().withConst(),
                      /*IsLambda*/ true);
  }
  RebuildFunctionContracts Rebuilder(*this, true);
  for (unsigned I = 0; I < First->getNumParams(); ++I) {
    Rebuilder.transformedLocalDecl(First->getParamDecl(I),
                                   Def->getParamDecl(I));
  }
  for (auto *RND : First->getContracts()->result_names()) {
    QualType Replacement = Def->getReturnType();
    auto *NewRND =
        ActOnResultNameDeclarator(ContractKind::Post, nullptr, Replacement,
                                  RND->getLocation(), RND->getIdentifier(),
                                  RND->getFunctionScopeDepth());
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

  auto *CSD = BuildContractSpecifierDecl(
      NewContracts, Def, First->getContracts()->getLocation(), IsInvalid);
  Def->setContracts(CSD);

  if (CSD->isInvalidDecl())
    Def->isInvalidDecl();
  return CSD;
}

DeclResult Sema::RebuildContractsWithPlaceholderReturnType(FunctionDecl *FD) {

  ContractSpecifierDecl *CSD = FD->getContracts();
  assert(CSD && CSD->hasInventedPlaceholdersTypes() &&
         "Cannot rebuild contracts without placeholders");
  if (FD->isInvalidDecl()) {
    CSD->setInvalidDecl(true);
    return DeclResult(/*IsInvalid*/ true);
  }

  RebuildFunctionContracts Rebuilder(*this, true);

  QualType Replacement;
  auto CheckFunctionReturnType = [&](SourceLocation Loc) -> bool {
    if (!Replacement.isNull())
      return false;

    if (FD->getReturnType()->isUndeducedType()) {
      assert(!FD->isInvalidDecl());
      assert(!CSD->isInvalidDecl());
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
        return DeclResult(/*IsInvalid*/ true);
      }
    }
    assert(!Replacement.isNull());
    auto *NewRND =
        ActOnResultNameDeclarator(ContractKind::Post, nullptr, Replacement,
                                  RND->getLocation(), RND->getIdentifier(), RND->getFunctionScopeDepth());
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

  auto *NewCSD = BuildContractSpecifierDecl(
      NewContracts, FD, FD->getContracts()->getLocation(), IsInvalid);

  FD->setContracts(NewCSD);
  if (NewCSD->isInvalidDecl())
    FD->isInvalidDecl();

  return NewCSD;
}

struct ContractCapturePair {
  SmallVector<const Capture *> InContract;
  SmallVector<const Capture *> OutOfContract;
};

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

  // If we had a deduced return type on a non-template function, we can now
  // attempt to deduce the return type and rebuild the contracts with the
  // deduced return type.
  if (Def->hasContracts() &&
      Def->getContracts()->hasInventedPlaceholdersTypes()) {
    assert((Def->isThisDeclarationADefinition() ||
            Def->isTemplateInstantiation()) &&
           "Wrong declaration passed?");

    DeclResult Res = RebuildContractsWithPlaceholderReturnType(Def);
    if (Res.isInvalid()) {
      Def->setInvalidDecl(true);
      return;
    }
    auto *NewCSD = Res.getAs<ContractSpecifierDecl>();
    assert(NewCSD);
    NewCSD->setOwningFunction(Def);
    Def->setContracts(NewCSD);
    if (NewCSD->isInvalidDecl())
      Def->setInvalidDecl(true);
  }

  if (const LambdaScopeInfo *LSI =
          dyn_cast<LambdaScopeInfo>(getCurFunction())) {
    llvm::DenseMap<const ValueDecl *, ContractCapturePair> CheckedCaptures;
    for (auto &KV : LSI->ContractCaptureMap) {
      assert(false);
      if (LSI->CaptureMap.contains(KV.first))
        continue;
      const Capture &C = LSI->ContractCaptures[KV.second - 1];
      assert(C.isVariableCapture());

      const ValueDecl *VD = C.getVariable();
      assert(VD && isa<NamedDecl>(VD));
      auto *ND = cast<NamedDecl>(KV.first);
      Diag(C.getLocation(), diag::err_lambda_implicit_capture_in_contracts_only)
          << ND;
      Diag(LSI->CaptureDefaultLoc,
           diag::note_lambda_implicit_capture_in_contracts_only)
          << ND;
      Diag(C.getContractLoc(), diag::note_contract_context);
      Def->isInvalidDecl();
    }
  }
}

bool Sema::isUsageAcrossContract(const ValueDecl *VD) {
  // There's no contract scope anywhere above us.
  if (!getCurrentContractEntry())
    return false;

  // Fast Path: We're in an immediate contract assertion expression evaluation context.
  if (isContractAssertionContext())
    return true;

  if (isa<VarDecl>(VD) && !cast<VarDecl>(VD)->isLocalVarDeclOrParm())
    return false;

  assert(VD);
  return getInterveningContractEntry(*this, VD) != nullptr;
  ;
#if 0
  if (!ContractScopes.empty())
    return true;

  // We're going to walk up from the DeclContext we captured when we entered the contract
  // scope to try and find the declaration context of the specified decl. If we do,
  // then the decl is used across a contract.

  // FIXME(EricWF): Why is this here?
  if (isa<VarDecl>(VD))
    VD = cast<VarDecl>(VD)->getCanonicalDecl();

  const DeclContext *DC = VD->getDeclContext();
  if (!DC->isFunctionOrMethod()) {
    return false;
  }



  assert(getCurrentContractEntry());


  // FIXME(EricWF): This seems expensive?
  // Make sure the ValueDecl has a DeclContext that is the same as or a parent
  // of the most recent contract entry
  auto *StartContext = getCurrentContractEntry()->ContextAtPush;

  while (StartContext) {
    if (StartContext->isFileContext())
      break;
    if (StartContext->Equals(VD->getDeclContext()))
      return true;
    StartContext = StartContext->getParent();
  }
  return false;
#endif
}

/// [basic.contract.general]
/// Within the predicate of a contract assertion, id-expressions referring to
/// variables with automatic storage duration are const ([expr.prim.id.unqual])
ContractConstification Sema::getContractConstification(const ValueDecl *VD) {
  //WalkUpContractScopesTest();
  auto &S = *this;
  assert(VD);
  const ContractScopeRecord *CSR = S.getCurrentContractEntry();

  if (!CSR || CSR->ContextAtPush->Encloses(VD->getDeclContext()))
    return CC_None;


  // If there is no contract scope that encloses the current context, then we don't need to constify the variable.
  if (getLastEnclosingContractScopeForContext(CurContext) == nullptr)
    return CC_None;

  CSR = getLastEnclosingContractScopeForContext(CurContext);
  if (CSR == nullptr)
    return CC_None;

  if (VD->getDeclContext()->Encloses(CSR->ContextAtPush)) {
    assert(!VD->getDeclContext()->Equals(CSR->ContextAtPush));
  }

  // Make sure that there's a contract scope interviening between the current
  // context and the declaration of the variable. If there isn't, we don't need
  // to constify the variable.
  if (!isUsageAcrossContract(VD))
    return CC_None;

  // if the unqualified-id appears in the predicate of a contract assertion
  //  ([basic.contract]) and the entity is
  // ...

  // — the result object of (possibly deduced, see [dcl.spec.auto]) type T of a
  // function
  //  call and the unqualified-id is the result name ([dcl.contract.res]) in a
  //  postcondition assertion,
  if (isa<ResultNameDecl>(VD))
    return CC_ApplyConst;

  // — a structured binding of type T whose corresponding variable has automatic
  // storage
  //  duration, or
  if (auto *Bound = dyn_cast<BindingDecl>(VD)) {
    if (!Bound->getHoldingVar())
      return CC_None;
    auto Var = Bound->getHoldingVar();
    if (Var->isLocalVarDeclOrParm() &&
        (Var->getStorageDuration() == SD_Automatic ||
         Var->getKind() == Decl::ParmVar))
      return CC_ApplyConst;
    return CC_None;
  }

  // — a variable with automatic storage duration ...
  if (auto Var = dyn_cast<VarDecl>(VD);
      Var && Var->isLocalVarDeclOrParm() &&
      (Var->getStorageDuration() == SD_Automatic ||
       Var->getKind() == Decl::ParmVar)) {
    // ... of object type T, or
    if (Var->getType()->isObjectType())
      return CC_ApplyConst;

    // of type 'reference to T'
    if (Var->getType()->isReferenceType() &&
        Var->getType().getNonReferenceType()->isObjectType())
      return CC_ApplyConst;
  }

  return CC_None;
}

static const DeclContext* walkUpDeclContextToFunction(const DeclContext *DC, bool AllowLambda = false) {
  while (true) {
    assert(DC);
    if (isa<BlockDecl>(DC) || isa<EnumDecl>(DC) || isa<CapturedDecl>(DC) ||
        isa<RequiresExprBodyDecl>(DC) || isa<LinkageSpecDecl>(DC) ||
        (isa<CXXRecordDecl>(DC) && cast<CXXRecordDecl>(DC)->isLambda())) {
      DC = DC->getParent();
    } else if (!AllowLambda && isa<CXXMethodDecl>(DC) &&
               cast<CXXMethodDecl>(DC)->getOverloadedOperator() == OO_Call &&
               cast<CXXRecordDecl>(DC->getParent())->isLambda()) {
      DC = DC->getParent()->getParent();
    } else
      break;
  }
  if (DC) {
    if (!DC->isFunctionOrMethod()) {
      assert(!DC->getParent() || !DC->getParent()->isFunctionOrMethod());
      assert(!DC->getLexicalParent() ||
             !DC->getLexicalParent()->isFunctionOrMethod());
    }
  } else {
    llvm::errs() << "Found Null DC";
  }
  if (DC && !DC->isFunctionOrMethod())
    return nullptr;
  return DC;
}

QualType Sema::adjustCXXThisTypeForContracts(QualType QT) {
  if (!getCurrentContractEntry())
    return QT;

  // 'this' is constified any time the `this` object that is captured by a lambda which exists fully
  // within a contract.
  //
  // We need to ensure that we haven't entered a nested member function context, because in that case we
  // don't want to constify the `this` object.
  // For example:
  // ```
  // struct A {
  //   void f() {
  //     [&]() {
  //        contract_assert([&] {
  //           struct B { int x; void f() { ++x; } };
  //            return true;
  //          }());
  //      }();
  //   }};
  // ```
  const DeclContext *ContractContext =
      walkUpDeclContextToFunction(getCurrentContractEntry()->ContextAtPush);
  if (!ContractContext)
    return QT;
  const DeclContext *QTContext = walkUpDeclContextToFunction(CurContext );
  if (!QTContext)
    return QT;
  if (!ContractContext->Equals(QTContext))
    return QT;

  return Context.getPointerType(QT->getPointeeType().withConst());
}

namespace {

struct CaptureUsage {
  clang::LambdaCapture Capture;
  const ContractStmt *UsedInContract = nullptr;
  const Expr *UsageExpr = nullptr;
  SourceLocation UsageLoc = SourceLocation();

  CaptureUsage(clang::LambdaCapture Capture) : Capture(Capture) {}
};

/// TODO(EricWF): Remove this and do it inline instead. We currently
/// do this as a RecursiveASTVisitor because the changes to do it during
/// the initial parsing pass are too invasive to do dur
struct LambdaCaptureChecker : RecursiveASTVisitor<LambdaCaptureChecker> {
  typedef RecursiveASTVisitor<LambdaCaptureChecker> super;

  Sema &Actions;
  const LambdaExpr *const CurLambda = nullptr;
  const ContractStmt *CurContract = nullptr;


  // Note: The value `nullptr` is used to denote a capture of CXXThis.
  DenseMap<const ValueDecl *, CaptureUsage> Captures;


  void observeUsage(const ValueDecl *VD, const Expr *E, SourceLocation Loc) {
    if (auto Pos = Captures.find(VD); Pos != Captures.end()) {
      if (!CurContract)
        Captures.erase(VD);
      else {
        auto& Usage = Pos->second;
        if (Usage.UsageExpr == nullptr || (isa<LambdaExpr>(Usage.UsageExpr) && !isa<LambdaExpr>(E))) {
          Usage.UsedInContract = CurContract;
          Usage.UsageExpr = E;
          Usage.UsageLoc = Loc;
        }
      }
    }
  }

private:
  LambdaCaptureChecker(Sema &S, LambdaExpr *LE) : Actions(S), CurLambda(LE) { Init(); }


  void Run() {
    // Traverse the lambdas function-level contracts and body to find the bad captures.
    FunctionDecl *FD = CurLambda->getCallOperator();
    assert(FD->getBody());
    if (FD->hasContracts())
      TraverseDecl(FD->getContracts());
    TraverseStmt(CurLambda->getBody());

    // Finally, diagnose any captures that still remain, since they do not have any non-contrac
    // usages.
    for (auto& [Var, Bad] : Captures) {
      // We likely didn't see the usage because there was a intervening lambda that captured by copy.
      if (Bad.UsedInContract == nullptr)
        continue;
      Actions.Diag(CurLambda->getCaptureDefaultLoc(),
                   diag::err_lambda_implicit_capture_in_contracts_only)
          << (int)Bad.Capture.capturesThis() << cast_or_null<NamedDecl>(Var);
      SourceLocation UsageLoc = Bad.UsageLoc;
      Actions.Diag(UsageLoc, diag::note_lambda_implicit_capture_in_contract_usage)
            << (int)Bad.Capture.capturesThis() << cast_or_null<NamedDecl>(Var);
      if (Bad.UsedInContract)
        Actions.Diag(Bad.UsedInContract->getBeginLoc(),
                    diag::note_contract_context);

    }
  }

  void Init() {
    // Collect all of the implicit captures of the lambda.
    // If the lambda capture hasn't been removed after traversing the tree then
    // that lambda capture is bad, and must be diagnosed as only being used inside
    // of a contract.
    for (auto C : CurLambda->captures()) {
      if (C.capturesThis() && C.isImplicit())
        Captures.insert({nullptr, {C}});
      if (!C.capturesVariable())
        continue;
      if (C.isExplicit())
        continue;
      // EricWFDump("Inserting Capture ", C.getCapturedVar(), &Actions.Context);
      Captures.insert({C.getCapturedVar(), {C}});
    }
  }

public:
  bool shouldVisitLambdaBody() const { return false; }

  bool TraverseContractStmt(ContractStmt *CS) {
    assert(CS->getCond());
    const ContractStmt *Prev = CurContract;
    CurContract = CS;
    TraverseStmt(CS->getCond());
    CurContract = Prev;
    return true;
  }

  bool TraverseLambdaCapture(LambdaExpr *LE, const LambdaCapture *C,
                             Expr *Init) {
    return true;
  }

  bool TraverseLambdaExpr(LambdaExpr *LE) {
    assert(LE != CurLambda && "Revisiting the root lambda?");
    // Iterate over the captures of the nested lambda, and mark any of our captures as having been seen
    // outside of a contract. This assumes that the inner lambda has a valid usage of the capture.
    // If it doesn't, we'll diagnose that separately.
    for (auto C : LE->captures()) {
      assert(C.capturesThis() || C.capturesVariable());
      observeUsage(C.capturesThis() ? nullptr : C.getCapturedVar(), LE, C.getLocation());
    }
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    if (auto *VD = dyn_cast<ValueDecl>(E->getDecl()); VD && E->isNonOdrUse() != NOUR_Unevaluated)
      observeUsage(VD, E, E->getExprLoc());
    return true;
  }

  bool VisitMemberExpr(MemberExpr *E) {
    if (E->isNonOdrUse() != NOUR_Unevaluated)
      observeUsage(nullptr, E, E->getExprLoc());
    return true;
  }

  bool VisitCXXThisExpr(CXXThisExpr *E) {
    observeUsage(nullptr, E, E->getExprLoc());
    return true;
  }

public:
  static void Check(Sema &Actions, LambdaExpr *LE) {
    LambdaCaptureChecker Checker(Actions, LE);
    Checker.Run();
  }
};

} // namespace

void Sema::CheckLambdaCapturesForContracts(LambdaExpr *LE) {
  // Check the contracts on the function declaration.
  LambdaCaptureChecker::Check(*this, LE);
}

std::optional<unsigned>
Sema::getFunctionScopeIndexForDeclaration(const ValueDecl *VD) {
  const DeclContext *Ctx = CurContext;
  assert(CurContext->isFunctionOrMethod());
  if (FunctionScopes.size() == 0)
    return std::nullopt;
  if (auto *PVD = dyn_cast<ParmVarDecl>(VD)) {
    assert(PVD->getDeclContext()->isFunctionOrMethod());
  }

  const DeclContext *const VarCtx = VD->getDeclContext();
  if (!VarCtx->Encloses(Ctx)) {
    assert(!VarCtx->Equals(Ctx));
    return std::nullopt;
  }
  unsigned StartScope = FunctionScopes.size() - 1;
  while (Ctx && !Ctx->Equals(VarCtx)) {
    assert(StartScope > 0);
    Ctx = walkUpDeclContextToFunction(Ctx, /*AllowLambda=*/true);
    --StartScope;
  }
  if (!Ctx)
    return std::nullopt;

  auto TestDC = getDeclContextForFunctionScopeIndex(StartScope);
  assert(TestDC && TestDC->Equals(Ctx));
  return StartScope;
}

const DeclContext *
Sema::getDeclContextForFunctionScopeIndex(unsigned ScopeIndex) {
  const DeclContext *Ctx = CurContext;
  assert(ScopeIndex < FunctionScopes.size());
  unsigned ScopesToGo = FunctionScopes.size() - ScopeIndex - 1;
  assert(Ctx->isFunctionOrMethod());
  if (!Ctx->isFunctionOrMethod())
    Ctx = walkUpDeclContextToFunction(Ctx, /*AllowLambda=*/true);
  assert(Ctx);
  while (ScopesToGo > 0) {
    --ScopesToGo;
    Ctx = walkUpDeclContextToFunction(Ctx, /*AllowLambda=*/true);
    assert(Ctx && Ctx->isFunctionOrMethod());
  }

  return Ctx;
}

bool Sema::isContractAssertionContext() const {
  auto *CR = getCurrentContractEntry();
  if (!CR)
    return false;

  return CR->ContextAtPush->Equals(CurContext);

#if 0
  return getCurrentContractEntry() && (getCurrentContractEntry()->FunctionScopeAtPush == getCurFunction() ||
      (getCurrentContractEntry()));
  if (FunctionScopes.empty()) {
    auto *CR = getCurrentContractEntry();
    return CR != nullptr;
    return ExprEvalContexts.back().isContractAssertionContext() ||
           (getCurrentContractEntry() &&
            getCurrentContractEntry()->HadNoFunctionScope);

  }
  return FunctionScopes.back()->InContract;
#endif
}

ArrayRef<Sema::ContractScopeRecord> Sema::getAllContractScopes() const {
  return llvm::ArrayRef(ContractScopeStack.begin(), ContractScopeStack.end());
}
ArrayRef<Sema::ContractScopeRecord> Sema::getContractScopes() const {
  return getAllContractScopes();
#if 0
  unsigned Offset = 0;
  for (auto Pos = ContractScopeStack.begin(); Pos != ContractScopeStack.end();
       ++Pos) {
    if (Pos->FunctionScopeStartAtPush == FunctionScopesStart)
      break;
    ++Offset;
  }
  return llvm::ArrayRef(ContractScopeStack.begin() + Offset,
                        ContractScopeStack.end());
#endif
}

ArrayRef<Sema::ContractScopeRecord>
Sema::getInterveningContractScopes(const ValueDecl *ValueD) const {
  assert(ValueD);

  auto *VD = dyn_cast<VarDecl>(ValueD);
  if (!VD)
    return std::nullopt;

  if (!VD->isLocalVarDeclOrParm())
    return std::nullopt;

  auto CScopes = [](auto CL) -> SmallVector<const ContractScopeRecord *> {
    SmallVector<const ContractScopeRecord*> Out;
    for (auto & CS : CL) {
      Out.push_back(&CS);
    }
    return Out;
  }(getContractScopes());
  if (CScopes.empty())
    return std::nullopt;


  VD = VD->getCanonicalDecl();
  assert(VD && VD->getDeclContext());
  const DeclContext *VarCtx = VD->getDeclContext();
  assert(VarCtx);
  auto Pos = CScopes.end();
  auto LastPos = CScopes.end();


  auto ReturnRef = [&](auto Start) -> ArrayRef<ContractScopeRecord> {
    if (Start == CScopes.end())
      return std::nullopt;
    unsigned StartIdx = (*Start)->Index;
    return llvm::ArrayRef(ContractScopeStack.begin() + StartIdx, ContractScopeStack.end());

  };

  while (Pos != CScopes.begin()) {
    --Pos;
    const ContractScopeRecord *CS = *Pos;
    assert(CS->ContextAtPush);
    if (!CS->ContextAtPush->isFunctionOrMethod()) {
      llvm::errs() << "Had non-function context\n";
      EricWFDump(CS->ContextAtPush);
      return ReturnRef(Pos);
    }

    auto CCtx = (*Pos)->ContextAtPush;
    if (VarCtx->Encloses(CCtx) || VarCtx->Equals(CCtx))
      LastPos = Pos;
    else
      break;
  }
  return ReturnRef(LastPos);
}

SmallVector<sema::FunctionScopeInfo *>
Sema::getInterveningFunctionScopesForContracts(const ValueDecl *ValueD) const {
  return {};
}

const DeclContext *
Sema::ContractScopeRecord::getFunctionContext(bool AllowLambda) const {
  if (!ContextAtPush->isFunctionOrMethod() ||
      (isLambdaCallOperator(ContextAtPush) && !AllowLambda))
    return walkUpDeclContextToFunction(ContextAtPush, AllowLambda);
  return ContextAtPush;
}

const Sema::ContractScopeRecord *Sema::getCurrentContractEntry() const {
  auto EntryList = getContractScopes();
  if (EntryList.empty())
    return nullptr;
  return &EntryList.back();
}

void Sema::WalkUpContractScopesTest() const {

  if (ContractScopeStack.empty())
    return;
  if (FunctionScopes.empty())
    return;
  ScopeWalker Walker(*this);
  auto Scopes = Walker.doIt();
  ((void)Scopes);
}


Sema::ContractScopeRAII::ContractScopeRAII(Sema &S, ContractKind CK, ContractScopeOffset ScopeOffset, SourceLocation Loc)
    : S(S) {
  S.PushContractScope(CK, ScopeOffset, Loc);
}

Sema::ContractScopeRAII::~ContractScopeRAII() {
  S.PopContractScope();
}


void Sema::PushContractScope(ContractKind Kind, ContractScopeOffset ScopeOffset, SourceLocation Loc) {
//  assert(!FunctionScopes.empty());

  ContractScopeRecord Record{
         .Index = static_cast<unsigned>(ContractScopeStack.size()),
         .Kind = Kind,
         .ScopeOffset = ScopeOffset,
         .KeywordLoc = Loc,
         .ContextAtPush = CurContext,
         .PreviousCXXThisType = CXXThisTypeOverride,
         .FunctionIndex = static_cast<unsigned>(
             FunctionScopes.empty() ? 0ul : FunctionScopes.size() - 1),
         .StartFunctionIndex = FunctionScopesStart,
         .FunctionScopeAtPush = getCurFunction(),
         .AddedConstToCXXThis = false,
         .WasInContractContext = ExprEvalContexts.back().InContractAssertion,
         .HadNoFunctionScope = FunctionScopes.empty(),
         .FunctionScopeStartAtPush = FunctionScopesStart};

    // Setup the constification context when building declref expressions.
    ExprEvalContexts.back().InContractAssertion = true;
  assert(CurContext);

  assert(ContractScopeIndexMap.find(CurContext) == ContractScopeIndexMap.end());

    ContractScopeIndexMap[CurContext] = Record.Index;
    // P2900R8 [expr.prim.this]p2
    //   If the expression 'this' appears ... in a contract assertion
    //     (including as the result of the implicit transformation in the body of
    //     a non-static member function and including in the bodies of nested
    //     lambda-expressions),
    // ...
    //  const is combined with the cv-qualifier-seq used to generate the resulting
    //  type (see below
    if (!CXXThisTypeOverride.isNull()) {
      assert(CXXThisTypeOverride->isPointerType());
      QualType ClassType = CXXThisTypeOverride->getPointeeType();
      if (not ClassType.isConstQualified()) {
        // If the 'this' object is const-qualified, we need to remove the
        // const-qualification for the contract check.
        ClassType.addConst();
        Record.AddedConstToCXXThis = true;
        CXXThisTypeOverride = Context.getPointerType(ClassType);
      }
    }

    //assert(!S.FunctionScopes.empty());


  if (Record.FunctionScopeAtPush) {
    auto *LastScope = Record.FunctionScopeAtPush;
    assert(LastScope && !LastScope->isInContract());

    LastScope->ContractScopeIndex = Record.Index;
  }

    ContractScopeStack.push_back(Record);
}

ContractScopeRecord Sema::PopContractScope() {
  assert(!ContractScopeStack.empty());

  auto Record = ContractScopeStack.back();
  ContractScopeStack.pop_back();

  assert(ContractScopeIndexMap.contains(Record.ContextAtPush) && ContractScopeIndexMap[Record.ContextAtPush]  == Record.Index);
  ContractScopeIndexMap.erase(Record.ContextAtPush);


  assert(ExprEvalContexts.back().InContractAssertion == true);
  ExprEvalContexts.back().InContractAssertion = Record.WasInContractContext;
  CXXThisTypeOverride = Record.PreviousCXXThisType;


  if (Record.FunctionScopeAtPush) {

    if (FunctionScopes.back() == Record.FunctionScopeAtPush) {
      assert(FunctionScopes.back()->ContractScopeIndex != unsigned(-1));
      FunctionScopes.back()->ContractScopeIndex = -1;
    } else {
      assert(getFunctionScopes().empty() || FunctionScopes.size() < Record.FunctionIndex);
    }
  }

  return Record;
}

const ContractScopeRecord *Sema::getContractScopeForContext(const DeclContext *DC) const {
  auto Pos = ContractScopeIndexMap.find(DC);
  if (Pos == ContractScopeIndexMap.end())
    return nullptr;
  unsigned Idx = Pos->second;
  assert(Idx < ContractScopeStack.size());
  return &ContractScopeStack[Idx];
}

const ContractScopeRecord *Sema::getFirstEnclosingContractScopeForContext(const DeclContext *DC) const {
  for (unsigned I=0; I < ContractScopeStack.size(); ++I) {
    if (ContractScopeStack[I].ContextAtPush->Encloses(DC)) {
      return &ContractScopeStack[I];
    }
  }
  return nullptr;

}


const ContractScopeRecord *Sema::getLastEnclosingContractScopeForContext(const DeclContext *DC) const {
  unsigned LastEnclosingIdx = unsigned(-1);
  for (unsigned I=0; I < ContractScopeStack.size(); ++I) {
    if (ContractScopeStack[I].ContextAtPush->Encloses(DC)) {
      LastEnclosingIdx = I;
    }
  }
  if (LastEnclosingIdx == unsigned(-1))
    return nullptr;
  assert(ContractScopeStack.size() > LastEnclosingIdx);
  return &ContractScopeStack[LastEnclosingIdx];

}


const ContractScopeRecord *Sema::getFirstEnclosedContractScopeForContext(const DeclContext *DC) const {
  for (unsigned I=0; I < ContractScopeStack.size(); ++I) {
    if (DC->Encloses(ContractScopeStack[I].ContextAtPush) || DC->Equals(ContractScopeStack[I].ContextAtPush))
      return &ContractScopeStack[I];
  }
  return nullptr;
}


const ContractScopeRecord *Sema::getLastEnclosedContractScopeForContext(const DeclContext *DC) const {
  unsigned LastIdx = unsigned(-1);
  for (unsigned I=0; I < ContractScopeStack.size(); ++I) {
    if (DC->Encloses(ContractScopeStack[I].ContextAtPush) || DC->Equals(ContractScopeStack[I].ContextAtPush)) {
      if (I > LastIdx || LastIdx == unsigned(-1))
        LastIdx = I;
    }
  }
  if (LastIdx == unsigned(-1))
    return nullptr;
  assert(LastIdx < ContractScopeStack.size());
  return &ContractScopeStack[LastIdx];

}

SourceLocation Sema::getContractLocForFunctionScope(const sema::FunctionScopeInfo *FSI) const {
  assert(FSI->isInContract());
  assert(FSI->ContractScopeIndex < ContractScopeStack.size());
  return ContractScopeStack[FSI->ContractScopeIndex].KeywordLoc;
}