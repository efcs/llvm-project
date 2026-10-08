//===--- CGContracts.cpp - Emit LLVM Code for C++ contracts ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit C++26 contract assertions.
//
// A contract assertion with a checking semantic evaluates its predicate and, if
// the predicate is false or its evaluation exits via an exception, reports a
// contract violation to the runtime library through the Itanium C++ Contracts
// ABI (https://github.com/notadragon/efcs_contracts-abi-specification):
//
//  * The violation data is a static constant "data block" emitted once per
//    assertion. It starts with a pointer to a descriptor table, which lists
//    the fields the block contains (as field IDs and byte offsets), and a
//    pointer to the next block of a chain (null here). Clang emits the
//    "basic" layout: the source location, the predicate text ("comment") and
//    the diagnostic message, all described by a single descriptor table that
//    is shared by every data block of the translation unit.
//
//  * The violation is reported by passing the address of the data block to a
//    runtime entry point whose name encodes the assertion kind, the evaluation
//    semantic and the detection mode:
//      __cxa_contract_violation_{pre,post,assert}_{observe,enforce}_{pf,ex}
//    The '_noexcept' variants are used where an exception thrown by the
//    violation handler could not propagate anyway; they do not unwind and
//    terminate if the handler throws. The 'enforce' entry points never return.
//    'quick_enforce' assertions trap instead of calling the runtime, and
//    'ignore' assertions emit nothing.
//
//  * When the predicate exits via an exception, the entry point is called from
//    within a handler for that exception, so that the runtime and the
//    violation handler can observe it via std::current_exception().
//
//  * The runtime calls the user-replaceable handler ::handle_contract_violation
//    through the C-linkage symbol __handle_contract_violation, which Clang
//    emits as an alias of the handler in the translation unit defining it.
//
//===----------------------------------------------------------------------===//

#include "CGCXXABI.h"
#include "CGCleanup.h"
#include "CGDebugInfo.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "CodeGenPGO.h"
#include "clang/AST/ASTLambda.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Expr.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Basic/SourceManager.h"
#include "clang/CodeGen/ConstantInitBuilder.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include <optional>

using namespace clang;
using namespace CodeGen;

constexpr ContractEvaluationSemantic Enforce =
    ContractEvaluationSemantic::Enforce;
constexpr ContractEvaluationSemantic QuickEnforce =
    ContractEvaluationSemantic::QuickEnforce;
constexpr ContractEvaluationSemantic Observe =
    ContractEvaluationSemantic::Observe;
constexpr ContractEvaluationSemantic Ignore =
    ContractEvaluationSemantic::Ignore;

constexpr ContractDetectionMode PredicateFailed =
    ContractDetectionMode::PredicateFailed;
constexpr ContractDetectionMode ExceptionRaised =
    ContractDetectionMode::ExceptionRaised;

namespace {

/// Constants of the Itanium C++ Contracts ABI data format.
namespace cxa {
enum : uint8_t {
  /// The descriptor table version this implementation emits.
  TableVersion = 1,
  /// The vendor ID of Clang (CXA_VENDOR_CLANG).
  VendorClang = 0x2,

  /// Field IDs of the standard fields emitted in the basic data block layout.
  FieldSourceLocation = 0x01, // __cxa_source_location
  FieldComment = 0x02,        // const char*: the predicate text
  FieldMessage = 0x03,        // const char*: the diagnostic message
};

/// The name of the C-linkage symbol through which the runtime calls the
/// replaceable contract-violation handler ::handle_contract_violation.
constexpr const char *HandlerSymbolName = "__handle_contract_violation";
} // namespace cxa

} // namespace

namespace clang::CodeGen {

struct CurrentContractInfo {
  const ContractStmt *Contract;
  ContractEvaluationSemantic Semantic;
};

struct SharedTrapBlock {
  static SharedTrapBlock Create(CodeGenFunction &CGF) {
    auto &Builder = CGF.Builder;
    CGBuilderTy::InsertPoint SavedIP = Builder.saveAndClearIP();
    // This block is inserted at the very end of the function by
    // FinishFunction.
    SharedTrapBlock This;
    This.Block = CGF.createBasicBlock("contract.violation.trap.handler");
    Builder.SetInsertPoint(This.Block);
    CGF.EmitContractViolationTrap();
    Builder.restoreIP(SavedIP);
    return This;
  }

  llvm::BasicBlock *Block = nullptr;

private:
  SharedTrapBlock() = default;
};

struct CGContractData {
  std::optional<SharedTrapBlock> TrapBlock;
  std::optional<CurrentContractInfo> CurContract;

  SharedTrapBlock &GetSharedTrapBlock(CodeGenFunction &CGF) {
    if (!TrapBlock)
      TrapBlock = SharedTrapBlock::Create(CGF);
    return *TrapBlock;
  }
};

} // namespace clang::CodeGen

CurrentContractInfo *CodeGenFunction::CurContract() {
  return ContractData->CurContract ? &ContractData->CurContract.value()
                                   : nullptr;
}

namespace {
struct CurrentContractRAII {
  CurrentContractRAII(CodeGenFunction &CGF, CurrentContractInfo CurContract)
      : CGF(CGF) {
    assert(!CGF.ContractData->CurContract);
    CGF.ContractData->CurContract.emplace(std::move(CurContract));
  }
  ~CurrentContractRAII() {
    assert(CGF.ContractData->CurContract);
    CGF.ContractData->CurContract.reset();
  }
  CodeGenFunction &CGF;
};
} // namespace

CGContractData *CGContractDataDeleter::Create() { return new CGContractData(); }
void CGContractDataDeleter::operator()(CGContractData *Data) const {
  if (Data)
    delete Data;
}

llvm::BasicBlock *
CodeGenFunction::GetSharedContractViolationTrapBlock(bool Create) {
  if (!ContractData->TrapBlock && !Create)
    return nullptr;
  return ContractData->GetSharedTrapBlock(*this).Block;
}

void CodeGenFunction::EmitContractViolationTrap() {
  llvm::CallInst *TrapCall = EmitTrapCall(llvm::Intrinsic::trap);
  TrapCall->setDoesNotReturn();
  TrapCall->setDoesNotThrow();
  Builder.CreateUnreachable();
  Builder.ClearInsertionPoint();
}

//===----------------------------------------------------------------------===//
//                           Violation data emission
//===----------------------------------------------------------------------===//

namespace {

/// The LLVM type of the basic data block, and the offsets of its fields.
struct ContractDataBlockLayout {
  /// { descriptor, next, { file, function, line, column }, comment, message }
  llvm::StructType *Ty;
  CharUnits Align;
  /// The byte offsets of the fields from the start of the block, which are
  /// recorded in the descriptor table.
  CharUnits SourceLocation, Comment, Message;

  enum Index { DescriptorIdx, NextIdx, SourceLocationIdx, CommentIdx, MessageIdx };

  static ContractDataBlockLayout get(CodeGenModule &CGM) {
    ASTContext &Ctx = CGM.getContext();
    llvm::LLVMContext &LLVMCtx = CGM.getLLVMContext();
    llvm::Type *PtrTy = CGM.DefaultPtrTy;
    // struct __cxa_source_location {
    //   const char* file_name; const char* function_name;
    //   unsigned line; unsigned column;
    // };
    llvm::StructType *LocTy = llvm::StructType::get(
        LLVMCtx, {PtrTy, PtrTy, CGM.IntTy, CGM.IntTy});

    ContractDataBlockLayout Layout;
    Layout.Ty = llvm::StructType::get(LLVMCtx, {PtrTy, PtrTy, LocTy, PtrTy, PtrTy});
    Layout.Align = std::max(CGM.getPointerAlign(),
                            Ctx.getTypeAlignInChars(Ctx.UnsignedIntTy));
    const llvm::StructLayout *SL = CGM.getDataLayout().getStructLayout(Layout.Ty);
    auto Offset = [&](unsigned Idx) {
      return CharUnits::fromQuantity(SL->getElementOffset(Idx));
    };
    Layout.SourceLocation = Offset(SourceLocationIdx);
    Layout.Comment = Offset(CommentIdx);
    Layout.Message = Offset(MessageIdx);
    return Layout;
  }
};

} // namespace

/// Get or create the descriptor table shared by all data blocks of the module.
///
/// The table has the layout of __cxa_descriptor_table_t:
///
///   uint8_t header;         // table version (high 4 bits), vendor (low 4)
///   uint8_t num_entries;
///   uint8_t field_ids[num_entries];
///   <padding to alignof(__cxa_descriptor_data_t)>
///   uintptr_t data[num_entries];   // byte offset of each field in the block
///
/// It is emitted as a packed struct with explicit padding, so that its layout
/// is exactly the one of the ABI regardless of how LLVM lays out structs.
static llvm::GlobalVariable *
getContractDescriptorTable(CodeGenModule &CGM,
                           const ContractDataBlockLayout &Layout,
                           llvm::GlobalVariable *&Table) {
  if (Table)
    return Table;

  ASTContext &Ctx = CGM.getContext();
  const uint8_t FieldIDs[] = {cxa::FieldSourceLocation, cxa::FieldComment,
                              cxa::FieldMessage};
  const CharUnits FieldOffsets[] = {Layout.SourceLocation, Layout.Comment,
                                    Layout.Message};
  constexpr unsigned NumEntries = std::size(FieldIDs);

  // __cxa_descriptor_data_t is a union of uintptr_t and const void*.
  CharUnits DataAlign =
      std::max(Ctx.getTypeAlignInChars(Ctx.getUIntPtrType()),
               Ctx.getTypeAlignInChars(Ctx.VoidPtrTy));

  ConstantInitBuilder Builder(CGM);
  auto Desc = Builder.beginStruct();
  Desc.setPacked(true);
  Desc.addInt(CGM.Int8Ty, (cxa::TableVersion << 4) | cxa::VendorClang);
  Desc.addInt(CGM.Int8Ty, NumEntries);
  for (uint8_t ID : FieldIDs)
    Desc.addInt(CGM.Int8Ty, ID);
  CharUnits Offset = Desc.getNextOffsetFromGlobal();
  for (CharUnits Pad = Offset.alignTo(DataAlign) - Offset; !Pad.isZero();
       Pad -= CharUnits::One())
    Desc.addInt(CGM.Int8Ty, 0);
  for (CharUnits FieldOffset : FieldOffsets)
    Desc.addInt(CGM.IntPtrTy, FieldOffset.getQuantity());

  Table = Desc.finishAndCreateGlobal("__contract_descriptor_table", DataAlign,
                                     /*constant=*/true,
                                     llvm::GlobalValue::PrivateLinkage);
  Table->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  return Table;
}

/// Compute the source location of the contract assertion \p S as the fields
/// of __cxa_source_location: the (presumed) file name, the enclosing function
/// \p D as __PRETTY_FUNCTION__ would name it, and the line and column of the
/// contract keyword.
static void getContractSourceLocation(CodeGenModule &CGM,
                                      const ContractStmt *S, const Decl *D,
                                      SmallVectorImpl<char> &File,
                                      std::string &Function, unsigned &Line,
                                      unsigned &Column) {
  ASTContext &Ctx = CGM.getContext();
  SourceManager &SM = Ctx.getSourceManager();
  PresumedLoc PLoc =
      SM.getPresumedLoc(SM.getExpansionRange(S->getBeginLoc()).getEnd());

  File.clear();
  Line = Column = 0;
  if (PLoc.isValid()) {
    StringRef FileName = PLoc.getFilename();
    File.assign(FileName.begin(), FileName.end());
    Preprocessor::processPathForFileMacro(File, Ctx.getLangOpts(),
                                          Ctx.getTargetInfo());
    Line = PLoc.getLine();
    Column = PLoc.getColumn();
  }

  // This is the function name std::source_location::current() produces.
  Function.clear();
  if (D && !isa<TranslationUnitDecl>(D))
    Function =
        PredefinedExpr::ComputeName(PredefinedIdentKind::PrettyFunction, D);
}

llvm::Constant *CodeGenModule::GetAddrOfContractDataBlock(const ContractStmt *S,
                                                          const Decl *D) {
  D = D ? D->getCanonicalDecl() : nullptr;
  llvm::Constant *&Entry = ContractDataBlocks[{S, D}];
  if (Entry)
    return Entry;

  ASTContext &Ctx = getContext();

  SmallString<256> File;
  std::string Function;
  unsigned Line, Column;
  getContractSourceLocation(*this, S, D, File, Function, Line, Column);

  // The comment is the source text of the predicate, and the message is the
  // diagnostic message given by [[clang::contract_message]], if any. They are
  // independent fields of the ABI (the message is the computed message of
  // P3099), so a message does not replace the predicate text; instead, a
  // contract_message attribute with include_source_text=false omits the text.
  std::string Comment;
  StringRef Message;
  if (const auto *MessageAttr = S->getAttrAs<ContractMessageAttr>()) {
    Message = MessageAttr->getMessage();
    if (MessageAttr->getIncludeSourceText())
      Comment = S->getSourceText(Ctx);
  } else {
    Comment = S->getSourceText(Ctx);
  }

  auto GetString = [&](StringRef Str) -> llvm::Constant * {
    return GetAddrOfConstantCString(Str.str(), ".str.contract").getPointer();
  };

  // The data block is a __cxa_contract_data_block followed by the fields of
  // the basic layout:
  //
  //   const __cxa_descriptor_table_t* descriptor;
  //   const __cxa_contract_data_block* next;    // null
  //   __cxa_source_location location;  // { file, function, line, column }
  //   const char* comment;
  //   const char* message;
  ContractDataBlockLayout Layout = ContractDataBlockLayout::get(*this);
  llvm::GlobalVariable *Descriptor =
      getContractDescriptorTable(*this, Layout, ContractDescriptorTable);

  ConstantInitBuilder Builder(*this);
  auto Block = Builder.beginStruct(Layout.Ty);
  Block.add(Descriptor);
  Block.addNullPointer(DefaultPtrTy);
  {
    auto Location = Block.beginStruct(
        cast<llvm::StructType>(Layout.Ty->getElementType(
            ContractDataBlockLayout::SourceLocationIdx)));
    Location.add(GetString(File));
    Location.add(GetString(Function));
    Location.addInt(IntTy, Line);
    Location.addInt(IntTy, Column);
    Location.finishAndAddTo(Block);
  }
  Block.add(GetString(Comment));
  Block.add(GetString(Message));

  llvm::GlobalVariable *GV = Block.finishAndCreateGlobal(
      "__contract_data", Layout.Align, /*constant=*/true,
      llvm::GlobalValue::PrivateLinkage);
  GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  Entry = GV;
  return GV;
}

//===----------------------------------------------------------------------===//
//                           Violation handler alias
//===----------------------------------------------------------------------===//

/// Whether \p FD declares the replaceable contract-violation handler
/// ([basic.contract.handler]): a function named ::handle_contract_violation
/// with C++ language linkage, attached to the global module, taking a single
/// 'const std::contracts::contract_violation&' and returning void.
static bool isContractViolationHandler(const FunctionDecl *FD) {
  if (!FD->getDeclName().isIdentifier() ||
      FD->getName() != "handle_contract_violation")
    return false;
  if (!FD->getDeclContext()->getRedeclContext()->isTranslationUnit() ||
      FD->isExternC() || FD->isCXXClassMember() || FD->isInlined() ||
      FD->getTemplatedKind() != FunctionDecl::TK_NonTemplate ||
      FD->isVariadic() || FD->getNumParams() != 1 ||
      !FD->getReturnType()->isVoidType())
    return false;

  const auto *RefTy =
      FD->getParamDecl(0)->getType()->getAs<LValueReferenceType>();
  if (!RefTy)
    return false;
  QualType Pointee = RefTy->getPointeeType();
  if (!Pointee.isConstQualified() || Pointee.isVolatileQualified())
    return false;
  const CXXRecordDecl *RD = Pointee->getAsCXXRecordDecl();
  if (!RD || !RD->getIdentifier() || RD->getName() != "contract_violation")
    return false;

  // The class is std::contracts::contract_violation; the standard library may
  // declare it in an inline namespace.
  const auto *Contracts = dyn_cast<NamespaceDecl>(RD->getDeclContext());
  while (Contracts && Contracts->isInline())
    Contracts = dyn_cast<NamespaceDecl>(Contracts->getParent());
  return Contracts && Contracts->getIdentifier() &&
         Contracts->getName() == "contracts" &&
         Contracts->getParent()->isStdNamespace();
}

void CodeGenModule::MaybeEmitContractViolationHandlerAlias(
    const FunctionDecl *FD, llvm::Function *Fn) {
  if (!FD->isExternallyVisible() || !isContractViolationHandler(FD))
    return;

  // The runtime calls the handler through a C-linkage symbol, so that handler
  // resolution does not depend on the mangling of the handler's signature. A
  // translation unit that declares that symbol itself (for instance, the
  // runtime library providing the default handler) takes care of it.
  if (getModule().getNamedValue(cxa::HandlerSymbolName) ||
      !getContext()
           .getTranslationUnitDecl()
           ->lookup(DeclarationName(
               &getContext().Idents.get(cxa::HandlerSymbolName)))
           .empty())
    return;

  // The symbol is referenced only by the runtime library, which may be a
  // shared library, so it must be visible outside of this module even when the
  // handler itself is not (-fvisibility=hidden).
  const llvm::Triple &Triple = getTriple();
  if (Triple.isOSBinFormatMachO() || Triple.isNVPTX()) {
    // The object format does not support aliases; emit a forwarding function
    // instead, and keep it from being dead-stripped.
    llvm::Function *Thunk = llvm::Function::Create(
        Fn->getFunctionType(), Fn->getLinkage(), Fn->getAddressSpace(),
        cxa::HandlerSymbolName, &getModule());
    Thunk->setVisibility(llvm::GlobalValue::DefaultVisibility);
    Thunk->setDSOLocal(Fn->isDSOLocal());
    Thunk->setCallingConv(Fn->getCallingConv());
    Thunk->setAttributes(Fn->getAttributes());
    llvm::IRBuilder<> IRB(
        llvm::BasicBlock::Create(getLLVMContext(), "entry", Thunk));
    SmallVector<llvm::Value *, 1> Args;
    for (llvm::Argument &Arg : Thunk->args())
      Args.push_back(&Arg);
    llvm::CallInst *Call = IRB.CreateCall(Fn, Args);
    Call->setCallingConv(Fn->getCallingConv());
    Call->setTailCallKind(llvm::CallInst::TCK_Tail);
    IRB.CreateRetVoid();
    addUsedGlobal(Thunk);
    return;
  }

  auto *Alias = llvm::GlobalAlias::create(
      Fn->getValueType(), Fn->getAddressSpace(), Fn->getLinkage(),
      cxa::HandlerSymbolName, Fn, &getModule());
  Alias->setVisibility(llvm::GlobalValue::DefaultVisibility);
  Alias->setDSOLocal(Fn->isDSOLocal());
}

//===----------------------------------------------------------------------===//
//                           Violation reporting
//===----------------------------------------------------------------------===//

/// Get the entry point of the contract-violation runtime for the given
/// assertion kind, evaluation semantic and detection mode:
///
///   __cxa_contract_violation_{kind}_{semantic}_{mode}[_noexcept]
///
/// All entry points take the address of the data block chain as their only
/// argument. The 'enforce' entry points do not return, and the '_noexcept'
/// variants do not unwind: they terminate if the violation handler throws.
static llvm::FunctionCallee
getContractViolationEntryPoint(CodeGenModule &CGM, ContractKind Kind,
                               ContractEvaluationSemantic Semantic,
                               ContractDetectionMode Mode, bool IsNoexcept) {
  SmallString<64> Name("__cxa_contract_violation_");
  switch (Kind) {
  case ContractKind::Pre:
    Name += "pre";
    break;
  case ContractKind::Post:
    Name += "post";
    break;
  case ContractKind::Assert:
    Name += "assert";
    break;
  }
  switch (Semantic) {
  case Enforce:
    Name += "_enforce";
    break;
  case Observe:
    Name += "_observe";
    break;
  case Ignore:
  case QuickEnforce:
    llvm_unreachable("semantic has no runtime entry point");
  }
  switch (Mode) {
  case PredicateFailed:
    Name += "_pf";
    break;
  case ExceptionRaised:
    Name += "_ex";
    break;
  case ContractDetectionMode::Unspecified:
    llvm_unreachable("detection mode has no runtime entry point");
  }
  if (IsNoexcept)
    Name += "_noexcept";

  // FIXME: On Apple platforms, the entry points are only available in the
  // system libc++abi starting with the release that ships the contracts
  // runtime. Checking contracts should be diagnosed when the deployment target
  // is older, as is done for other runtime-dependent features such as aligned
  // allocation.
  ASTContext &Ctx = CGM.getContext();
  llvm::FunctionCallee Callee =
      CGM.CreateRuntimeFunction(Ctx.VoidTy, {Ctx.VoidPtrTy}, Name);
  if (auto *Fn = dyn_cast<llvm::Function>(Callee.getCallee())) {
    llvm::AttrBuilder FnAttrs(CGM.getLLVMContext());
    FnAttrs.addAttribute(llvm::Attribute::Cold);
    if (Semantic == Enforce)
      FnAttrs.addAttribute(llvm::Attribute::NoReturn);
    if (IsNoexcept)
      FnAttrs.addAttribute(llvm::Attribute::NoUnwind);
    Fn->addFnAttrs(FnAttrs);
  }
  return Callee;
}

bool CodeGenFunction::isContractViolationInNoexceptContext() {
  if (!getLangOpts().Exceptions)
    return true;
  for (const EHScope &Scope : EHStack) {
    switch (Scope.getKind()) {
    case EHScope::Cleanup:
      // Cleanups between the violation and a terminate scope need not run
      // ([except.terminate]).
      continue;
    case EHScope::Terminate:
      return true;
    case EHScope::Catch:
    case EHScope::Filter:
      return false;
    }
  }
  return false;
}

void CodeGenFunction::EmitContractViolation(const ContractStmt &S,
                                            ContractDetectionMode Mode) {
  ContractEvaluationSemantic Semantic = S.getSemantic(getContext());
  switch (Semantic) {
  case Ignore:
    return;
  case QuickEnforce:
    // No handler, no data: trap.
    EmitContractViolationTrap();
    return;
  case Enforce:
  case Observe:
    break;
  }

  // The data block names the innermost enclosing function, as
  // std::source_location::current() would.
  llvm::Constant *Data = CGM.GetAddrOfContractDataBlock(&S, CurCodeDecl);
  bool IsNoexcept = isContractViolationInNoexceptContext();
  llvm::FunctionCallee EntryPoint = getContractViolationEntryPoint(
      CGM, S.getContractKind(), Semantic, Mode, IsNoexcept);
  llvm::Value *Args[] = {Data};

  if (Semantic == Enforce) {
    if (IsNoexcept) {
      llvm::CallInst *Call = EmitNounwindRuntimeCall(EntryPoint, Args);
      Call->setDoesNotReturn();
      Builder.CreateUnreachable();
    } else {
      EmitNoreturnRuntimeCallOrInvoke(EntryPoint, Args);
    }
    Builder.ClearInsertionPoint();
    return;
  }

  if (IsNoexcept)
    EmitNounwindRuntimeCall(EntryPoint, Args);
  else
    EmitRuntimeCallOrInvoke(EntryPoint, Args);
}

//===----------------------------------------------------------------------===//
//                           Contract assertions
//===----------------------------------------------------------------------===//

/// Returns true if \p FD has a function contract assertion (of kind \p Kind,
/// if given) which is evaluated with a checking semantic.
static bool hasCheckedContracts(const FunctionDecl *FD,
                                std::optional<ContractKind> Kind) {
  const ASTContext &Ctx = FD->getASTContext();
  for (const ContractStmt *S : FD->contracts())
    if ((!Kind || S->getContractKind() == *Kind) &&
        S->getSemantic(Ctx) != Ignore)
      return true;
  return false;
}

bool CodeGenFunction::hasCheckedFunctionContracts(const FunctionDecl *FD) {
  return hasCheckedContracts(FD, std::nullopt);
}

bool CodeGenFunction::hasCheckedPreconditions(const FunctionDecl *FD) {
  return hasCheckedContracts(FD, ContractKind::Pre);
}

bool CodeGenFunction::hasCheckedPostconditions(const FunctionDecl *FD) {
  return hasCheckedContracts(FD, ContractKind::Post);
}

bool CodeGenFunction::shouldEmitFunctionContracts(GlobalDecl GD) {
  const auto *FD = cast<FunctionDecl>(GD.getDecl());
  const auto *MD = dyn_cast<CXXMethodDecl>(FD);

  if (!hasCheckedFunctionContracts(FD))
    return false;

  // A naked function has no prologue or epilogue, so there are neither
  // parameters nor a result to evaluate the contract assertions with.
  if (FD->hasAttr<NakedAttr>())
    return false;

  // The lambda static invoker forwards to the call operator, which evaluates
  // the contracts.
  if (MD && MD->isLambdaStaticInvoker())
    return false;

  // On targets using inalloca, the call operator of a lambda with a static
  // invoker forwards to a separate implementation function, which evaluates
  // the contracts.
  if (MD && isLambdaCallOperator(MD) && CurFnInfo &&
      !CurFnInfo->isDelegateCall() && MD->getParent()->getLambdaStaticInvoker() &&
      hasInAllocaArg(MD))
    return false;

  if (const auto *Ctor = dyn_cast<CXXConstructorDecl>(FD)) {
    // An inheriting constructor calls the inherited constructor, which
    // evaluates its own contracts.
    if (Ctor->isInheritingConstructor())
      return false;
    // The complete object constructor may simply delegate to the base object
    // constructor. See EmitConstructorBody.
    if (GD.getCtorType() == Ctor_Complete &&
        CGM.getTarget().getCXXABI().hasConstructorVariants() &&
        IsConstructorDelegationValid(Ctor))
      return false;
    return true;
  }

  if (const auto *Dtor = dyn_cast<CXXDestructorDecl>(FD)) {
    // These must stay in sync with EmitDestructorBody.
    switch (GD.getDtorType()) {
    case Dtor_Deleting:
    case Dtor_VectorDeleting:
      // Always delegates to the complete object destructor.
      return false;
    case Dtor_Complete:
      // Complete destructors of abstract classes are never called, and
      // otherwise the complete destructor either delegates to the base
      // destructor or runs the destructor body itself.
      if (Dtor->getParent()->isAbstract())
        return false;
      return !IsDestructorDelegationValid(Dtor);
    case Dtor_Base:
    case Dtor_Unified:
    case Dtor_Comdat:
      return true;
    }
    llvm_unreachable("unhandled destructor type");
  }

  return true;
}

void CodeGenFunction::MapContractParameters(const FunctionDecl *FD) {
  // The predicates of the function contract assertions refer to the
  // parameters (and result name) of the declaration that introduces the
  // contracts, which need not be the definition being emitted. Make them
  // denote the parameters of the definition; the result name reads the
  // return slot (see EmitDeclRefLValue).
  const ContractSpecifierDecl *CSD = FD->getContractsInEffect();
  const auto *Owner = dyn_cast<FunctionDecl>(CSD->getDeclContext());
  if (!Owner || Owner == FD || Owner->getNumParams() != FD->getNumParams())
    return;
  for (unsigned I = 0, N = FD->getNumParams(); I != N; ++I) {
    auto It = LocalDeclMap.find(FD->getParamDecl(I));
    if (It != LocalDeclMap.end())
      setAddrOfLocalVar(Owner->getParamDecl(I), It->second);
  }
}

void CodeGenFunction::EmitPreconditions(const FunctionDecl *FD) {
  for (const ContractStmt *S : FD->preconditions())
    EmitStmt(S);

  // The execution counts of the conditions in the predicates do not apply to
  // the function body which follows.
  if (const Stmt *Body = FD->getBody())
    PGO->setCurrentStmt(Body);
}

void CodeGenFunction::BeginFunctionPostconditions() {
  assert(ContractsFD && !PostconditionsSavedReturnBlock.isValid());
  if (!hasCheckedPostconditions(ContractsFD))
    return;

  // [expr.call]p10, [stmt.return]p5: The postcondition assertions are
  // evaluated after the result object has been initialized and the variables
  // (and temporaries) of the function body have been destroyed, but before
  // the function parameters are destroyed.
  //
  // The only cleanups on the stack at this point are those of the parameters
  // which are destroyed in the callee, so make return statements branch to a
  // block inside the scope of these cleanups. FinishFunction evaluates the
  // postconditions there once the cleanups of the function body have been
  // popped.
  PostconditionsSavedReturnBlock = ReturnBlock;
  PostconditionsCleanupDepth = EHStack.stable_begin();
  ReturnBlock = getJumpDestInCurrentScope("contract.post");
}

void CodeGenFunction::EmitFunctionPostconditions(SourceLocation EndLoc) {
  assert(ContractsFD && PostconditionsSavedReturnBlock.isValid());
  assert(EHStack.stable_begin() == PostconditionsCleanupDepth &&
         "cleanups of the function body must have been popped");

  llvm::BasicBlock *PostBB = ReturnBlock.getBlock();
  ReturnBlock = PostconditionsSavedReturnBlock;
  PostconditionsSavedReturnBlock = JumpDest();

  // Avoid emitting a separate block in simple cases, like EmitReturnBlock.
  if (llvm::BasicBlock *CurBB = Builder.GetInsertBlock()) {
    assert(!CurBB->hasTerminator() && "Unexpected terminated block.");
    if (CurBB->empty() || PostBB->use_empty()) {
      PostBB->replaceAllUsesWith(CurBB);
      delete PostBB;
    } else {
      EmitBlock(PostBB);
    }
  } else if (PostBB->use_empty()) {
    // The function never returns normally, so the postcondition assertions
    // are never evaluated.
    delete PostBB;
    return;
  } else {
    auto *BI = PostBB->hasOneUse()
                   ? dyn_cast<llvm::UncondBrInst>(*PostBB->user_begin())
                   : nullptr;
    if (BI && BI->getSuccessor(0) == PostBB) {
      Builder.SetInsertPoint(BI->getParent());
      BI->eraseFromParent();
      delete PostBB;
    } else {
      EmitBlock(PostBB);
    }
  }

  // If evaluating a postcondition exits via an exception (e.g. because the
  // violation handler throws), the behavior is as if the function body exited
  // via that exception ([basic.contract.eval]p17). The result object, which
  // has already been initialized, must be destroyed.
  EHScopeStack::stable_iterator CleanupDepth = EHStack.stable_begin();
  if (getLangOpts().Exceptions && ReturnValue.isValid() &&
      !FnRetTy->isReferenceType()) {
    if (QualType::DestructionKind DtorKind = FnRetTy.isDestructedType())
      pushDestroy(EHCleanup, ReturnValue, FnRetTy, getDestroyer(DtorKind),
                  /*useEHCleanupForArray=*/true);
  }

  // Likewise, the subobjects initialized by a constructor must be destroyed
  // ([except.ctor]p3). All subobjects have already been destroyed when the
  // postconditions of a destructor are evaluated.
  if (isa<CXXConstructorDecl>(ContractsFD))
    EnterStructorSubobjectEHCleanups();

  for (const ContractStmt *S : ContractsFD->postconditions())
    EmitStmt(S);

  PopCleanupBlocks(CleanupDepth);

  // The remainder of the epilogue is not part of any postcondition.
  if (CGDebugInfo *DI = getDebugInfo())
    DI->EmitLocation(Builder, EndLoc);
}

void CodeGenFunction::EmitContractStmt(const ContractStmt &S) {
  assert(!CurContract() && "contract assertions do not nest");

  // [basic.contract.eval]p4: The evaluation of a contract assertion using the
  // ignore semantic has no effect.
  ContractEvaluationSemantic Semantic = S.getSemantic(getContext());
  if (Semantic == Ignore)
    return;

  CurrentContractRAII CurContractRAII(*this, {&S, Semantic});

  llvm::BasicBlock *End = createBasicBlock("contract.end");
  // Violations of quick_enforce assertions share one trap block per function;
  // the others are reported inline.
  llvm::BasicBlock *Violation = Semantic == QuickEnforce
                                    ? GetSharedContractViolationTrapBlock()
                                    : createBasicBlock("contract.violation");

  // Catch any exception the evaluation of the predicate exits with, and report
  // the violation from within the handler: the contract-violation handler is
  // invoked from within an active implicit handler for that exception
  // ([basic.contract.eval]).
  bool CatchExceptions =
      getLangOpts().CXXExceptions && getLangOpts().ContractExceptions;
  if (CatchExceptions)
    EnterCatchAllScope();

  // Contract violations are unexpected: keep the violation handling out of
  // the fast path.
  llvm::Value *Predicate = EvaluateExprAsBool(S.getCond());
  Builder.CreateCondBr(
      Predicate, End, Violation,
      llvm::MDBuilder(getLLVMContext()).createLikelyBranchWeights());
  Builder.ClearInsertionPoint();

  if (CatchExceptions)
    ExitCatchAllScope(S.getBeginLoc(), End,
                      [&] { EmitContractViolation(S, ExceptionRaised); });

  if (Semantic != QuickEnforce) {
    EmitBlock(Violation);
    EmitContractViolation(S, PredicateFailed);
    if (HaveInsertPoint())
      Builder.CreateBr(End);
  }

  EmitBlock(End);
}
