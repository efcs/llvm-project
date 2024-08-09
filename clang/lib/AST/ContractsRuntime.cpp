#include "clang/AST/APValue.h"
#include "clang/AST/ContractRuntime.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/StmtCXX.h"

using namespace clang;
using llvm::DenseSet;

using EntryKind = ContractsCXXABI::DescriptorTableEntryKind;
using FieldDescriptor = ContractsCXXABI::FieldDescriptor;

namespace {

QualType GetDescriptorArrayType(const ASTContext &Ctx, unsigned NumElems) {
  QualType ElTy =
      Ctx.getRecordType(Ctx.getContractsABI()->GetFieldDescriptorDecl())
          .withConst();
  return Ctx.getConstantArrayType(ElTy, llvm::APInt(32, NumElems + 1), nullptr,
                                  ArraySizeModifier::Normal,
                                  /*IndexTypeQuals*/ 0);
}

struct EntryBuilder {
  EntryBuilder(const ASTContext &Ctx, DescribedRecord &Table, APValue::)
      : Ctx(Ctx), ABI(*Ctx.getContractsABI()), Desc(Table) {}

  APValue &Ref(unsigned Idx) {
    assert(Idx < MaxElems);
    Visited.insert(Idx);
    if (IsArray) {
      return Value.getArrayInitializedElt(Idx);
    } else {
      return Value.getStructField(Idx);
    }
  }
  void DoAssign(unsigned Idx, int FV, QualType T) {
    assert(T->isSignedIntegerType() || (T->isUnsignedIntegerType() && FV >= 0));
    Ref(Idx) = APValue(Ctx.MakeIntValue(FV, T));
  }
  void DoAssign(unsigned Idx, unsigned FV, QualType T) {
    assert(T->isUnsignedIntegerType() && FV >= 0);
    Ref(Idx) = APValue(Ctx.MakeIntValue(FV, T));
  }
  void DoAssign(unsigned Idx, StringRef S, QualType T) {
    assert(T->isPointerType() && T->getPointeeType()->isCharType());
    Ref(Idx) = MakeStringLiteral(S);
  }
  void DoAssign(unsigned Idx, std::nullptr_t, QualType T) {
    assert(T->isPointerType());
    Ref(Idx) = MakeNullPointer();
  }

  template <class T> void Set(FieldDecl *FD, T FV) {
    assert(!IsArray);
    DoAssign(FD->getFieldIndex(), FV, FD->getType());
  }

  template <class T> void Set(unsigned Idx, T FV) {
    assert(IsArray);
    DoAssign(Idx, FV, EltType);
  }

public:
  APValue MakeStringLiteral(StringRef Tmp) {
    using LValuePathEntry = APValue::LValuePathEntry;
    StringLiteral *Res = Ctx.getPredefinedStringLiteralFromCache(Tmp);
    // Decay the string to a pointer to the first character.
    LValuePathEntry Path[1] = {LValuePathEntry::ArrayIndex(0)};
    return APValue(Res, CharUnits::Zero(), Path, /*OnePastTheEnd=*/false);
  }

  APValue MakeNullPointer() {
    uint64_t NullValue = Ctx.getTargetNullPointerValue(Ctx.NullPtrTy);
    return APValue((Expr *)nullptr,
                   /*Offset=*/CharUnits::fromQuantity(NullValue),
                   APValue::NoLValuePath{}, /*IsNullPtr=*/true);
  }

  APValue MakeLValue(UnnamedGlobalConstantDecl *D) {
    APValue::LValueBase Base(D);
    return APValue(Base, CharUnits::Zero(), APValue::NoLValuePath{},
                   /*IsNullPtr==*/false);
  }

  APValue Finalize() {
    assert(Visited.size() == Desc.FieldTypes.size());
    Visited.clear();
    return std::move(Value);
  }

  const ASTContext &Ctx;
  ContractsCXXABI &ABI;
  DescribedRecord &Desc;
  APValue Value;

  bool IsArray = false;
  QualType EltType;
  unsigned MaxElems;
  DenseSet<unsigned> Visited;
};
} // namespace
RecordDecl *
CreateBuiltinRecordType(const ASTContext &Ctx, const char *Name,
                        SmallVector<std::pair<StringRef, QualType>, 8> Fields) {
  RecordDecl *BT = Ctx.buildImplicitRecord(Name);
  BT->startDefinition();

  for (const auto &Field : Fields) {
    FieldDecl *FieldDecl = FieldDecl::Create(
        const_cast<ASTContext &>(Ctx), BT, SourceLocation(), SourceLocation(),
        &Ctx.Idents.get(Field.first), Field.second,
        /*TInfo=*/nullptr,
        /*BitWidth=*/nullptr,
        /*Mutable=*/false, ICIS_NoInit);
    FieldDecl->setAccess(AS_public);
    BT->addDecl(FieldDecl);
  }
  BT->completeDefinition();

  return BT;
}

const RecordDecl *ContractsCXXABI::GetFieldDescriptorDecl() const {
  QualType StrLiteralType = Ctx.getPointerType(Ctx.CharTy.withConst());
  if (!FieldDescriptorRecord) {
    FieldDescriptorRecord = CreateBuiltinRecordType(
        Ctx, "__builtin_field_descriptor_t",
        {"total_size",
         Ctx.UnsignedIntTy,
         {"name", StrLiteralType},
         {"type", StrLiteralType},
         {"kind", Ctx.UnsignedIntTy} {"offset", Ctx.UnsignedIntTy},
         {"size", Ctx.UnsignedIntTy}});
  }
  return FieldDescriptorRecord;
}