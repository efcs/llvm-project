// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Defines the types used to communicate contract violations between
/// the compiler and the library.
///
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_CONTRACTS_RUNTIME_H
#define LLVM_CLANG_AST_CONTRACTS_RUNTIME_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTFwd.h"
#include "clang/AST/CanonicalType.h"
#include "clang/AST/CommentCommandTraits.h"
#include "clang/AST/ComparisonCategories.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/ExternalASTSource.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/RawCommentList.h"
#include "clang/AST/TemplateName.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/TypeSize.h"
#include <optional>

namespace llvm {

class Value;
class Constant;
class APFixedPoint;
class FixedPointSemantics;
struct fltSemantics;
template <typename T, unsigned N> class SmallPtrSet;
} // namespace llvm

namespace clang {

class UnnamedGlobalConstantDecl;

struct ContractsCXXABI {

  // These enumeration values shouldn't change as they're assumed by the runtime
  enum DescriptorTableEntryKind : unsigned {
    EK_None = 0,
    EK_CStr = 1,
    EK_VoidPtr = 2,
    EK_Unsigned = 4,
    EK_Int = 5,
    EK_DescriptorTable = 8,
  };

  struct FieldDescriptor {
    // FIXME: Do I need this level of paranoia?
    unsigned TotalSize; // sizeof(FieldDescriptor)

    // The name of the field. Or unset if this is the first field. In which case
    // the rest of the members have special meaning.
    const char *Name;

    // Do we need this? I kind of want it more than I want the enumerator.
    // Should this be the mangled type string?
    const char *Type;

    // The type of the field. Or 0 if this is the first field.
    // Alternatively maybe a typeid?
    // Maybe emit as a string?
    EntryKind Kind;

    // The offset of the field in the table, or if the first record, the
    // number of entries in the table.
    unsigned Offset;

    // The size of the table in bytes or if the first record, the size of the
    // table in bytes.
    unsigned Size;
  };

private:
public:
  const RecordDecl *GetFieldDescriptorDecl() const;

private:
  /// The RecordDecl for thd above FieldDescriptorType, represented in the AST,
  /// Used to generate the
  mutable RecordDecl *FieldDescriptorRecord = nullptr;

  DenseMap<const RecordDecl *, const UnnamedGlobalConstantDecl *>
      DescribedTables;

public:
  const ASTContext &Ctx;
};

} // namespace clang

#endif