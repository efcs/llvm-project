//===- ContractsOptions.h - C++ Contract Options ----------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Defines common enums and types used by contracts and contract attributes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_BASIC_CONTRACT_OPTIONS_H
#define LLVM_CLANG_BASIC_CONTRACT_OPTIONS_H

#include "clang/Basic/LLVM.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include <cassert>
#include <cstdint>
#include <functional>

namespace clang {
using llvm::StringRef;
class ASTContext;

enum class ContractTag { No = 0, Yes = 1 };

constexpr ContractTag NotAContract = ContractTag::No;
constexpr ContractTag IsAContract = ContractTag::Yes;
constexpr ContractTag InContract = ContractTag::Yes;
constexpr ContractTag NotInContract = ContractTag::No;

enum class ContractGroupDiagnostic {
  // The remaining values map to %select values in diagnostics in both
  // DiagnosticSemaKinds.td and DiagnosticDriverKinds.td.
  InvalidFirstChar,
  InvalidChar,
  InvalidLastChar,
  EmptySubGroup,
  Empty,
  InvalidSemantic,
};

/// The kind of contract.
///
/// NOTE: These enumerations _do not_ match the values in
/// std::contracts::assertion_kind because they start at 1.
enum class ContractKind {
  /// A function precondition
  Pre,

  /// A function post condition
  Post,

  /// contract_assert
  Assert
};

// std::contracts::assertion_kind
enum class ContractAssertionKind {
  Pre = 1,

  Post = 2,

  Assert = 3,

  Manual = 4,

  CAssert = 5
};

/// Contract evaluation mode. Determines whether to check contracts, and
// whether contract failures cause compile errors.
//
// These values match up with std::contracts::evaluation_semantic. However, only
// `enforce` and `observe` actually appear in STL enum.
enum class ContractEvaluationSemantic {
  // Contracts are parsed, syntax checked and type checked, but never evaluated.
  // FIXME(EricWF): This doesn't yet map to an actual enumerator in
  //  std::contracts::evaluation_semantic
  Ignore = 0,

  // Contracts are run, failures are reported, and when a contract fails the
  // program is terminated. The compiler can assume after contracts statements
  // that the contracts hold.
  Enforce = 1,

  // Contracts are run, and failures are reported, but contract failures do not
  // logically stop execution of the program, nor can the compiler assume
  // contracts are true for optimizing.
  Observe = 2,

  // Contracts are run, failures cause an immediate trap
  // FIXME(EricWF): This doesn't yet map to an actual enumerator in
  //  std::contracts::evaluation_semantic
  QuickEnforce = 3,
};

/// The result of checking a contract. The second two values match
/// std::contracts::detection_mode.
enum class ContractDetectionMode {
  Unspecified = 0,
  PredicateFailed = 1,
  ExceptionRaised = 2
};

/// The code generation style for the contract. Inline
enum class ContractEmissionStyle {
  /// Emit the contract at every call site, in the caller.
  InCaller,

  /// Emit the contract with the function definition.
  InDefinition,
};


// Indicates whether the contract scope information was pushed prior to the function declaration context being
// available (and therefore will need special consideration when adjusting the context).
enum ContractScopeOffset {
  CSO_ParentContext,
  CSO_FunctionContext
};

/// Represents the set of contract groups that have been enabled or disabled
/// on the command line using '-fclang-contract-groups='.
///
/// A contract group is a string consisting of identifiers join by '.'. For
/// example, "a.b.c" is a contract group with three subgroups.
///
/// When determining if a particular contract check is enabled, we check if
/// the user has enabled/disabled the group/subgroup that the check belongs to,
/// in order of specificity. For example, passing
///   '-fclang-contract-groups=-std,+std.hardening,-std.hardening.foo'
/// will enable 'std.hardening.baz', but disable 'std.hardening.foo.bar' and
/// 'std.baz'.
///
/// TODO(EricWF): Should we match in the same manner as clang-tidy checks?
///    Specifically, allow the use of '*' and drop all notion of groups?
class ContractOptions {
public:
  ContractOptions() = default;

public:
  using DiagnoseGroupFunc =
      std::function<void(ContractGroupDiagnostic, StringRef, StringRef)>;

  static bool validateContractGroup(llvm::StringRef GroupAndValue,
                                    const DiagnoseGroupFunc &Diagnoser);

  std::vector<std::string> serializeContractGroupArgs() const;

  void parseContractGroups(const std::vector<std::string> &Groups,
                           const DiagnoseGroupFunc &Diagnoser);

  void addUnparsedContractGroup(StringRef GroupAndValue,
                                const DiagnoseGroupFunc &Diagnoser);

  ContractEvaluationSemantic getSemanticForGroup(llvm::StringRef Group) const;
  void setGroupSemantic(llvm::StringRef Group,
                        ContractEvaluationSemantic Semantic);

public:
  /// The default semantics for contracts.
  ContractEvaluationSemantic DefaultSemantic =
      ContractEvaluationSemantic::Enforce;

  /// The semantics for each contract group, if specified.
  llvm::StringMap<ContractEvaluationSemantic> SemanticsByGroup;

  ContractEmissionStyle DefaultEmission = ContractEmissionStyle::InDefinition;
};

} // namespace clang

#endif // LLVM_CLANG_BASIC_CONTRACT_OPTIONS_H
