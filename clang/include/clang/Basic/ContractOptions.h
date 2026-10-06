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
/// std::contracts::assertion_kind, which start at 1.
enum class ContractKind {
  /// A function precondition
  Pre,

  /// A function post condition
  Post,

  /// contract_assert
  Assert
};

/// Contract evaluation semantic. Determines whether to check contracts, and
/// what happens when a contract is violated.
///
/// NOTE: These values are Clang's internal encoding. They do *not* match
/// std::contracts::evaluation_semantic (ignore=1, observe=2, enforce=3,
/// quick_enforce=4), and they are never passed to the runtime: the evaluation
/// semantic is encoded in the name of the runtime entry point that CodeGen
/// calls (see CGContracts.cpp).
enum class ContractEvaluationSemantic {
  // Contracts are parsed, syntax checked and type checked, but never evaluated.
  Ignore = 0,

  // Contracts are run, failures are reported, and when a contract fails the
  // program is terminated. The compiler can assume after contracts statements
  // that the contracts hold.
  Enforce = 1,

  // Contracts are run, and failures are reported, but contract failures do not
  // logically stop execution of the program, nor can the compiler assume
  // contracts are true for optimizing.
  Observe = 2,

  // Contracts are run, failures cause an immediate trap without invoking the
  // contract-violation handler.
  QuickEnforce = 3,
};

/// How a contract violation was detected. The values match the enumerators of
/// std::contracts::detection_mode (and of the Itanium C++ Contracts ABI).
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


/// Represents the contract evaluation semantics selected on the command line.
///
/// The default semantic is set with '-fcontract-evaluation-semantic=<sem>'.
/// Individual contract groups can be given a different semantic with
/// '-fcontract-group-evaluation-semantic=<group>=<sem>[,<group>=<sem>...]'.
///
/// A contract group is a string consisting of identifiers joined by '.'. For
/// example, "a.b.c" is a contract group with three subgroups. A contract is
/// placed in a group with [[clang::contract_group("a.b.c")]].
///
/// When determining the semantic for a particular contract, the most specific
/// group with an explicitly specified semantic wins. For example, passing
///   '-fcontract-group-evaluation-semantic=std=ignore,std.hardening=enforce'
/// will enforce 'std.hardening.foo', but ignore 'std.bar'. Contracts in groups
/// with no matching entry use the default semantic.
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
