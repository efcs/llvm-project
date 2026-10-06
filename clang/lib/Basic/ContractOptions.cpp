//===- Contracts.cpp - C Language Family Language Options -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines the classes from ContractOptions.h
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/ContractOptions.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"

using namespace clang;

void ContractOptions::setGroupSemantic(StringRef Key,
                                       ContractEvaluationSemantic Value) {
  assert(!Key.empty() &&
         "Set the default group semantic using DefaultSemantic");
  SemanticsByGroup[Key] = Value;
}

ContractEvaluationSemantic
ContractOptions::getSemanticForGroup(StringRef Group) const {
  while (!Group.empty()) {
    if (auto Pos = SemanticsByGroup.find(Group);
        Pos != SemanticsByGroup.end()) {
      return Pos->second;
    }
    auto NewEnd = Group.rfind('.');
    if (NewEnd == StringRef::npos)
      break;
    Group = Group.substr(0, NewEnd);
  }
  return DefaultSemantic;
}

static const char *semanticToString(ContractEvaluationSemantic Sem) {
  switch (Sem) {
  case ContractEvaluationSemantic::Ignore:
    return "ignore";
  case ContractEvaluationSemantic::Enforce:
    return "enforce";
  case ContractEvaluationSemantic::Observe:
    return "observe";
  case ContractEvaluationSemantic::QuickEnforce:
    return "quick_enforce";
  }
  llvm_unreachable("unhandled ContractEvaluationSemantic");
}

std::vector<std::string> ContractOptions::serializeContractGroupArgs() const {
  std::vector<std::string> ArgStrings;

  ArgStrings.reserve(SemanticsByGroup.size() + 1);
  ArgStrings.push_back(semanticToString(DefaultSemantic));
  for (const auto &Group : this->SemanticsByGroup) {
    ArgStrings.push_back(llvm::Twine(Group.first(), "=")
                             .concat(semanticToString(Group.second))
                             .str());
  }
  return ArgStrings;
}

std::optional<ContractEvaluationSemantic> semanticFromString(StringRef Str) {
  return llvm::StringSwitch<std::optional<ContractEvaluationSemantic>>(Str)
      .Case("ignore", ContractEvaluationSemantic::Ignore)
      .Case("enforce", ContractEvaluationSemantic::Enforce)
      .Case("observe", ContractEvaluationSemantic::Observe)
      .Case("quick_enforce", ContractEvaluationSemantic::QuickEnforce)
      .Default(std::nullopt);
}

void ContractOptions::addUnparsedContractGroup(
    StringRef Group, const ContractOptions::DiagnoseGroupFunc &Diagnoser) {
  if (Group.empty())
    return Diagnoser(ContractGroupDiagnostic::Empty, "", "");

  const bool ParseAsValueOnly = not Group.contains("=");
  auto [Key, Value] = Group.split('=');
  if (ParseAsValueOnly) {
    assert(Value.empty() && "Value should be empty");
    std::swap(Key, Value);
  } else {
    if (!validateContractGroup(Key, Diagnoser))
      return;
  }

  std::optional<ContractEvaluationSemantic> Sem = semanticFromString(Value);
  if (!Sem)
    return Diagnoser(ContractGroupDiagnostic::InvalidSemantic, Group, Value);

  if (ParseAsValueOnly)
    DefaultSemantic = Sem.value();
  else
    SemanticsByGroup[Key] = Sem.value();
}

void ContractOptions::parseContractGroups(
    const std::vector<std::string> &Groups,
    const ContractOptions::DiagnoseGroupFunc &Diagnoser) {

  for (const auto &GroupStr : Groups) {
    addUnparsedContractGroup(GroupStr, Diagnoser);
  }
}

bool ContractOptions::validateContractGroup(
    llvm::StringRef GroupName,
    const ContractOptions::DiagnoseGroupFunc &Diagnoser) {
  using CGD = ContractGroupDiagnostic;
  if (GroupName.empty()) {
    Diagnoser(CGD::Empty, GroupName, "");
    return false;
  }

  if (auto Pos = GroupName.find_first_not_of("abcdefghijklmnopqrstuvwxyz"
                                             "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                             "0123456789"
                                             "_.-");
      Pos != StringRef::npos) {
    Diagnoser(CGD::InvalidChar, GroupName, GroupName.substr(Pos, 1));
    return false;
  }

  if (GroupName[0] == '.') {
    Diagnoser(CGD::InvalidFirstChar, GroupName, ".");
    return false;
  }
  if (GroupName.back() == '.') {
    Diagnoser(CGD::InvalidLastChar, GroupName, ".");
    return false;
  }
  // Diagnose empty subgroups. i.e. "a..b"
  if (GroupName.find("..", 0) != StringRef::npos) {
    Diagnoser(CGD::EmptySubGroup, GroupName, "..");
    return false;
  }
  return true;
}
