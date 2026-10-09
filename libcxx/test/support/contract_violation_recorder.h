//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef SUPPORT_CONTRACT_VIOLATION_RECORDER_H
#define SUPPORT_CONTRACT_VIOLATION_RECORDER_H

// Helpers for testing C++26 contracts end-to-end.
//
// Including this header defines the program's contract-violation handler
// (::handle_contract_violation), so it must be included by exactly one
// translation unit of the test program. The handler records every violation in
// a global ViolationLog and then, depending on `on_violation`, returns or
// throws a ViolationThrown exception.
//
// Note that contract predicates cannot modify variables declared outside of
// the predicate by naming them directly (such variables are const within the
// predicate), but they can call functions. Tests therefore observe evaluation
// of predicates through functions like `hit()` and `alive()` below.

#include <cassert>
#include <contracts>
#include <cstring>
#include <exception>
#include <source_location>
#include <string>
#include <vector>

#include "test_macros.h"

struct RecordedViolation {
  std::contracts::assertion_kind kind;
  std::contracts::evaluation_semantic semantic;
  std::contracts::detection_mode mode;
  bool is_terminating;
  bool has_evaluation_exception;
  // Whether std::current_exception() inside the handler refers to the same
  // exception as contract_violation::evaluation_exception().
  bool current_exception_matches;
  std::exception_ptr evaluation_exception;
  std::string comment;
  std::string file;
  std::string function;
  unsigned line;
};

enum class OnViolation { Return, Throw };

struct ViolationThrown {
  int id;
};

struct ViolationLog {
  std::vector<RecordedViolation> violations;
  OnViolation on_violation = OnViolation::Return;
  int throws = 0;

  void clear() { violations.clear(); }
  std::size_t size() const { return violations.size(); }
  const RecordedViolation& operator[](std::size_t i) const { return violations.at(i); }
  const RecordedViolation& back() const {
    assert(!violations.empty());
    return violations.back();
  }
};

inline ViolationLog& violation_log() {
  static ViolationLog log;
  return log;
}

void handle_contract_violation(const std::contracts::contract_violation& v) {
  ViolationLog& log = violation_log();
  std::source_location loc = v.location();
  log.violations.push_back(RecordedViolation{
      v.kind(),
      v.semantic(),
      v.detection_mode(),
      v.is_terminating(),
      static_cast<bool>(v.evaluation_exception()),
      std::current_exception() == v.evaluation_exception(),
      v.evaluation_exception(),
      v.comment() ? v.comment() : "",
      loc.file_name() ? loc.file_name() : "",
      loc.function_name() ? loc.function_name() : "",
      static_cast<unsigned>(loc.line())});
#ifndef TEST_HAS_NO_EXCEPTIONS
  if (log.on_violation == OnViolation::Throw)
    throw ViolationThrown{++log.throws};
#endif
}

inline bool contains(const std::string& haystack, const char* needle) {
  return haystack.find(needle) != std::string::npos;
}

// A table of counters, indexed by small integer ids, that predicates can update
// through function calls.
constexpr int MaxCounters = 64;

inline int* counters() {
  static int c[MaxCounters] = {};
  return c;
}

// Increments counter `id` and returns `result`.
inline bool hit(int id, bool result = true) {
  assert(id >= 0 && id < MaxCounters);
  ++counters()[id];
  return result;
}

inline int count(int id) { return counters()[id]; }

inline void reset_counters() {
  for (int i = 0; i != MaxCounters; ++i)
    counters()[i] = 0;
}

// Appends `id` to the evaluation sequence and returns true.
inline std::vector<int>& sequence() {
  static std::vector<int> seq;
  return seq;
}

inline bool seq(int id) {
  sequence().push_back(id);
  return true;
}

#endif // SUPPORT_CONTRACT_VIOLATION_RECORDER_H
