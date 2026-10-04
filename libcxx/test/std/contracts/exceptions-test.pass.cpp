//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// UNSUPPORTED: no-exceptions
// REQUIRES: clang, has-fcontracts
// XFAIL: availability-contracts-missing

// ADDITIONAL_COMPILE_FLAGS: -fcontracts -fcontract-evaluation-semantic=observe
// ADDITIONAL_COMPILE_FLAGS: -fcontract-group-evaluation-semantic=enforce=enforce

// Check the interaction of contract assertions with exceptions:
//  - an exception escaping the evaluation of a predicate is a contract
//    violation with detection_mode::evaluation_exception, and the handler can
//    access the exception through std::current_exception() and
//    contract_violation::evaluation_exception();
//  - an exception thrown by the contract-violation handler propagates out of
//    the contract assertion, and objects are destroyed as usual.

#include <cassert>
#include <contracts>
#include <exception>

#include "contract_violation_recorder.h"

bool throws(int v) { throw v; }

enum { Local, Local2, InCatch };

template <int Id>
struct AliveCounter {
  AliveCounter() { ++counters()[Id]; }
  AliveCounter(const AliveCounter&) { ++counters()[Id]; }
  ~AliveCounter() { --counters()[Id]; }
};

bool make_temporary_and_fail() {
  AliveCounter<Local2> tmp;
  return count(Local2) != 1;
}

void test_evaluation_exception() {
  using namespace std::contracts;
  violation_log().clear();
  violation_log().on_violation = OnViolation::Return;

  // With the observe semantic, the program continues after the handler returns.
  contract_assert(throws(42));
  assert(violation_log().size() == 1);
  const RecordedViolation& v = violation_log()[0];
  assert(v.kind == assertion_kind::assert);
  assert(v.mode == detection_mode::evaluation_exception);
  assert(v.semantic == evaluation_semantic::observe);
  assert(v.has_evaluation_exception);
  assert(contains(v.comment, "throws(42)"));
}

void test_exception_is_accessible() {
  violation_log().clear();
  contract_assert(throws(42));
  assert(violation_log().size() == 1);
  const RecordedViolation& v = violation_log()[0];
  assert(v.current_exception_matches);
  assert(v.evaluation_exception != nullptr);
  try {
    std::rethrow_exception(v.evaluation_exception);
  } catch (int i) {
    assert(i == 42);
  }

  // When the predicate evaluates to false, there is no exception.
  violation_log().clear();
  contract_assert(false);
  assert(violation_log().size() == 1);
  assert(violation_log()[0].evaluation_exception == nullptr);
  assert(violation_log()[0].mode == std::contracts::detection_mode::predicate_false);
}

void test_handler_throws() {
  violation_log().clear();
  violation_log().on_violation = OnViolation::Throw;
  reset_counters();

  try {
    [] {
      AliveCounter<Local> local;
      contract_assert(make_temporary_and_fail());
      assert(false && "not reached");
    }();
    assert(false && "not reached");
  } catch (const ViolationThrown&) {
    ++counters()[InCatch];
  }
  assert(violation_log().size() == 1);
  assert(count(InCatch) == 1);
  assert(count(Local) == 0);
  assert(count(Local2) == 0);

  // The same applies to the enforce semantic: the program is not terminated
  // when the handler exits via an exception.
  violation_log().clear();
  try {
    contract_assert [[clang::contract_group("enforce")]] (false);
    assert(false && "not reached");
  } catch (const ViolationThrown&) {
    ++counters()[InCatch];
  }
  assert(count(InCatch) == 2);
  assert(violation_log().size() == 1);
  assert(violation_log()[0].semantic == std::contracts::evaluation_semantic::enforce);
  assert(violation_log()[0].is_terminating);

  violation_log().on_violation = OnViolation::Return;
}

int main(int, char**) {
  test_evaluation_exception();
  test_exception_is_accessible();
  test_handler_throws();
  return 0;
}
