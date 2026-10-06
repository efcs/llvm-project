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

// A contract violation detected inside a catch block, with a predicate that
// evaluated to false, does not touch the exception being handled there: it is
// still the current exception once the handler has returned, nothing becomes
// uncaught, and leaving the catch block destroys the exception object as
// usual.

#include <cassert>
#include <contracts>
#include <exception>

#include "contract_violation_recorder.h"

struct Tracked {
  Tracked() { ++alive; }
  Tracked(const Tracked&) { ++alive; }
  ~Tracked() { --alive; }
  static int alive;
};
int Tracked::alive = 0;

void checked(int x) pre(x > 0) {}

int main(int, char**) {
  ViolationLog& log = violation_log();

  try {
    throw Tracked();
  } catch (const Tracked&) {
    assert(Tracked::alive == 1);
    std::exception_ptr before = std::current_exception();
    assert(before != nullptr);

    checked(-1);
    assert(log.size() == 1);
    assert(log.back().mode == std::contracts::detection_mode::predicate_false);
    // No predicate exception: evaluation_exception() is null, and the
    // exception being handled is not the violation's.
    assert(!log.back().has_evaluation_exception);

    assert(std::uncaught_exceptions() == 0);
    assert(std::current_exception() == before);
    assert(Tracked::alive == 1);
    log.clear();
  }
  assert(Tracked::alive == 0);
  assert(std::current_exception() == nullptr);

  // Nested in a handler for another exception, with a handler that throws:
  // the handler's exception replaces nothing, and is catchable in the catch
  // block.
  log.on_violation = OnViolation::Throw;
  try {
    throw Tracked();
  } catch (const Tracked&) {
    try {
      checked(-2);
      assert(false);
    } catch (const ViolationThrown& e) {
      assert(e.id == 1);
    }
    assert(Tracked::alive == 1);
    // Back to handling the outer exception.
    try {
      std::rethrow_exception(std::current_exception());
    } catch (const Tracked&) {
    }
  }
  assert(Tracked::alive == 0);

  return 0;
}
