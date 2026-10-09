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

// When the predicate of an observed assertion exits via an exception, the
// contract-violation handler is invoked from within a handler for that
// exception. If the violation handler itself throws, its exception unwinds
// through the implicit handler for the predicate's exception: the predicate's
// exception object is destroyed (nothing keeps a reference to it), the new
// exception is the one the caller catches and the current one there, and
// nothing is left uncaught afterwards.

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

bool throws() { throw Tracked(); }

void checked(int x) pre(x > 0 || throws()) {}

int main(int, char**) {
  ViolationLog& log = violation_log();
  log.on_violation  = OnViolation::Throw;

  assert(std::uncaught_exceptions() == 0);
  try {
    checked(-1);
    assert(false);
  } catch (const ViolationThrown& e) {
    assert(e.id == 1);
    assert(log.size() == 1);
    assert(log.back().mode == std::contracts::detection_mode::evaluation_exception);
    assert(log.back().has_evaluation_exception);
    assert(log.back().current_exception_matches);
    // Only the handler's exception is active now.
    assert(std::uncaught_exceptions() == 0);
    std::exception_ptr current = std::current_exception();
    assert(current != nullptr);
    try {
      std::rethrow_exception(current);
    } catch (const ViolationThrown& again) {
      assert(again.id == 1);
    }
  }
  assert(std::current_exception() == nullptr);
  assert(std::uncaught_exceptions() == 0);

  // The predicate's exception object is kept alive by the handler's record of
  // it, and no longer than that.
  assert(Tracked::alive == 1);
  log.clear();
  assert(Tracked::alive == 0);

  // The same when the handler returns: the predicate's exception is destroyed
  // when the violation has been reported.
  log.on_violation = OnViolation::Return;
  checked(-2);
  assert(log.size() == 1);
  assert(Tracked::alive == 1);
  log.clear();
  assert(Tracked::alive == 0);
  assert(std::current_exception() == nullptr);

  return 0;
}
