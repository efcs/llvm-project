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

// contract_violation::evaluation_exception() returns an exception_ptr that
// refers to the exception the predicate exited with. A copy saved by the
// handler stays valid after the handler and the contract assertion have
// completed: the exception object lives as long as the exception_ptr does.
// It is also the exception the predicate exited with even when the handler
// calls it from inside a handler for another exception.

#include <cassert>
#include <contracts>
#include <exception>

#include "contract_violation_recorder.h"

struct Tracked {
  explicit Tracked(int v) : value(v) { ++alive; }
  Tracked(const Tracked& other) : value(other.value) { ++alive; }
  ~Tracked() { --alive; }
  int value;
  static int alive;
};
int Tracked::alive = 0;

bool throws(int v) { throw Tracked(v); }

void checked(int x) pre(x > 0 || throws(x)) {}

int main(int, char**) {
  ViolationLog& log = violation_log();

  checked(-7);
  assert(log.size() == 1);
  // The handler recorded evaluation_exception(), which was the current
  // exception at the time; the record keeps the exception object alive.
  assert(log.back().has_evaluation_exception);
  assert(log.back().current_exception_matches);
  assert(std::current_exception() == nullptr);
  assert(Tracked::alive == 1);

  std::exception_ptr saved = log.back().evaluation_exception;
  log.clear();
  assert(Tracked::alive == 1);
  try {
    std::rethrow_exception(saved);
  } catch (const Tracked& t) {
    assert(t.value == -7);
  }
  saved = nullptr;
  assert(Tracked::alive == 0);

  // Inside a handler for an unrelated exception, the violation's exception is
  // still the one the predicate exited with.
  try {
    throw 42;
  } catch (int) {
    checked(-8);
    assert(log.size() == 1);
    assert(log.back().has_evaluation_exception);
    assert(log.back().current_exception_matches);
    try {
      std::rethrow_exception(log.back().evaluation_exception);
    } catch (const Tracked& t) {
      assert(t.value == -8);
    }
    log.clear();
    // The unrelated exception is still the one being handled.
    try {
      std::rethrow_exception(std::current_exception());
    } catch (int i) {
      assert(i == 42);
    }
  }
  assert(Tracked::alive == 0);

  return 0;
}
