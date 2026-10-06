//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// UNSUPPORTED: no-exceptions

// The 'ex' entry points hold a reference to the exception the predicate exited
// with (the current exception) for the duration of the dispatch, and release
// it when they return. The 'pf' entry points do not touch the current
// exception at all: a violation detected inside a catch block must not keep
// the unrelated exception being handled there alive.

#include <cassert>
#include <exception>

#include "support.h"

using namespace __cxxabiv1;

int calls = 0;
int alive = 0;

struct Tracked {
  Tracked() { ++alive; }
  Tracked(const Tracked&) { ++alive; }
  ~Tracked() { --alive; }
};

CONTRACTS_TEST_HANDLER() { ++calls; }

int main(int, char**) {
  BasicBlock block = make_basic_block("file.cpp", "f", 1, 1, "x > 0");

  void (*const entries[])(void*) = {
      __cxa_contract_violation_pre_observe_pf,
      __cxa_contract_violation_pre_observe_pf_noexcept,
      __cxa_contract_violation_assert_observe_ex,
      __cxa_contract_violation_assert_observe_ex_noexcept,
      __cxa_contract_violation,
      __cxa_contract_violation_noexcept,
  };
  for (void (*entry)(void*) : entries) {
    calls = 0;
    try {
      throw Tracked();
    } catch (const Tracked&) {
      assert(alive == 1);
      entry(block.data());
      assert(calls == 1);
      // The exception is still being handled.
      assert(alive == 1);
      assert(std::uncaught_exceptions() == 0);
      assert(std::current_exception() != nullptr);
    }
    // Leaving the catch block destroys the exception object: nothing else
    // holds a reference to it.
    assert(alive == 0);
    assert(std::current_exception() == nullptr);
  }

  // Nested in a handler for another exception, the same holds for the
  // exception being handled by the outer catch block.
  calls = 0;
  try {
    throw Tracked();
  } catch (const Tracked&) {
    try {
      throw 1;
    } catch (int) {
      assert(alive == 1);
      __cxa_contract_violation_pre_observe_ex(block.data());
      __cxa_contract_violation_pre_observe_pf(block.data());
      assert(calls == 2);
    }
    assert(alive == 1);
  }
  assert(alive == 0);

  return 0;
}
