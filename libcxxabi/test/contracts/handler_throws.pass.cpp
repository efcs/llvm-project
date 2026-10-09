//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// UNSUPPORTED: no-exceptions

// An exception thrown by the handler propagates out of the entry points that
// are not '_noexcept', including the 'enforce' ones (which then do not
// terminate the program), and the 'ex' entry points record the exception the
// predicate exited with, which is the current exception when they are called.

#include <cassert>
#include <csignal>
#include <cstdlib>
#include <exception>

#include "support.h"

using namespace __cxxabiv1;

int calls = 0;
void* seen_exception;
bool seen_matches_current;

CONTRACTS_TEST_HANDLER() {
  Violation v = *static_cast<const Violation*>(violation_ptr);
  ++calls;
  seen_exception       = v.exception();
  seen_matches_current = false;
  // The exception object recorded in the chain is the one being handled.
  if (std::current_exception() != nullptr) {
    try {
      std::rethrow_exception(std::current_exception());
    } catch (int* current) {
      // The recorded exception object is the thrown int* itself.
      seen_matches_current = seen_exception != nullptr && *static_cast<int* const*>(seen_exception) == current;
    } catch (...) {
    }
  }
  throw 42;
}

int main(int, char**) {
  std::signal(SIGABRT, [](int) { std::_Exit(2); });
  std::set_terminate([] { std::_Exit(3); });

  BasicBlock block = make_basic_block("file.cpp", "f", 1, 1, "x > 0");

  void (*const entries[])(void*) = {
      __cxa_contract_violation_pre_observe_pf,
      __cxa_contract_violation_pre_enforce_pf,
      __cxa_contract_violation_post_observe_ex,
      __cxa_contract_violation_assert_enforce_ex,
      __cxa_contract_violation,
  };
  for (void (*entry)(void*) : entries) {
    calls = 0;
    try {
      entry(block.data());
      assert(false && "the handler's exception should have propagated");
    } catch (int i) {
      assert(i == 42);
    }
    assert(calls == 1);
  }

  // When the predicate exited via an exception, the entry point is called from
  // within a handler for it, and it records the exception object.
  calls = 0;
  try {
    try {
      throw new int(7);
    } catch (int* p) {
      // The 'ex' entry points capture the current exception.
      __cxa_contract_violation_pre_observe_ex(block.data());
    }
    assert(false);
  } catch (int i) {
    assert(i == 42);
  }
  assert(calls == 1);
  assert(seen_exception != nullptr);
  assert(seen_matches_current);

  // The 'pf' entry points do not record one.
  calls = 0;
  try {
    try {
      throw new int(7);
    } catch (int*) {
      __cxa_contract_violation_pre_observe_pf(block.data());
    }
  } catch (int) {
  }
  assert(calls == 1);
  assert(seen_exception == nullptr);

  return 0;
}
