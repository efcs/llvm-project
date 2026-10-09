//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// UNSUPPORTED: no-exceptions

// A handler that exits via an exception at a '_noexcept' entry point
// terminates the program via std::terminate(), with the exception still
// active so that the terminate handler can inspect it.

#include <cassert>
#include <csignal>
#include <cstdlib>
#include <exception>

#include "support.h"

using namespace __cxxabiv1;

int calls = 0;

CONTRACTS_TEST_HANDLER() {
  ++calls;
  throw 42;
}

int main(int, char**) {
  std::signal(SIGABRT, [](int) { std::_Exit(2); });
  std::set_terminate([] {
    // The exception thrown by the handler is the current exception.
    bool ok = calls == 1 && std::current_exception() != nullptr;
    if (ok) {
      try {
        std::rethrow_exception(std::current_exception());
      } catch (int i) {
        ok = i == 42;
      } catch (...) {
        ok = false;
      }
    }
    std::_Exit(ok ? 0 : 3);
  });

  BasicBlock block = make_basic_block("file.cpp", "f", 1, 1, "x > 0");
  __cxa_contract_violation_assert_observe_pf_noexcept(block.data());
  // The observe entry point would have returned if the handler had.
  std::_Exit(1);
}
