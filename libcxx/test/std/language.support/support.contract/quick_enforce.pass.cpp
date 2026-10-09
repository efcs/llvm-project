//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// REQUIRES: clang, has-fcontracts, has-unix-headers
// UNSUPPORTED: no-localization
// XFAIL: availability-contracts-missing

// ADDITIONAL_COMPILE_FLAGS: -fcontracts -fcontract-evaluation-semantic=quick_enforce

// <contracts>

// With the quick_enforce semantic, the program is terminated without calling
// the contract-violation handler.

#include <cassert>
#include <contracts>
#include <cstdlib>

#include "check_assertion.h"

void handle_contract_violation(const std::contracts::contract_violation&) {
  // Not reached: calling the handler is a failure.
  std::_Exit(3);
}

void f(int x) pre(x > 0) {}

int main(int, char**) {
  f(1);
  assert(ExpectDeath(DeathCause::Trap, "f(0)", [] { f(0); }));
  return 0;
}
