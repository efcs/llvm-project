//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// REQUIRES: clang, has-fcontracts
// XFAIL: availability-contracts-missing

// ADDITIONAL_COMPILE_FLAGS: -fcontracts -fcontract-evaluation-semantic=observe

// <contracts>

// The contract-violation handler may be declared noexcept, and replaces the
// default handler.

#include <cassert>
#include <contracts>

int handler_calls = 0;

void handle_contract_violation(const std::contracts::contract_violation& v) noexcept {
  assert(v.kind() == std::contracts::assertion_kind::assert);
  ++handler_calls;
}

int main(int, char**) {
  contract_assert(handler_calls == 1);
  assert(handler_calls == 1);
  contract_assert(handler_calls == 1);
  assert(handler_calls == 1);
  return 0;
}
