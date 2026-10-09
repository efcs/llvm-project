//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// Test the experimental _LIBCPP_HARDENING_USE_CONTRACTS mode, where libc++'s
// hardening assertions are implemented as contract assertions placed in contract
// groups named "std.<category>".

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// REQUIRES: clang, has-fcontracts
// XFAIL: availability-contracts-missing

// Ignore all contracts by default, but enforce the valid-element-access checks.
// ADDITIONAL_COMPILE_FLAGS: -fcontracts -D_LIBCPP_HARDENING_USE_CONTRACTS
// ADDITIONAL_COMPILE_FLAGS: -fcontract-evaluation-semantic=ignore
// ADDITIONAL_COMPILE_FLAGS: -fcontract-group-evaluation-semantic=std.valid_element_access=enforce

#include <contracts>
#include <cstdio>
#include <cstdlib>
#include <span>

int handler_calls = 0;

void handle_contract_violation(const std::contracts::contract_violation& v) {
  ++handler_calls;
  bool ok = v.kind() == std::contracts::assertion_kind::assert &&
            v.semantic() == std::contracts::evaluation_semantic::enforce && v.is_terminating();
  // Exit before the program is terminated and before the invalid access happens.
  std::_Exit(ok && handler_calls == 1 ? 0 : 2);
}

int main(int, char**) {
  int a[2] = {1, 2};
  std::span<int> s(a);
  (void)s.front(); // OK, the check passes.
  if (handler_calls != 0)
    return 3;

  std::span<int> empty;
  (void)empty.front(); // The check fails and the handler is called.
  std::fprintf(stderr, "the contract-violation handler should have been called\n");
  return 1;
}
