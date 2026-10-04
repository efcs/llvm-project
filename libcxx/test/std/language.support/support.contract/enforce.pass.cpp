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

// ADDITIONAL_COMPILE_FLAGS: -fcontracts -fcontract-evaluation-semantic=enforce

// <contracts>

// With the enforce semantic, the contract-violation handler is called with a
// terminating contract_violation, and the program is terminated when the
// handler returns.

#include <contracts>
#include <cstdio>
#include <cstdlib>
#include <exception>

using namespace std::contracts;

int handler_calls = 0;
bool handler_ok   = false;

void handle_contract_violation(const contract_violation& v) {
  ++handler_calls;
  handler_ok = v.semantic() == evaluation_semantic::enforce && v.is_terminating() &&
               v.kind() == assertion_kind::pre && v.detection_mode() == detection_mode::predicate_false;
}

void f(int x) pre(x > 0) {}

int main(int, char**) {
  std::set_terminate([] {
    // Exit successfully only if the handler was called exactly once with the
    // expected information before terminating.
    std::_Exit(handler_calls == 1 && handler_ok ? 0 : 2);
  });
  f(1);
  f(0);
  std::fprintf(stderr, "the program should have been terminated\n");
  return 1;
}
