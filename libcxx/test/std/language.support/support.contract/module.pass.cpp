//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// UNSUPPORTED: clang-modules-build
// UNSUPPORTED: gcc
// XFAIL: has-no-cxx-module-support
// XFAIL: availability-contracts-missing

// Check that `import std;` exports the contents of <contracts>. This does not
// require -fcontracts.

// MODULE_DEPENDENCIES: std

import std;

static_assert(static_cast<int>(std::contracts::assertion_kind::assert) == 3);
static_assert(static_cast<int>(std::contracts::evaluation_semantic::enforce) == 3);
static_assert(static_cast<int>(std::contracts::detection_mode::evaluation_exception) == 2);
static_assert(!std::is_copy_constructible_v<std::contracts::contract_violation>);

void handle_contract_violation(const std::contracts::contract_violation& v) {
  (void)v.comment();
  (void)v.detection_mode();
  (void)v.evaluation_exception();
  (void)v.is_terminating();
  (void)v.kind();
  (void)v.location();
  (void)v.semantic();
  std::contracts::invoke_default_contract_violation_handler(v);
}

int main(int, char**) { return 0; }
