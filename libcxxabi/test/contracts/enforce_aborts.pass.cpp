//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23

// The 'enforce' entry points call the handler and then terminate the program
// with std::abort() when the handler returns; a terminate handler does not
// interfere.

#include <cassert>
#include <csignal>
#include <cstdint>
#include <cstdlib>
#include <exception>

#include "support.h"

using namespace __cxxabiv1;

int calls = 0;
std::uint8_t seen_kind, seen_semantic, seen_mode;

CONTRACTS_TEST_HANDLER() {
  Violation v = *static_cast<const Violation*>(violation_ptr);
  ++calls;
  seen_kind     = v.kind();
  seen_semantic = v.semantic();
  seen_mode     = v.mode();
}

void on_abort(int) {
  // Exit successfully only if the handler was called exactly once with the
  // enforce semantic before the program was terminated.
  bool ok = calls == 1 && seen_kind == CXA_AK_POST && seen_semantic == CXA_ES_ENFORCE &&
            seen_mode == CXA_DM_EVALUATION_EXCEPTION;
  std::_Exit(ok ? 0 : 2);
}

int main(int, char**) {
  std::set_terminate([] { std::_Exit(3); });
  std::signal(SIGABRT, on_abort);

  BasicBlock block = make_basic_block("file.cpp", "f", 1, 1, "x > 0");
  __cxa_contract_violation_post_enforce_ex_noexcept(block.data());
}
