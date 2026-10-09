//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23

// The universal entry point takes the evaluation semantic from the chain, and
// terminates the program with std::abort() after the handler returns when it
// is enforce.

#include <cassert>
#include <csignal>
#include <cstdint>
#include <cstdlib>
#include <exception>

#include "support.h"

using namespace __cxxabiv1;

int calls = 0;
std::uint8_t seen_semantic;

CONTRACTS_TEST_HANDLER() {
  Violation v = *static_cast<const Violation*>(violation_ptr);
  ++calls;
  seen_semantic = v.semantic();
}

int main(int, char**) {
  std::set_terminate([] { std::_Exit(3); });
  std::signal(SIGABRT, [](int) { std::_Exit(calls == 1 && seen_semantic == CXA_ES_ENFORCE ? 0 : 2); });

  BasicBlock block = make_basic_block("file.cpp", "f", 1, 1, "x > 0");
  KindBlock kinds  = make_kind_block(CXA_AK_MANUAL, CXA_ES_ENFORCE, CXA_DM_UNSPECIFIED, block.get());
  __cxa_contract_violation_noexcept(kinds.data());
  // The universal entry point is not [[noreturn]]: the semantic is in the chain.
  std::_Exit(1);
}
