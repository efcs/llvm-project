//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23

// Without a replacement handler in the program, the default handler is used,
// which prints a diagnostic describing the violation to stderr. It can also be
// called directly, through __contract_invoke_default_handler.

#include <cassert>
#include <cstdio>
#include <cstring>
#include <string>

#include "support.h"

using namespace __cxxabiv1;

std::string read_file(const char* path) {
  std::FILE* f = std::fopen(path, "r");
  assert(f);
  std::string result;
  char buf[256];
  std::size_t n;
  while ((n = std::fread(buf, 1, sizeof(buf), f)) > 0)
    result.append(buf, n);
  std::fclose(f);
  return result;
}

bool contains(const std::string& s, const char* needle) { return s.find(needle) != std::string::npos; }

int main(int, char**) {
  const char* path = "default_handler.stderr.txt";
  std::FILE* err   = std::freopen(path, "w", stderr);
  assert(err);

  BasicBlock block = make_basic_block("file.cpp", "int f(int)", 42, 7, "x > 0", "x must be positive");
  __cxa_contract_violation_pre_observe_pf(block.data());
  __cxa_contract_violation_assert_observe_ex_noexcept(block.data());

  // Through the C-linkage default handler symbol, with a chain that has none of
  // the fields.
  KindBlock kinds = make_kind_block(CXA_AK_POST, CXA_ES_ENFORCE, CXA_DM_PREDICATE_FALSE);
  Violation v     = {kinds.get()};
  __contract_invoke_default_handler(&v);
  std::fflush(stderr);

  std::string output = read_file(path);
  assert(contains(output, "file.cpp:42: int f(int): contract violation in pre(x > 0: \"x must be positive\"): "
                          "predicate evaluated to false (semantic: observe)\n"));
  assert(contains(output, "file.cpp:42: int f(int): contract violation in contract_assert(x > 0: \"x must be positive\"): "
                          "predicate evaluation exited via an exception (semantic: observe)\n"));
  assert(contains(output, "<unknown file>:0: contract violation in post(): predicate evaluated to false "
                          "(semantic: enforce)\n"));
  return 0;
}
