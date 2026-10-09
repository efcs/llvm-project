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

// void invoke_default_contract_violation_handler(const contract_violation&);

// A user-provided handler can invoke the default handler.

#include <cassert>
#include <contracts>
#include <cstdio>
#include <string>

#include "test_macros.h"

int handler_calls = 0;

void handle_contract_violation(const std::contracts::contract_violation& v) {
  ++handler_calls;
  std::contracts::invoke_default_contract_violation_handler(v);
}

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

int main(int, char**) {
  const char* path = "invoke_default.stderr.txt";
  std::FILE* err   = std::freopen(path, "w", stderr);
  assert(err);
  contract_assert(handler_calls != 0);
  assert(handler_calls == 1);
  std::fflush(stderr);

  std::string output = read_file(path);
  LIBCPP_ASSERT(output.find("invoke_default_contract_violation_handler.pass.cpp") != std::string::npos);
  LIBCPP_ASSERT(output.find("contract_assert(handler_calls != 0)") != std::string::npos);
  (void)output;
  return 0;
}
