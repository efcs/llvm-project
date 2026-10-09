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

// Without a user-provided contract-violation handler, the default handler is
// used. libc++'s default handler writes a diagnostic to stderr; with the
// observe semantic the program then continues.

#include <cassert>
#include <contracts>
#include <cstdio>
#include <string>

#include "test_macros.h"

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

int observed(int x) pre(x != 42) { return x; }

int main(int, char**) {
  const char* path = "default_handler.stderr.txt";
  std::FILE* err   = std::freopen(path, "w", stderr);
  assert(err);
  assert(observed(42) == 42);
  std::fflush(stderr);

  std::string output = read_file(path);
  // The exact format is unspecified; libc++ includes the location, the kind of
  // assertion and its source text.
  LIBCPP_ASSERT(output.find("default_handler.pass.cpp") != std::string::npos);
  LIBCPP_ASSERT(output.find("observed") != std::string::npos);
  LIBCPP_ASSERT(output.find("pre(x != 42)") != std::string::npos);
  LIBCPP_ASSERT(output.find("contract violation") != std::string::npos);
  (void)output;
  return 0;
}
