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

// Check the information reported by contract_violation for violations of
// compiler-emitted preconditions, postconditions and assertions, using a
// user-provided contract-violation handler.

#include <cassert>
#include <contracts>
#include <cstring>
#include <exception>
#include <source_location>
#include <string_view>

#include "test_macros.h"

using namespace std::contracts;

struct Expected {
  assertion_kind kind;
  evaluation_semantic semantic;
  detection_mode mode;
  const char* comment;
  unsigned line;
  const char* function;
};

Expected expected;
int handler_calls = 0;

void handle_contract_violation(const contract_violation& v) {
  ++handler_calls;
  assert(v.kind() == expected.kind);
  assert(v.semantic() == expected.semantic);
  assert(v.detection_mode() == expected.mode);
  assert(!v.is_terminating());
  assert(std::string_view(v.comment()).find(expected.comment) != std::string_view::npos);

  std::source_location loc = v.location();
  assert(loc.line() == expected.line);
  assert(std::string_view(loc.file_name()).find("contract_violation.pass.cpp") != std::string_view::npos);
  assert(std::string_view(loc.function_name()).find(expected.function) != std::string_view::npos);

  if (v.detection_mode() == detection_mode::predicate_false) {
    assert(v.evaluation_exception() == nullptr);
  } else {
    assert(v.evaluation_exception() != nullptr);
    assert(v.evaluation_exception() == std::current_exception());
  }
}

int pre_violated(int x) pre(x > 0) { return x; }
constexpr unsigned pre_line = __LINE__ - 1;

int post_violated(const int x) post(x < 0) { return x; }
constexpr unsigned post_line = __LINE__ - 1;

void assert_violated(int x) {
  contract_assert(x == 0);
}
constexpr unsigned assert_line = __LINE__ - 2;

#ifndef TEST_HAS_NO_EXCEPTIONS
bool throw_int() { throw 42; }

void assert_throws() {
  contract_assert(throw_int());
}
constexpr unsigned assert_throws_line = __LINE__ - 2;
#endif

int main(int, char**) {
  expected = {assertion_kind::pre, evaluation_semantic::observe, detection_mode::predicate_false, "x > 0", pre_line,
              "pre_violated"};
  assert(pre_violated(-1) == -1);
  assert(handler_calls == 1);

  expected = {assertion_kind::post, evaluation_semantic::observe, detection_mode::predicate_false, "x < 0", post_line,
              "post_violated"};
  assert(post_violated(1) == 1);
  assert(handler_calls == 2);

  expected = {assertion_kind::assert, evaluation_semantic::observe, detection_mode::predicate_false, "x == 0",
              assert_line, "assert_violated"};
  assert_violated(1);
  assert(handler_calls == 3);

  // Contracts that hold do not call the handler.
  assert(pre_violated(1) == 1);
  assert_violated(0);
  assert(handler_calls == 3);

#ifndef TEST_HAS_NO_EXCEPTIONS
  expected = {assertion_kind::assert, evaluation_semantic::observe, detection_mode::evaluation_exception,
              "throw_int()", assert_throws_line, "assert_throws"};
  assert_throws();
  assert(handler_calls == 4);
#endif

  return 0;
}
