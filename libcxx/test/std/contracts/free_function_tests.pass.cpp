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

// Check that preconditions, postconditions and contract assertions on free
// functions and function templates are evaluated, and that violations are
// reported to the contract-violation handler with the right information.

#include <cassert>
#include <contracts>
#include <type_traits>

#include "contract_violation_recorder.h"

enum { Pre, Post, Assert, IntPre, NotIntPre };

int f(const int x) pre(hit(Pre)) post(r : hit(Post) && r == x + 1) {
  contract_assert(hit(Assert));
  return x + 1;
}

template <class T>
T ft(T v) pre(hit(Pre)) post(hit(Post)) pre(hit(std::is_same_v<T, int> ? IntPre : NotIntPre)) {
  contract_assert(hit(Assert));
  return v;
}

int g(const int x) pre(x > 0) post(r : r < 0) {
  contract_assert(x != 42);
  return x;
}
constexpr unsigned g_line = __LINE__ - 4;

void test_evaluation() {
  reset_counters();
  violation_log().clear();

  assert(f(1) == 2);
  assert(count(Pre) == 1 && count(Post) == 1 && count(Assert) == 1);

  assert(ft(42) == 42);
  assert(count(Pre) == 2 && count(Post) == 2 && count(Assert) == 2);
  assert(count(IntPre) == 1 && count(NotIntPre) == 0);

  assert(ft(42L) == 42L);
  assert(count(Pre) == 3 && count(Post) == 3 && count(Assert) == 3);
  assert(count(IntPre) == 1 && count(NotIntPre) == 1);

  assert(ft(1) == 1);
  assert(count(IntPre) == 2 && count(NotIntPre) == 1);

  assert(violation_log().size() == 0);
}

void test_violations() {
  using namespace std::contracts;

  // The precondition fails; with the observe semantic the program continues
  // after the handler returns.
  violation_log().clear();
  assert(g(-1) == -1);
  assert(violation_log().size() == 1);
  {
    const RecordedViolation& v = violation_log()[0];
    assert(v.kind == assertion_kind::pre);
    assert(v.semantic == evaluation_semantic::observe);
    assert(v.mode == detection_mode::predicate_false);
    assert(!v.is_terminating);
    assert(!v.has_evaluation_exception);
    assert(contains(v.comment, "x > 0"));
    assert(v.line == g_line);
    assert(contains(v.function, "g"));
    assert(contains(v.file, "free_function_tests.pass.cpp"));
  }

  // The postcondition fails.
  violation_log().clear();
  assert(g(1) == 1);
  assert(violation_log().size() == 1);
  assert(violation_log()[0].kind == assertion_kind::post);
  assert(contains(violation_log()[0].comment, "r < 0"));
  assert(violation_log()[0].line == g_line);

  // The assertion and the postcondition fail.
  violation_log().clear();
  assert(g(42) == 42);
  assert(violation_log().size() == 2);
  assert(violation_log()[0].kind == assertion_kind::assert);
  assert(contains(violation_log()[0].comment, "x != 42"));
  assert(violation_log()[0].line == g_line + 1);
  assert(violation_log()[1].kind == assertion_kind::post);
}

int main(int, char**) {
  test_evaluation();
  test_violations();
  return 0;
}
