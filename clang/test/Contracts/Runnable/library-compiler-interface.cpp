// REQUIRES: contracts-libcxx
// RUN: %clangxx -fcontracts -std=c++26 -fcontract-evaluation-semantic=observe \
// RUN:    -fcontract-group-evaluation-semantic=observe=observe,enforce=enforce,ignore=ignore,quick_enforce=quick_enforce \
// RUN:    %s %libcxx_flags -o %t -g
// RUN:  %t 0
// RUN: %t 1

// Test that the compiler-emitted contract violation call passes correct
// field values through to the user's handle_contract_violation handler.
// Tests the public std::contracts::contract_violation API, not the internal
// ABI data layout.

#include <contracts>
#include "my_assert.h"
#include <stdio.h>
#include <string.h>

using namespace std::contracts;


bool location_equals(std::source_location LHS, std::source_location RHS,
                     int rhs_offset = 0) {
  bool value = __builtin_strcmp(LHS.file_name(), RHS.file_name()) == 0
  && __builtin_strcmp(LHS.function_name(), RHS.function_name()) == 0
  && LHS.line() == RHS.line() + rhs_offset;
  if (!value) {
    const char* fmt_str = R"cpp(
Assertion Failed: Source locations not equal
Actual  : %s:%d in %s
Expected: %s:%d in %s
)cpp";
    fprintf(stderr, fmt_str,
            LHS.file_name(), (int)LHS.line(), LHS.function_name(),
            RHS.file_name(), (int)RHS.line() + rhs_offset,
            RHS.function_name());
  }
  return value;
}


evaluation_semantic expected_semantic;
detection_mode expected_mode = detection_mode::predicate_false;
std::source_location expected_loc;
int expected_line_offset = 0;
unsigned handler_called = 0;


void handle_contract_violation(const contract_violation& v) {
  ++handler_called;

  assert(v.semantic() == expected_semantic);

  assert(v.detection_mode() == detection_mode::predicate_false ||
         v.detection_mode() == detection_mode::evaluation_exception);
  assert(v.detection_mode() == expected_mode);

  assert(expected_loc.file_name());
  assert(location_equals(v.location(), expected_loc, expected_line_offset));

  if (v.semantic() == evaluation_semantic::enforce &&
      expected_semantic == evaluation_semantic::enforce) {
    exit(0);
  }
}

void expect_location(int offset,
                     std::source_location loc = std::source_location::current()) {
  ::expected_loc = loc;
  expected_line_offset = offset;
}

unsigned evaluated_count;
bool check_evaluated(bool value) {
  ++evaluated_count;
  return value;
}

void reset_counters() {
  evaluated_count = 0;
  handler_called = 0;
}

void test_contract_assert() {
  expected_semantic = evaluation_semantic::observe;
  expect_location(+1);
  contract_assert
      [[clang::contract_group("observe")]]
  (check_evaluated(false));
  assert(evaluated_count == 1);
  assert(handler_called == 1);
  reset_counters();

  int was_evaluated = 0;
  contract_assert
    [[clang::contract_group("enforce")]]
  (check_evaluated(true));
  assert(handler_called == 0);
  assert(evaluated_count == 1);
  reset_counters();

  contract_assert
    [[clang::contract_group("ignore")]]
  (check_evaluated(false));
  assert(handler_called == 0);
  assert(evaluated_count == 0);

  expected_semantic = evaluation_semantic::enforce;
  expect_location(+1);
  contract_assert
    [[clang::contract_group("enforce")]]
  (check_evaluated(false));
  __builtin_abort();
}

enum Semantic {
  S_Ignore,
  S_Enforce,
  S_Observe
};

void has_pre_ignore(bool x)
  pre [[clang::contract_group("ignore")]] (check_evaluated(x)) {
  expect_location(-1);
}


void has_pre_observe(bool x)
  pre [[clang::contract_group("observe")]] (check_evaluated(x)) {
  expect_location(-1);
}

void has_pre_enforce(bool x)
  pre [[clang::contract_group("enforce")]] (check_evaluated(x)) {
  expect_location(-1);
}

void test_pre_condition() {
  expected_semantic = evaluation_semantic::observe;
  has_pre_observe(true); // this sets the expected location
  assert(evaluated_count == 1);
  assert(handler_called == 0);
  reset_counters();

  expected_semantic = evaluation_semantic::observe;

  has_pre_observe(false);
  assert(handler_called == 1);
  assert(evaluated_count == 1);
  reset_counters();

  has_pre_enforce(true);
  assert(handler_called == 0);
  assert(evaluated_count == 1);
  reset_counters();

  has_pre_ignore(false);
  assert(handler_called == 0);
  assert(evaluated_count == 0);

  expected_semantic = evaluation_semantic::enforce;
  has_pre_enforce(true);
  has_pre_enforce(false);
  __builtin_abort();
}

int main(int argc, char **argv) {
  assert(argc == 2);
  int test_case = atoi(argv[1]);

  switch (test_case) {
  case 0:
    test_contract_assert();
    break;
  case 1:
    test_pre_condition();
    break;
  default:
    assert(false && "Not a valid test case");
  }
}
