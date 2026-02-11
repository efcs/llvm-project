// RUN: %clang -fcontracts -std=c++20 -nostdlib++ -fcontract-evaluation-semantic=observe \
// RUN:    -fcontract-group-evaluation-semantic=observe=observe,enforce=enforce,ignore=ignore,quick_enforce=quick_enforce %s -o %t  -g
// RUN:  %t 0
// RUN: %t 1



#define CONTRACT_GROUP(x)

#include "contracts.h"
#include "my_assert.h"
#include <stdio.h>
#include <string.h>

// This is the layout of the data emitted by clang for version 3 of callback
struct BuiltinContractStruct {
  enum { VERSION = 3 };
  unsigned version; // the version of the struct
  const char* file;
  const char* function;
  unsigned lineno = 0;
  unsigned column = 0;
  const char* comment;
  unsigned contract_kind;
};



bool location_equals(std::source_location LHS, std::source_location RHS, int rhs_offset = 0) {
  bool value = __builtin_strcmp(LHS.file_name(), RHS.file_name()) == 0
  && __builtin_strcmp(LHS.function_name(), RHS.function_name()) == 0
  && LHS.line() == RHS.line() + rhs_offset;
  if (!value) {
    const char* fmt_str = R"cpp(
Assertion Failed: Source locations not equal
Actual  : %s:%d in %s
Expected: %s:%d in %s
)cpp";
    fprintf(stderr, fmt_str, LHS.file_name(), (int)LHS.line(), LHS.function_name(),
            RHS.file_name(), (int)RHS.line() + rhs_offset, RHS.function_name());
  }
  return value;
}

using namespace std::contracts;


evaluation_semantic expected_semantic;
detection_mode expected_mode = detection_mode::predicate_false;
std::source_location expected_loc;
int expected_line_offset = 0;
unsigned handler_called = 0;



extern "C" void __handle_contract_violation_v3(unsigned __sem, unsigned __mode, void *data) {
  using namespace std::contracts;
  ++handler_called;
  assert(__sem == static_cast<unsigned>(expected_semantic));
  evaluation_semantic sem = static_cast<evaluation_semantic>(__sem);
  assert(__mode == static_cast<unsigned>(detection_mode::predicate_false) ||
         __mode == static_cast<unsigned>(detection_mode::evaluation_exception));
  assert(__mode == static_cast<unsigned>(expected_mode));
  detection_mode mode = static_cast<detection_mode>(__mode);
  BuiltinContractStruct *cs = (BuiltinContractStruct*)data;
  void* SourceLoc = reinterpret_cast<char*>(cs) + __builtin_offsetof(BuiltinContractStruct, file);
  std::source_location loc = std::source_location::__create_from_pointer(SourceLoc);
  assert(expected_loc.file_name());
  assert(location_equals(loc, expected_loc, expected_line_offset));
  _ContractViolationImpl impl{.kind = static_cast<assertion_kind>(cs->contract_kind),
                              .semantic = sem,
                              .mode = mode,
                              .comment = cs->comment};
  if (sem == evaluation_semantic::enforce && expected_semantic == evaluation_semantic::enforce) {
    exit(0);
  }
}

void expect_location(int offset, std::source_location loc = std::source_location::current()) {
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