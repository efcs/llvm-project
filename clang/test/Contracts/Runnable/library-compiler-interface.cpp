// RUN: %clangxx -fcontracts -std=c++26 -fcontract-evaluation-semantic=observe \
// RUN:    -fcontract-group-evaluation-semantic=observe=observe,enforce=enforce,ignore=ignore,quick_enforce=quick_enforce %s -o %t  -g
// RUN:  %t 0
// RUN: %t 1

// Checks the interface between the compiler and the contract-violation
// runtime: the Itanium C++ Contracts ABI entry points that Clang calls, and
// the data block it passes to them. This test provides the entry points
// itself, and checks the data block against the expected layout.

#include "contracts.h"
#include "my_assert.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// The basic data block layout emitted by Clang.
struct DescriptorTable {
  uint8_t header;      // (version << 4) | vendor
  uint8_t num_entries; // 3
  uint8_t field_ids[3];
  uint8_t padding[alignof(uintptr_t) - (5 % alignof(uintptr_t))];
  uintptr_t offsets[3];
};

struct DataBlock {
  const DescriptorTable* descriptor;
  const DataBlock* next;
  const char* file;
  const char* function;
  unsigned line;
  unsigned column;
  const char* comment;
  const char* message;
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
assertion_kind expected_kind;
std::source_location expected_loc;
int expected_line_offset = 0;
unsigned handler_called = 0;

void check_data_block(const DataBlock* block) {
  assert(block->next == nullptr);

  // The descriptor table says which fields the block has, and where.
  const DescriptorTable* desc = block->descriptor;
  assert(desc->header == 0x12); // version 1, vendor Clang (2)
  assert(desc->num_entries == 3);
  assert(desc->field_ids[0] == 0x01); // source location
  assert(desc->field_ids[1] == 0x02); // comment
  assert(desc->field_ids[2] == 0x03); // message
  assert(desc->offsets[0] == __builtin_offsetof(DataBlock, file));
  assert(desc->offsets[1] == __builtin_offsetof(DataBlock, comment));
  assert(desc->offsets[2] == __builtin_offsetof(DataBlock, message));

  std::source_location loc = std::source_location::__create_from_pointer(&block->file);
  assert(expected_loc.file_name());
  assert(location_equals(loc, expected_loc, expected_line_offset));
  assert(strcmp(block->comment, "check_evaluated(false)") == 0 ||
         strcmp(block->comment, "check_evaluated(x)") == 0);
  assert(strcmp(block->message, "") == 0);
}

void violation(void* data, assertion_kind kind, evaluation_semantic sem, detection_mode mode) {
  ++handler_called;
  assert(kind == expected_kind);
  assert(sem == expected_semantic);
  assert(mode == expected_mode);
  check_data_block(static_cast<const DataBlock*>(data));
  if (sem == evaluation_semantic::enforce) {
    exit(0);
  }
}

#define ENTRY_POINT(name, kind, sem, mode)                                                         \
  extern "C" void __cxa_contract_violation_##name(void* data) {                                    \
    violation(data, kind, sem, mode);                                                              \
  }                                                                                                \
  extern "C" void __cxa_contract_violation_##name##_noexcept(void* data) noexcept {                \
    violation(data, kind, sem, mode);                                                              \
  }

ENTRY_POINT(assert_observe_pf, assertion_kind::assert, evaluation_semantic::observe, detection_mode::predicate_false)
ENTRY_POINT(assert_enforce_pf, assertion_kind::assert, evaluation_semantic::enforce, detection_mode::predicate_false)
ENTRY_POINT(pre_observe_pf, assertion_kind::pre, evaluation_semantic::observe, detection_mode::predicate_false)
ENTRY_POINT(pre_enforce_pf, assertion_kind::pre, evaluation_semantic::enforce, detection_mode::predicate_false)

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
  expected_kind = assertion_kind::assert;
  expected_semantic = evaluation_semantic::observe;
  expect_location(+1);
  contract_assert
      [[clang::contract_group("observe")]]
  (check_evaluated(false));
  assert(evaluated_count == 1);
  assert(handler_called == 1);
  reset_counters();

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
  expected_kind = assertion_kind::pre;
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
