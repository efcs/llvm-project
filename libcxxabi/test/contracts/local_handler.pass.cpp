//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23

// A data block with a P3400 local handler (the trampoline a compiler emits for
// an assertion with an assertion-control object) and its label: the runtime
// calls the local handler before the global handler, with the label and the
// address of the contract_violation, and calls the global handler only if the
// local handler did not report the violation as handled (specification
// section 6.1, step 2). Clang does not emit such blocks; this is what the
// runtime does with the blocks of other compilers.

#include <cassert>
#include <csignal>
#include <cstdint>
#include <cstdlib>

#include "support.h"

using namespace __cxxabiv1;

// The "label" layout the compiler emits: the basic layout plus the local
// handler and the label pointer (specification section 5.5).
struct LabelBlock {
  const __cxa_descriptor_table_t* descriptor;
  const __cxa_contract_data_block* next;
  __cxa_source_location location;
  const char* comment;
  const char* message;
  __cxa_local_handler_fn_t local_handler;
  const void* label_ptr;

  void* data() const { return const_cast<LabelBlock*>(this); }
};

const DescriptorTable<5> label_descriptor = {
    kClangHeader,
    5,
    {CXA_FIELD_SOURCE_LOCATION, CXA_FIELD_COMMENT, CXA_FIELD_MESSAGE, CXA_FIELD_LOCAL_HANDLER, CXA_FIELD_LABEL_PTR},
    {},
    {{offsetof(LabelBlock, location)},
     {offsetof(LabelBlock, comment)},
     {offsetof(LabelBlock, message)},
     {offsetof(LabelBlock, local_handler)},
     {offsetof(LabelBlock, label_ptr)}},
};

struct Label {
  int handled_result;
  int calls = 0;
};

int local_calls         = 0;
int global_calls        = 0;
const void* seen_label  = nullptr;
std::uint8_t seen_kind  = 0;
const void* seen_violation_in_local;
const void* seen_violation_in_global;

int local_handler(const void* label_ptr, const void* violation_ptr) {
  ++local_calls;
  seen_label              = label_ptr;
  seen_violation_in_local = violation_ptr;
  Violation v             = *static_cast<const Violation*>(violation_ptr);
  seen_kind               = v.kind();
  Label* label            = const_cast<Label*>(static_cast<const Label*>(label_ptr));
  ++label->calls;
  return label->handled_result;
}

CONTRACTS_TEST_HANDLER() {
  ++global_calls;
  seen_violation_in_global = violation_ptr;
}

int main(int, char**) {
  Label handles  = {1};
  Label declines = {0};

  LabelBlock handled = {
      label_descriptor.get(), nullptr, {"file.cpp", "f", 1, 1}, "x > 0", nullptr, &local_handler, &handles};
  LabelBlock declined = {
      label_descriptor.get(), nullptr, {"file.cpp", "f", 2, 1}, "x > 0", nullptr, &local_handler, &declines};
  LabelBlock no_local = {label_descriptor.get(), nullptr, {"file.cpp", "f", 3, 1}, "x > 0", nullptr, nullptr, &handles};

  // The local handler handles the violation: the global handler is not called.
  __cxa_contract_violation_pre_observe_pf(handled.data());
  assert(local_calls == 1);
  assert(global_calls == 0);
  assert(handles.calls == 1);
  assert(seen_label == &handles);
  // It sees the violation as the global handler would, with the override block
  // of the entry point in the chain.
  assert(seen_kind == CXA_AK_PRE);

  // The local handler declines: the global handler is called with the same
  // violation.
  __cxa_contract_violation_assert_observe_pf_noexcept(declined.data());
  assert(local_calls == 2);
  assert(global_calls == 1);
  assert(declines.calls == 1);
  assert(seen_label == &declines);
  assert(seen_kind == CXA_AK_ASSERT);
  assert(seen_violation_in_local == seen_violation_in_global);

  // A null local handler is ignored.
  __cxa_contract_violation(no_local.data());
  assert(local_calls == 2);
  assert(global_calls == 2);

  // The universal entry point dispatches the same way.
  KindBlock kinds = make_kind_block(CXA_AK_POST, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE,
                                    reinterpret_cast<const __cxa_contract_data_block*>(&handled));
  __cxa_contract_violation(kinds.data());
  assert(local_calls == 3);
  assert(global_calls == 2);
  assert(seen_kind == CXA_AK_POST);

  // A handled violation of an enforced assertion still terminates the program.
  std::signal(SIGABRT, [](int) { std::_Exit(0); });
  __cxa_contract_violation_pre_enforce_pf(handled.data());
  return 1;
}
