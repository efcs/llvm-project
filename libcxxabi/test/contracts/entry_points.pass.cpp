//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23

// The entry points of the contract-violation runtime dispatch a violation to
// the contract-violation handler. The specialized entry points prepend an
// override block with the assertion kind, evaluation semantic and detection
// mode encoded in their name, which takes precedence over the chain; the
// universal entry point uses the chain as is. The 'observe' entry points
// return, and the handler sees the compiler's data through the chain.

#include <cassert>
#include <cstdint>
#include <cstring>

#include "support.h"

using namespace __cxxabiv1;

struct Seen {
  int calls = 0;
  std::uint8_t kind, semantic, mode;
  void* exception;
  const char* comment;
  const char* message;
  const __cxa_source_location* location;
} seen;

CONTRACTS_TEST_HANDLER() {
  Violation v = *static_cast<const Violation*>(violation_ptr);
  ++seen.calls;
  seen.kind      = v.kind();
  seen.semantic  = v.semantic();
  seen.mode      = v.mode();
  seen.exception = v.exception();
  seen.comment   = v.comment();
  seen.message   = v.message();
  seen.location  = v.location();
}

int main(int, char**) {
  BasicBlock block = make_basic_block("file.cpp", "int f(int)", 42, 7, "x > 0", "the message");

  struct Expected {
    void (*entry)(void*);
    std::uint8_t kind, semantic, mode;
  };
  const Expected expected[] = {
      {__cxa_contract_violation_pre_observe_pf, CXA_AK_PRE, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_pre_observe_pf_noexcept, CXA_AK_PRE, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_pre_observe_ex, CXA_AK_PRE, CXA_ES_OBSERVE, CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_pre_observe_ex_noexcept, CXA_AK_PRE, CXA_ES_OBSERVE, CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_post_observe_pf, CXA_AK_POST, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_post_observe_pf_noexcept, CXA_AK_POST, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_post_observe_ex, CXA_AK_POST, CXA_ES_OBSERVE, CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_post_observe_ex_noexcept, CXA_AK_POST, CXA_ES_OBSERVE, CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_assert_observe_pf, CXA_AK_ASSERT, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_assert_observe_pf_noexcept, CXA_AK_ASSERT, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_assert_observe_ex, CXA_AK_ASSERT, CXA_ES_OBSERVE, CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_assert_observe_ex_noexcept, CXA_AK_ASSERT, CXA_ES_OBSERVE, CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_post_capture_observe_pf, CXA_AK_POST_CAPTURE, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_post_capture_observe_pf_noexcept,
       CXA_AK_POST_CAPTURE,
       CXA_ES_OBSERVE,
       CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_post_capture_observe_ex, CXA_AK_POST_CAPTURE, CXA_ES_OBSERVE, CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_post_capture_observe_ex_noexcept,
       CXA_AK_POST_CAPTURE,
       CXA_ES_OBSERVE,
       CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_implicit_observe_pf, CXA_AK_IMPLICIT, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_implicit_observe_pf_noexcept, CXA_AK_IMPLICIT, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE},
      {__cxa_contract_violation_implicit_observe_ex, CXA_AK_IMPLICIT, CXA_ES_OBSERVE, CXA_DM_EVALUATION_EXCEPTION},
      {__cxa_contract_violation_implicit_observe_ex_noexcept,
       CXA_AK_IMPLICIT,
       CXA_ES_OBSERVE,
       CXA_DM_EVALUATION_EXCEPTION},
  };

  int calls = 0;
  for (const Expected& e : expected) {
    seen = Seen();
    e.entry(block.data());
    assert(seen.calls == 1);
    ++calls;
    assert(seen.kind == e.kind);
    assert(seen.semantic == e.semantic);
    assert(seen.mode == e.mode);
    // The exception the predicate exited with is recorded by the 'ex' entry
    // points; here there is none.
    assert(seen.exception == nullptr);
    // The rest of the data comes from the compiler's block.
    assert(seen.comment == block.comment);
    assert(seen.message == block.message);
    assert(seen.location == &block.location);
  }
  assert(calls == 20);

  // The override block takes precedence over kind, semantic and mode fields in
  // the chain.
  {
    KindBlock kinds = make_kind_block(CXA_AK_POST, CXA_ES_ENFORCE, CXA_DM_EVALUATION_EXCEPTION, block.get());
    seen            = Seen();
    __cxa_contract_violation_pre_observe_pf(kinds.data());
    assert(seen.calls == 1);
    assert(seen.kind == CXA_AK_PRE);
    assert(seen.semantic == CXA_ES_OBSERVE);
    assert(seen.mode == CXA_DM_PREDICATE_FALSE);
    assert(seen.comment == block.comment);
  }

  // The universal entry points take everything from the chain, and return when
  // the semantic is not a terminating one, including when it is absent.
  {
    KindBlock kinds = make_kind_block(CXA_AK_ASSERT, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE, block.get());
    seen            = Seen();
    __cxa_contract_violation(kinds.data());
    assert(seen.calls == 1);
    assert(seen.kind == CXA_AK_ASSERT);
    assert(seen.semantic == CXA_ES_OBSERVE);
    assert(seen.mode == CXA_DM_PREDICATE_FALSE);
    assert(seen.comment == block.comment);

    seen = Seen();
    __cxa_contract_violation_noexcept(kinds.data());
    assert(seen.calls == 1);
    assert(seen.kind == CXA_AK_ASSERT);

    seen = Seen();
    __cxa_contract_violation(block.data());
    assert(seen.calls == 1);
    assert(seen.kind == CXA_AK_UNSPECIFIED);
    assert(seen.semantic == CXA_ES_UNSPECIFIED);
    assert(seen.mode == CXA_DM_UNSPECIFIED);
    assert(seen.comment == block.comment);
  }

  // A block without the comment and message fields yields the defaults.
  {
    KindBlock kinds = make_kind_block(CXA_AK_PRE, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE);
    seen            = Seen();
    __cxa_contract_violation(kinds.data());
    assert(seen.calls == 1);
    assert(std::strcmp(seen.comment, "") == 0);
    assert(std::strcmp(seen.message, "") == 0);
    assert(seen.location == nullptr);
  }

  return 0;
}
