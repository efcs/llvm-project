// -*- C++ -*-
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// The contract-violation runtime of the Itanium C++ Contracts ABI
// (https://github.com/notadragon/efcs_contracts-abi-specification).
//
// When a contract assertion is violated, the compiler calls one of the entry
// points below with the address of a chain of data blocks describing the
// violation (see <__contracts/abi.h>). An entry point dispatches the violation
// to the contract-violation handler: the program's replacement of
// ::handle_contract_violation if it has one, otherwise the default handler,
// which prints a diagnostic. If the evaluation semantic is 'enforce', the
// program is then terminated.
//
// The specification splits the runtime into a core tier (chain walking, the
// dispatch and the entry points that may propagate exceptions) and a C++ tier
// (the '_noexcept' entry points, which need an exception-handling barrier, and
// the default handler). This file provides both, and is compiled into the
// library that hosts the runtime:
//
//  * libc++abi, when libc++ is built on it (libcxxabi/src/cxa_contracts.cpp):
//    LLVM's C++ runtime library, and keeping the dispatch and the default
//    handler together means nothing here calls up into libc++, which matters
//    for static linking and for Mach-O's two-level namespace.
//
//  * libc++ itself, when it is built on another ABI library (libcxxrt,
//    libsupc++, vcruntime, or none; libcxx/src/contracts.cpp), as is done for
//    operator new. The runtime only needs the ABI library for the current
//    exception, which it reaches through std::current_exception() when the
//    library has no __cxa_current_primary_exception.
//
// std::contracts::contract_violation and
// std::contracts::invoke_default_contract_violation_handler live in libc++ and
// call down into this runtime. The symbols defined here are declared in
// <__contracts/abi.h>, with the export annotation of the hosting library.
//
//===----------------------------------------------------------------------===//

#include <__config>
#include <__contracts/abi.h>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <exception>

using namespace __cxxabiv1;

//===----------------------------------------------------------------------===//
// Chain walking
//===----------------------------------------------------------------------===//

extern "C" _LIBCPP_CONTRACTS_RUNTIME_VIS const void*
__cxa_find_field(const __cxa_contract_data_block* chain, const uint8_t* ids, uint8_t num_ids) {
  // First-found-wins: the first block of the chain that has any of the
  // requested fields provides it. Fields with unknown IDs, and blocks without
  // a descriptor, are skipped.
  for (const __cxa_contract_data_block* block = chain; block; block = block->next) {
    const __cxa_descriptor_table_t* desc = block->descriptor;
    if (!desc)
      continue;
    const __cxa_descriptor_data_t* data = desc->data();
    for (uint8_t i = 0; i < desc->num_entries; ++i) {
      for (uint8_t j = 0; j < num_ids; ++j) {
        if (desc->field_ids[i] != ids[j])
          continue;
        if (desc->field_ids[i] >= CXA_FIELD_EXTENDED)
          return data[i].pointer;
        return reinterpret_cast<const char*>(block) + data[i].offset;
      }
    }
  }
  return nullptr;
}

namespace {

// The value of a string field, or nullptr if the chain has no such field or
// the field is null. The compiler leaves the message null when the assertion
// has none, which is distinct from an empty message.
const char* find_string(const __cxa_contract_data_block* chain, uint8_t id) {
  const char* const* p = __cxa_find_field_ptr<const char*>(chain, id);
  return p ? *p : nullptr;
}

//===----------------------------------------------------------------------===//
// The contract_violation object
//===----------------------------------------------------------------------===//

// std::contracts::contract_violation holds nothing but the chain pointer, so
// this has the same layout and the handlers can be called with its address.
struct violation_t {
  const __cxa_contract_data_block* chain;
};

//===----------------------------------------------------------------------===//
// Override blocks
//===----------------------------------------------------------------------===//

// A specialized entry point knows the assertion kind, evaluation semantic and
// detection mode from its name. It makes them available to the accessors by
// prepending an override block with these three fields to the chain it was
// called with; since the first block that has a field provides it, they take
// precedence over anything in the compiler-emitted block. The entry points for
// a predicate that exited via an exception also record the exception, so that
// contract_violation::evaluation_exception() does not depend on what the
// current exception is when the handler calls it.
//
// The descriptor tables of the override blocks are laid out by hand with the
// padding the ABI requires between the field IDs and the data entries.
template <size_t N>
struct override_descriptor_t {
  uint8_t header;
  uint8_t num_entries;
  uint8_t field_ids[N];
  uint8_t padding[(2 + N + alignof(__cxa_descriptor_data_t) - 1) / alignof(__cxa_descriptor_data_t) *
                      alignof(__cxa_descriptor_data_t) -
                  (2 + N)];
  __cxa_descriptor_data_t data[N];
};

template <size_t N>
constexpr bool descriptor_is_well_formed =
    offsetof(override_descriptor_t<N>, data) ==
    (2 + N + alignof(__cxa_descriptor_data_t) - 1) / alignof(__cxa_descriptor_data_t) *
        alignof(__cxa_descriptor_data_t);

struct override_block_t {
  const __cxa_descriptor_table_t* descriptor;
  const __cxa_contract_data_block* next;
  uint8_t kind;
  uint8_t semantic;
  uint8_t mode;
  void* exception; // CXA_FIELD_EXCEPTION_PTR; only described by the 'ex' descriptor
};

constexpr uint8_t generic_header = (1u << 4) | CXA_VENDOR_GENERIC; // version 1

static_assert(descriptor_is_well_formed<3> && descriptor_is_well_formed<4>, "");

constexpr override_descriptor_t<3> override_descriptor_pf = {
    generic_header,
    3,
    {CXA_FIELD_ASSERTION_KIND, CXA_FIELD_EVALUATION_SEMANTIC, CXA_FIELD_DETECTION_MODE},
    {},
    {{offsetof(override_block_t, kind)}, {offsetof(override_block_t, semantic)}, {offsetof(override_block_t, mode)}},
};

constexpr override_descriptor_t<4> override_descriptor_ex = {
    generic_header,
    4,
    {CXA_FIELD_ASSERTION_KIND, CXA_FIELD_EVALUATION_SEMANTIC, CXA_FIELD_DETECTION_MODE, CXA_FIELD_EXCEPTION_PTR},
    {},
    {{offsetof(override_block_t, kind)},
     {offsetof(override_block_t, semantic)},
     {offsetof(override_block_t, mode)},
     {offsetof(override_block_t, exception)}},
};

//===----------------------------------------------------------------------===//
// Handler resolution
//===----------------------------------------------------------------------===//

// The program's replacement of ::handle_contract_violation, if any, is reached
// through the C-linkage symbol __handle_contract_violation, which the compiler
// defines as an alias of the replacement. The dispatch must find out whether
// the program defines it, which the object formats support differently.
#if defined(__MACH__)
// Mach-O's two-level namespace cannot express a reference to a symbol that
// the program may or may not define. Instead, this is a weak definition that
// the program's strong definition overrides (as for operator new): calls bind
// to the first definition dyld finds, which is the program's when it has one.
extern "C" _LIBCPP_CONTRACTS_RUNTIME_VIS __attribute__((__weak__)) void
__handle_contract_violation(const void* violation) {
  __contract_invoke_default_handler(violation);
}
#elif defined(_WIN32)
// FIXME: Neither weak references nor weak definitions resolve across DLL
// boundaries on Windows. Making the handler replaceable there needs the
// dispatch to live in a static library (so that /alternatename can select the
// default), or a registration mechanism that the compiler emits with the
// alias. Until then, the default handler is always used.
#else
extern "C" __attribute__((__weak__)) void __handle_contract_violation(const void* violation);
#endif

void call_handler(const violation_t& violation) {
#if defined(_WIN32)
  __contract_invoke_default_handler(&violation);
#else
  if (&__handle_contract_violation)
    __handle_contract_violation(&violation);
  else
    __contract_invoke_default_handler(&violation);
#endif
}

//===----------------------------------------------------------------------===//
// Dispatch
//===----------------------------------------------------------------------===//

// Dispatch a violation to the handler, and terminate the program when the
// handler returns from a violation with a terminating semantic.
//
// An assertion with a P3400 assertion-control object has a local handler in
// its data block (a trampoline the compiler emits, with the address of the
// control object as its label), which is called before the global handler and
// may report that it handled the violation, in which case the global handler
// is not called (specification section 6.1, step 2).
void dispatch(const __cxa_contract_data_block* chain, uint8_t semantic) {
  violation_t violation = {chain};
  bool handled          = false;
  if (__cxa_local_handler_fn_t local_handler =
          __cxa_find_field_value<__cxa_local_handler_fn_t>(chain, CXA_FIELD_LOCAL_HANDLER, nullptr)) {
    const void* label = __cxa_find_field_value<const void*>(chain, CXA_FIELD_LABEL_PTR, nullptr);
    handled           = local_handler(label, &violation) != 0;
  }
  if (!handled)
    call_handler(violation);
  if (semantic == CXA_ES_ENFORCE || semantic == CXA_ES_QUICK_ENFORCE)
    std::abort();
}

// Dispatch under the noexcept barrier: a handler that exits via an exception
// terminates the program, with the exception still active for the terminate
// handler to inspect.
void dispatch_noexcept(const __cxa_contract_data_block* chain, uint8_t semantic) noexcept {
#if _LIBCPP_HAS_EXCEPTIONS
  try {
    dispatch(chain, semantic);
  } catch (...) {
    std::terminate();
  }
#else
  dispatch(chain, semantic);
#endif
}

// Holds a reference to the exception the predicate exited with (the current
// exception when an 'ex' entry point is called), for the duration of the
// dispatch.
#if defined(_LIBCPPABI_VERSION) || defined(LIBCXXRT)
// The ABI library has the exception reference counting API: record the
// exception object (as a std::exception_ptr refers to it) in the override
// block, where contract_violation::evaluation_exception() finds it.
struct current_exception_holder {
  void* exception = __cxa_current_primary_exception();
  ~current_exception_holder() { __cxa_decrement_exception_refcount(exception); }
};
#else
// Otherwise the exception is only kept alive, and
// contract_violation::evaluation_exception() falls back to
// std::current_exception().
struct current_exception_holder {
  std::exception_ptr held = std::current_exception();
  void* exception         = nullptr;
};
#endif

template <bool IsNoexcept, bool IsException>
void dispatch_with_override(void* data, uint8_t kind, uint8_t semantic, uint8_t mode) {
  override_block_t block = {
      reinterpret_cast<const __cxa_descriptor_table_t*>(IsException ? static_cast<const void*>(&override_descriptor_ex)
                                                                    : static_cast<const void*>(&override_descriptor_pf)),
      static_cast<const __cxa_contract_data_block*>(data),
      kind,
      semantic,
      mode,
      nullptr,
  };
  const __cxa_contract_data_block* chain = reinterpret_cast<const __cxa_contract_data_block*>(&block);
  auto dispatch_chain                    = [&] {
    if (IsNoexcept)
      dispatch_noexcept(chain, semantic);
    else
      dispatch(chain, semantic);
  };
  if constexpr (IsException) {
    // Only the 'ex' entry points are called from within a handler for the
    // exception the predicate exited with; the 'pf' entry points may be called
    // from within a handler for an unrelated exception, which is left alone.
    current_exception_holder exception;
    block.exception = exception.exception;
    dispatch_chain();
  } else {
    dispatch_chain();
  }
}

} // namespace

//===----------------------------------------------------------------------===//
// The default handler
//===----------------------------------------------------------------------===//

extern "C" _LIBCPP_CONTRACTS_RUNTIME_VIS void __contract_invoke_default_handler(const void* violation) {
  const __cxa_contract_data_block* chain = static_cast<const violation_t*>(violation)->chain;

  const char* file     = "";
  const char* function = "";
  unsigned line        = 0;
  if (const __cxa_source_location* loc = __cxa_find_field_ptr<__cxa_source_location>(chain, CXA_FIELD_SOURCE_LOCATION)) {
    file     = loc->file_name ? loc->file_name : "";
    function = loc->function_name ? loc->function_name : "";
    line     = loc->line;
  }

  char unknown_kind[32];
  const char* kind         = unknown_kind;
  const uint8_t kind_value = __cxa_find_field_value<uint8_t>(chain, CXA_FIELD_ASSERTION_KIND, CXA_AK_UNSPECIFIED);
  switch (kind_value) {
  case CXA_AK_PRE:
    kind = "pre";
    break;
  case CXA_AK_POST:
  case CXA_AK_POST_CAPTURE:
    kind = "post";
    break;
  case CXA_AK_ASSERT:
    kind = "contract_assert";
    break;
  case CXA_AK_MANUAL:
    kind = "manual";
    break;
  case CXA_AK_CASSERT:
    kind = "assert";
    break;
  case CXA_AK_IMPLICIT:
    kind = "implicit";
    break;
  default:
    std::snprintf(unknown_kind, sizeof(unknown_kind), "<unknown kind %u>", static_cast<unsigned>(kind_value));
    break;
  }

  char unknown_semantic[32];
  const char* semantic         = unknown_semantic;
  const uint8_t semantic_value = __cxa_find_field_value<uint8_t>(chain, CXA_FIELD_EVALUATION_SEMANTIC, CXA_ES_UNSPECIFIED);
  switch (semantic_value) {
  case CXA_ES_IGNORE:
    semantic = "ignore";
    break;
  case CXA_ES_OBSERVE:
    semantic = "observe";
    break;
  case CXA_ES_ENFORCE:
    semantic = "enforce";
    break;
  case CXA_ES_QUICK_ENFORCE:
    semantic = "quick_enforce";
    break;
  default:
    std::snprintf(
        unknown_semantic, sizeof(unknown_semantic), "<unknown semantic %u>", static_cast<unsigned>(semantic_value));
    break;
  }

  const char* cause =
      __cxa_find_field_value<uint8_t>(chain, CXA_FIELD_DETECTION_MODE, CXA_DM_UNSPECIFIED) == CXA_DM_EVALUATION_EXCEPTION
          ? "predicate evaluation exited via an exception"
          : "predicate evaluated to false";

  // The assertion is described as kind(comment: "message"), without the parts
  // the chain lacks: a null or empty message is absent, and the comment is
  // absent when it is empty (an assertion whose message replaces its text).
  const char* comment = find_string(chain, CXA_FIELD_COMMENT);
  const char* message = find_string(chain, CXA_FIELD_MESSAGE);
  if (!comment)
    comment = "";
  if (!message)
    message = "";
  const char* separator = *comment && *message ? ": " : "";

  std::fprintf(stderr,
               "%s:%u: %s%scontract violation in %s(%s%s%s%s%s): %s (semantic: %s)\n",
               *file ? file : "<unknown file>",
               line,
               function,
               *function ? ": " : "",
               kind,
               comment,
               separator,
               *message ? "\"" : "",
               message,
               *message ? "\"" : "",
               cause,
               semantic);
  std::fflush(stderr);
}

//===----------------------------------------------------------------------===//
// Entry points
//===----------------------------------------------------------------------===//

extern "C" _LIBCPP_CONTRACTS_RUNTIME_VIS void __cxa_contract_violation(void* data) {
  const __cxa_contract_data_block* chain = static_cast<const __cxa_contract_data_block*>(data);
  dispatch(chain, __cxa_find_field_value<uint8_t>(chain, CXA_FIELD_EVALUATION_SEMANTIC, CXA_ES_UNSPECIFIED));
}

extern "C" _LIBCPP_CONTRACTS_RUNTIME_VIS void __cxa_contract_violation_noexcept(void* data) noexcept {
  const __cxa_contract_data_block* chain = static_cast<const __cxa_contract_data_block*>(data);
  dispatch_noexcept(chain, __cxa_find_field_value<uint8_t>(chain, CXA_FIELD_EVALUATION_SEMANTIC, CXA_ES_UNSPECIFIED));
}

#define CONTRACT_VIOLATION_OBSERVE_ENTRY_POINT(name, kind, mode, is_exception)                                        \
  extern "C" _LIBCPP_CONTRACTS_RUNTIME_VIS void __cxa_contract_violation_##name(void* data) {                          \
    dispatch_with_override<false, is_exception>(data, kind, CXA_ES_OBSERVE, mode);                                     \
  }                                                                                                                    \
  extern "C" _LIBCPP_CONTRACTS_RUNTIME_VIS void __cxa_contract_violation_##name##_noexcept(void* data) noexcept {      \
    dispatch_with_override<true, is_exception>(data, kind, CXA_ES_OBSERVE, mode);                                      \
  }

#define CONTRACT_VIOLATION_ENFORCE_ENTRY_POINT(name, kind, mode, is_exception)                                        \
  extern "C" [[noreturn]] _LIBCPP_CONTRACTS_RUNTIME_VIS void __cxa_contract_violation_##name(void* data) {             \
    dispatch_with_override<false, is_exception>(data, kind, CXA_ES_ENFORCE, mode);                                     \
    __builtin_unreachable();                                                                                           \
  }                                                                                                                    \
  extern "C" [[noreturn]] _LIBCPP_CONTRACTS_RUNTIME_VIS void __cxa_contract_violation_##name##_noexcept(void* data)    \
      noexcept {                                                                                                       \
    dispatch_with_override<true, is_exception>(data, kind, CXA_ES_ENFORCE, mode);                                      \
    __builtin_unreachable();                                                                                           \
  }

#define CONTRACT_VIOLATION_ENTRY_POINTS(name, kind)                                                                   \
  CONTRACT_VIOLATION_OBSERVE_ENTRY_POINT(name##_observe_pf, kind, CXA_DM_PREDICATE_FALSE, false)                      \
  CONTRACT_VIOLATION_OBSERVE_ENTRY_POINT(name##_observe_ex, kind, CXA_DM_EVALUATION_EXCEPTION, true)                  \
  CONTRACT_VIOLATION_ENFORCE_ENTRY_POINT(name##_enforce_pf, kind, CXA_DM_PREDICATE_FALSE, false)                      \
  CONTRACT_VIOLATION_ENFORCE_ENTRY_POINT(name##_enforce_ex, kind, CXA_DM_EVALUATION_EXCEPTION, true)

CONTRACT_VIOLATION_ENTRY_POINTS(pre, CXA_AK_PRE)
CONTRACT_VIOLATION_ENTRY_POINTS(post, CXA_AK_POST)
CONTRACT_VIOLATION_ENTRY_POINTS(assert, CXA_AK_ASSERT)
CONTRACT_VIOLATION_ENTRY_POINTS(post_capture, CXA_AK_POST_CAPTURE)
CONTRACT_VIOLATION_ENTRY_POINTS(implicit, CXA_AK_IMPLICIT)

#undef CONTRACT_VIOLATION_ENTRY_POINTS
#undef CONTRACT_VIOLATION_ENFORCE_ENTRY_POINT
#undef CONTRACT_VIOLATION_OBSERVE_ENTRY_POINT
