// A minimal contract-violation runtime for the runnable tests, so that they do
// not depend on the standard library's. It implements the entry points of the
// Itanium C++ Contracts ABI that Clang calls when a contract assertion is
// violated, reading the data block chain the compiler emits for the assertion,
// and defines the accessors of the contract_violation of contracts.h.

#ifndef CONTRACTS_CONTRACTS_RUNTIME_H
#define CONTRACTS_CONTRACTS_RUNTIME_H

#include "contracts.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

namespace __cxxabiv1 {

// The wire format: see <__contracts/abi.h> in libc++.
enum : uint8_t {
  CXA_AK_UNSPECIFIED = 0x00,
  CXA_AK_PRE = 0x01,
  CXA_AK_POST = 0x02,
  CXA_AK_ASSERT = 0x03,

  CXA_ES_UNSPECIFIED = 0x00,
  CXA_ES_OBSERVE = 0x02,
  CXA_ES_ENFORCE = 0x03,
  CXA_ES_QUICK_ENFORCE = 0x04,

  CXA_DM_UNSPECIFIED = 0x00,
  CXA_DM_PREDICATE_FALSE = 0x01,
  CXA_DM_EVALUATION_EXCEPTION = 0x02,

  CXA_FIELD_SOURCE_LOCATION = 0x01,
  CXA_FIELD_COMMENT = 0x02,
  CXA_FIELD_MESSAGE = 0x03,
  CXA_FIELD_ASSERTION_KIND = 0x07,
  CXA_FIELD_EVALUATION_SEMANTIC = 0x08,
  CXA_FIELD_DETECTION_MODE = 0x09,
  CXA_FIELD_EXTENDED = 0x40,

  CXA_VENDOR_GENERIC = 0x0,
};

struct __cxa_source_location {
  const char* file_name;
  const char* function_name;
  unsigned line;
  unsigned column;
};

union __cxa_descriptor_data_t {
  uintptr_t offset;
  const void* pointer;
};

struct __cxa_descriptor_table_t {
  uint8_t header;
  uint8_t num_entries;
  uint8_t field_ids[];

  const __cxa_descriptor_data_t* data() const noexcept {
    uintptr_t p = reinterpret_cast<uintptr_t>(field_ids + num_entries);
    p = (p + alignof(__cxa_descriptor_data_t) - 1) & ~uintptr_t(alignof(__cxa_descriptor_data_t) - 1);
    return reinterpret_cast<const __cxa_descriptor_data_t*>(p);
  }
};

struct __cxa_contract_data_block {
  const __cxa_descriptor_table_t* descriptor;
  const __cxa_contract_data_block* next;
};

// First-found-wins lookup of a field in the chain.
inline const void* find_field(const __cxa_contract_data_block* chain, uint8_t id) noexcept {
  for (const __cxa_contract_data_block* block = chain; block; block = block->next) {
    const __cxa_descriptor_table_t* desc = block->descriptor;
    if (!desc)
      continue;
    const __cxa_descriptor_data_t* data = desc->data();
    for (uint8_t i = 0; i < desc->num_entries; ++i) {
      if (desc->field_ids[i] != id)
        continue;
      if (id >= CXA_FIELD_EXTENDED)
        return data[i].pointer;
      return reinterpret_cast<const char*>(block) + data[i].offset;
    }
  }
  return nullptr;
}

template <class T>
inline T find_value(const __cxa_contract_data_block* chain, uint8_t id, T default_value) noexcept {
  const void* p = find_field(chain, id);
  return p ? *static_cast<const T*>(p) : default_value;
}

inline const char* find_string(const __cxa_contract_data_block* chain, uint8_t id) noexcept {
  return find_value<const char*>(chain, id, nullptr);
}

// The override block a specialized entry point prepends to the chain, with
// the assertion kind, evaluation semantic and detection mode its name encodes.
struct override_descriptor_t {
  uint8_t header;
  uint8_t num_entries;
  uint8_t field_ids[3];
  uint8_t padding[alignof(__cxa_descriptor_data_t) - (5 % alignof(__cxa_descriptor_data_t))];
  __cxa_descriptor_data_t data[3];
};

struct override_block_t {
  const __cxa_descriptor_table_t* descriptor;
  const __cxa_contract_data_block* next;
  uint8_t kind;
  uint8_t semantic;
  uint8_t mode;
};

inline const override_descriptor_t override_descriptor = {
    (1u << 4) | CXA_VENDOR_GENERIC,
    3,
    {CXA_FIELD_ASSERTION_KIND, CXA_FIELD_EVALUATION_SEMANTIC, CXA_FIELD_DETECTION_MODE},
    {},
    {{offsetof(override_block_t, kind)}, {offsetof(override_block_t, semantic)}, {offsetof(override_block_t, mode)}},
};

} // namespace __cxxabiv1

namespace std::contracts {

// The runtime creates the contract_violation for the handler.
struct __contract_violation_access {
  static contract_violation create(const __cxxabiv1::__cxa_contract_data_block* chain) noexcept {
    return contract_violation(chain);
  }
};

inline std::source_location contract_violation::location() const noexcept {
  if (const void* location = __cxxabiv1::find_field(__chain_, __cxxabiv1::CXA_FIELD_SOURCE_LOCATION))
    return std::source_location::__create_from_pointer(location);
  return std::source_location();
}
inline const char* contract_violation::comment() const noexcept {
  const char* comment = __cxxabiv1::find_string(__chain_, __cxxabiv1::CXA_FIELD_COMMENT);
  return comment ? comment : "";
}
inline const char* contract_violation::message() const noexcept {
  return __cxxabiv1::find_string(__chain_, __cxxabiv1::CXA_FIELD_MESSAGE);
}
inline contracts::detection_mode contract_violation::detection_mode() const noexcept {
  return static_cast<contracts::detection_mode>(
      __cxxabiv1::find_value<uint8_t>(__chain_, __cxxabiv1::CXA_FIELD_DETECTION_MODE, __cxxabiv1::CXA_DM_UNSPECIFIED));
}
inline assertion_kind contract_violation::kind() const noexcept {
  return static_cast<assertion_kind>(
      __cxxabiv1::find_value<uint8_t>(__chain_, __cxxabiv1::CXA_FIELD_ASSERTION_KIND, __cxxabiv1::CXA_AK_UNSPECIFIED));
}
inline evaluation_semantic contract_violation::semantic() const noexcept {
  return static_cast<evaluation_semantic>(__cxxabiv1::find_value<uint8_t>(
      __chain_, __cxxabiv1::CXA_FIELD_EVALUATION_SEMANTIC, __cxxabiv1::CXA_ES_UNSPECIFIED));
}
inline bool contract_violation::is_terminating() const noexcept {
  return semantic() == evaluation_semantic::enforce || semantic() == evaluation_semantic::quick_enforce;
}

} // namespace std::contracts

namespace __cxxabiv1 {

// Dispatch a violation to the program's handler if it has one, else to the
// default handler, and terminate when the semantic is a terminating one.
inline void dispatch(void* data, uint8_t kind, uint8_t semantic, uint8_t mode) {
  override_block_t block = {
      reinterpret_cast<const __cxa_descriptor_table_t*>(&override_descriptor),
      static_cast<const __cxa_contract_data_block*>(data),
      kind,
      semantic,
      mode,
  };
  std::contracts::contract_violation violation = std::contracts::__contract_violation_access::create(
      reinterpret_cast<const __cxa_contract_data_block*>(&block));
  if (::handle_contract_violation)
    ::handle_contract_violation(violation);
  else
    std::contracts::invoke_default_contract_violation_handler(violation);

  if (semantic == CXA_ES_ENFORCE)
    __builtin_abort();
}

} // namespace __cxxabiv1

// The entry points: __cxa_contract_violation_{kind}_{semantic}_{mode}[_noexcept].
// The 'enforce' variants never return (the handler terminates the program),
// and the '_noexcept' variants terminate if the handler throws.
#define CONTRACTS_RUNTIME_ENTRY_POINT(name, kind, semantic, mode)                                  \
  extern "C" inline void __cxa_contract_violation_##name(void* data) {                             \
    __cxxabiv1::dispatch(data, kind, semantic, mode);                                              \
  }                                                                                                \
  extern "C" inline void __cxa_contract_violation_##name##_noexcept(void* data) noexcept {         \
    __cxxabiv1::dispatch(data, kind, semantic, mode);                                              \
  }

#define CONTRACTS_RUNTIME_ENTRY_POINTS(name, kind)                                                 \
  CONTRACTS_RUNTIME_ENTRY_POINT(name##_observe_pf, kind, __cxxabiv1::CXA_ES_OBSERVE,               \
                                __cxxabiv1::CXA_DM_PREDICATE_FALSE)                                \
  CONTRACTS_RUNTIME_ENTRY_POINT(name##_observe_ex, kind, __cxxabiv1::CXA_ES_OBSERVE,               \
                                __cxxabiv1::CXA_DM_EVALUATION_EXCEPTION)                           \
  CONTRACTS_RUNTIME_ENTRY_POINT(name##_enforce_pf, kind, __cxxabiv1::CXA_ES_ENFORCE,               \
                                __cxxabiv1::CXA_DM_PREDICATE_FALSE)                                \
  CONTRACTS_RUNTIME_ENTRY_POINT(name##_enforce_ex, kind, __cxxabiv1::CXA_ES_ENFORCE,               \
                                __cxxabiv1::CXA_DM_EVALUATION_EXCEPTION)

CONTRACTS_RUNTIME_ENTRY_POINTS(pre, __cxxabiv1::CXA_AK_PRE)
CONTRACTS_RUNTIME_ENTRY_POINTS(post, __cxxabiv1::CXA_AK_POST)
CONTRACTS_RUNTIME_ENTRY_POINTS(assert, __cxxabiv1::CXA_AK_ASSERT)

#undef CONTRACTS_RUNTIME_ENTRY_POINTS
#undef CONTRACTS_RUNTIME_ENTRY_POINT

#endif // CONTRACTS_CONTRACTS_RUNTIME_H
