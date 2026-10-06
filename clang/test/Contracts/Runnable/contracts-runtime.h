// A minimal contract-violation runtime for the runnable tests, so that they do
// not depend on the standard library's. It implements the entry points of the
// Itanium C++ Contracts ABI that Clang calls when a contract assertion is
// violated, reading the data block chain the compiler emits for the assertion.

#ifndef CONTRACTS_CONTRACTS_RUNTIME_H
#define CONTRACTS_CONTRACTS_RUNTIME_H

#include "contracts.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

namespace __cxxabi {

// The wire format: see <__contracts/abi.h> in libc++.
enum : uint8_t {
  CXA_AK_PRE = 0x01,
  CXA_AK_POST = 0x02,
  CXA_AK_ASSERT = 0x03,

  CXA_ES_OBSERVE = 0x02,
  CXA_ES_ENFORCE = 0x03,

  CXA_DM_PREDICATE_FALSE = 0x01,
  CXA_DM_EVALUATION_EXCEPTION = 0x02,

  CXA_FIELD_SOURCE_LOCATION = 0x01,
  CXA_FIELD_COMMENT = 0x02,
  CXA_FIELD_MESSAGE = 0x03,
  CXA_FIELD_EXTENDED = 0x40,
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

  const __cxa_descriptor_data_t* data() const {
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
inline const void* find_field(const __cxa_contract_data_block* chain, uint8_t id) {
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

inline const char* find_string(const __cxa_contract_data_block* chain, uint8_t id) {
  const char* const* p = static_cast<const char* const*>(find_field(chain, id));
  return p && *p ? *p : "";
}

inline std::contracts::_ContractViolationImpl
create_impl(void* data, uint8_t kind, uint8_t semantic, uint8_t mode) {
  using namespace std::contracts;
  const __cxa_contract_data_block* chain = static_cast<const __cxa_contract_data_block*>(data);
  _ContractViolationImpl impl;
  impl.kind = static_cast<assertion_kind>(kind);
  impl.semantic = static_cast<evaluation_semantic>(semantic);
  impl.mode = static_cast<_DetectionMode>(mode);
  impl.comment = find_string(chain, CXA_FIELD_COMMENT);
  impl.message = find_string(chain, CXA_FIELD_MESSAGE);
  impl.location = std::source_location::__create_from_pointer(find_field(chain, CXA_FIELD_SOURCE_LOCATION));
  return impl;
}

inline void dispatch(void* data, uint8_t kind, uint8_t semantic, uint8_t mode) {
  std::contracts::_ContractViolationImpl impl = create_impl(data, kind, semantic, mode);
  std::contracts::__handle_contract_violation_internal(impl.__create_violation());
}

} // namespace __cxxabi

// The entry points: __cxa_contract_violation_{kind}_{semantic}_{mode}[_noexcept].
// The 'enforce' variants never return (the handler terminates the program),
// and the '_noexcept' variants terminate if the handler throws.
#define CONTRACTS_RUNTIME_ENTRY_POINT(name, kind, semantic, mode)                                  \
  extern "C" inline void __cxa_contract_violation_##name(void* data) {                               \
    __cxxabi::dispatch(data, kind, semantic, mode);                                                \
  }                                                                                                \
  extern "C" inline void __cxa_contract_violation_##name##_noexcept(void* data) noexcept {           \
    __cxxabi::dispatch(data, kind, semantic, mode);                                                \
  }

#define CONTRACTS_RUNTIME_ENTRY_POINTS(name, kind)                                                 \
  CONTRACTS_RUNTIME_ENTRY_POINT(name##_observe_pf, kind, __cxxabi::CXA_ES_OBSERVE,                 \
                                __cxxabi::CXA_DM_PREDICATE_FALSE)                                  \
  CONTRACTS_RUNTIME_ENTRY_POINT(name##_observe_ex, kind, __cxxabi::CXA_ES_OBSERVE,                 \
                                __cxxabi::CXA_DM_EVALUATION_EXCEPTION)                             \
  CONTRACTS_RUNTIME_ENTRY_POINT(name##_enforce_pf, kind, __cxxabi::CXA_ES_ENFORCE,                 \
                                __cxxabi::CXA_DM_PREDICATE_FALSE)                                  \
  CONTRACTS_RUNTIME_ENTRY_POINT(name##_enforce_ex, kind, __cxxabi::CXA_ES_ENFORCE,                 \
                                __cxxabi::CXA_DM_EVALUATION_EXCEPTION)

CONTRACTS_RUNTIME_ENTRY_POINTS(pre, __cxxabi::CXA_AK_PRE)
CONTRACTS_RUNTIME_ENTRY_POINTS(post, __cxxabi::CXA_AK_POST)
CONTRACTS_RUNTIME_ENTRY_POINTS(assert, __cxxabi::CXA_AK_ASSERT)

#undef CONTRACTS_RUNTIME_ENTRY_POINTS
#undef CONTRACTS_RUNTIME_ENTRY_POINT

#endif // CONTRACTS_CONTRACTS_RUNTIME_H
