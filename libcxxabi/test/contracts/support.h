//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// Support for testing the contract-violation runtime of the Itanium C++
// Contracts ABI: data block chains built by hand, exactly as the specification
// lays them out, and a std::contracts::contract_violation replacement that
// reads them the same way, so that the tests do not depend on libc++'s.

#ifndef LIBCXXABI_TEST_CONTRACTS_SUPPORT_H
#define LIBCXXABI_TEST_CONTRACTS_SUPPORT_H

#include <cstddef>
#include <cstdint>
#include <cstring>

#include <__contracts/abi.h>

// An independent statement of the ABI's constants, to check the header
// against the specification.
static_assert(__cxxabiv1::CXA_AK_UNSPECIFIED == 0x00, "");
static_assert(__cxxabiv1::CXA_AK_PRE == 0x01, "");
static_assert(__cxxabiv1::CXA_AK_POST == 0x02, "");
static_assert(__cxxabiv1::CXA_AK_ASSERT == 0x03, "");
static_assert(__cxxabiv1::CXA_AK_MANUAL == 0x04, "");
static_assert(__cxxabiv1::CXA_AK_CASSERT == 0x05, "");
static_assert(__cxxabiv1::CXA_AK_POST_CAPTURE == 0x06, "");
static_assert(__cxxabiv1::CXA_AK_IMPLICIT == 0x07, "");
static_assert(__cxxabiv1::CXA_ES_UNSPECIFIED == 0x00, "");
static_assert(__cxxabiv1::CXA_ES_IGNORE == 0x01, "");
static_assert(__cxxabiv1::CXA_ES_OBSERVE == 0x02, "");
static_assert(__cxxabiv1::CXA_ES_ENFORCE == 0x03, "");
static_assert(__cxxabiv1::CXA_ES_QUICK_ENFORCE == 0x04, "");
static_assert(__cxxabiv1::CXA_DM_UNSPECIFIED == 0x00, "");
static_assert(__cxxabiv1::CXA_DM_PREDICATE_FALSE == 0x01, "");
static_assert(__cxxabiv1::CXA_DM_EVALUATION_EXCEPTION == 0x02, "");
static_assert(__cxxabiv1::CXA_FIELD_SOURCE_LOCATION == 0x01, "");
static_assert(__cxxabiv1::CXA_FIELD_COMMENT == 0x02, "");
static_assert(__cxxabiv1::CXA_FIELD_MESSAGE == 0x03, "");
static_assert(__cxxabiv1::CXA_FIELD_LOCAL_HANDLER == 0x04, "");
static_assert(__cxxabiv1::CXA_FIELD_QUERY_FUNCTION == 0x05, "");
static_assert(__cxxabiv1::CXA_FIELD_LABEL_PTR == 0x06, "");
static_assert(__cxxabiv1::CXA_FIELD_ASSERTION_KIND == 0x07, "");
static_assert(__cxxabiv1::CXA_FIELD_EVALUATION_SEMANTIC == 0x08, "");
static_assert(__cxxabiv1::CXA_FIELD_DETECTION_MODE == 0x09, "");
static_assert(__cxxabiv1::CXA_FIELD_EXCEPTION_PTR == 0x0A, "");
static_assert(__cxxabiv1::CXA_FIELD_REPORT == 0x0B, "");
static_assert(__cxxabiv1::CXA_FIELD_EXTENDED == 0x40, "");
static_assert(__cxxabiv1::CXA_VENDOR_GENERIC == 0x0, "");
static_assert(__cxxabiv1::CXA_VENDOR_GCC == 0x1, "");
static_assert(__cxxabiv1::CXA_VENDOR_CLANG == 0x2, "");
static_assert(__cxxabiv1::CXA_VENDOR_MSVC == 0x3, "");

static_assert(sizeof(__cxxabiv1::__cxa_source_location) == 2 * sizeof(void*) + 2 * sizeof(unsigned), "");
static_assert(sizeof(__cxxabiv1::__cxa_descriptor_data_t) == sizeof(void*), "");
static_assert(sizeof(__cxxabiv1::__cxa_contract_data_block) == 2 * sizeof(void*), "");
static_assert(offsetof(__cxxabiv1::__cxa_contract_data_block, next) == sizeof(void*), "");
static_assert(sizeof(__cxxabiv1::__cxa_assertion_kind_t) == 1, "");
static_assert(sizeof(__cxxabiv1::__cxa_evaluation_semantic_t) == 1, "");
static_assert(sizeof(__cxxabiv1::__cxa_detection_mode_t) == 1, "");

// A descriptor table with N entries, laid out as the specification requires:
// header, num_entries, field_ids[N], padding to the alignment of the data
// entries, data[N].
template <std::size_t N>
struct DescriptorTable {
  std::uint8_t header;
  std::uint8_t num_entries;
  std::uint8_t field_ids[N];
  std::uint8_t padding[(2 + N + sizeof(void*) - 1) / sizeof(void*) * sizeof(void*) - (2 + N)];
  __cxxabiv1::__cxa_descriptor_data_t data[N];

  const __cxxabiv1::__cxa_descriptor_table_t* get() const {
    return reinterpret_cast<const __cxxabiv1::__cxa_descriptor_table_t*>(this);
  }
};
static_assert(offsetof(DescriptorTable<3>, data) == (2 + 3 + sizeof(void*) - 1) / sizeof(void*) * sizeof(void*), "");

constexpr std::uint8_t kGenericHeader = (1u << 4) | __cxxabiv1::CXA_VENDOR_GENERIC;
constexpr std::uint8_t kClangHeader   = (1u << 4) | __cxxabiv1::CXA_VENDOR_CLANG;

// The basic data block layout the compiler emits (specification section 5.5).
struct BasicBlock {
  const __cxxabiv1::__cxa_descriptor_table_t* descriptor;
  const __cxxabiv1::__cxa_contract_data_block* next;
  __cxxabiv1::__cxa_source_location location;
  const char* comment;
  const char* message;

  const __cxxabiv1::__cxa_contract_data_block* get() const {
    return reinterpret_cast<const __cxxabiv1::__cxa_contract_data_block*>(this);
  }
  void* data() const { return const_cast<BasicBlock*>(this); }
};

inline const DescriptorTable<3> basic_descriptor = {
    kClangHeader,
    3,
    {__cxxabiv1::CXA_FIELD_SOURCE_LOCATION, __cxxabiv1::CXA_FIELD_COMMENT, __cxxabiv1::CXA_FIELD_MESSAGE},
    {},
    {{offsetof(BasicBlock, location)}, {offsetof(BasicBlock, comment)}, {offsetof(BasicBlock, message)}},
};

inline BasicBlock make_basic_block(const char* file,
                                   const char* function,
                                   unsigned line,
                                   unsigned column,
                                   const char* comment,
                                   const char* message                               = "",
                                   const __cxxabiv1::__cxa_contract_data_block* next = nullptr) {
  return BasicBlock{basic_descriptor.get(), next, {file, function, line, column}, comment, message};
}

// A block with the kind, semantic and detection mode fields, as a stack block
// of the P3290 API or an override block would have.
struct KindBlock {
  const __cxxabiv1::__cxa_descriptor_table_t* descriptor;
  const __cxxabiv1::__cxa_contract_data_block* next;
  std::uint8_t kind;
  std::uint8_t semantic;
  std::uint8_t mode;

  const __cxxabiv1::__cxa_contract_data_block* get() const {
    return reinterpret_cast<const __cxxabiv1::__cxa_contract_data_block*>(this);
  }
  void* data() const { return const_cast<KindBlock*>(this); }
};

inline const DescriptorTable<3> kind_descriptor = {
    kGenericHeader,
    3,
    {__cxxabiv1::CXA_FIELD_ASSERTION_KIND,
     __cxxabiv1::CXA_FIELD_EVALUATION_SEMANTIC,
     __cxxabiv1::CXA_FIELD_DETECTION_MODE},
    {},
    {{offsetof(KindBlock, kind)}, {offsetof(KindBlock, semantic)}, {offsetof(KindBlock, mode)}},
};

inline KindBlock make_kind_block(std::uint8_t kind,
                                 std::uint8_t semantic,
                                 std::uint8_t mode,
                                 const __cxxabiv1::__cxa_contract_data_block* next = nullptr) {
  return KindBlock{kind_descriptor.get(), next, kind, semantic, mode};
}

// What the handler sees. The contract-violation runtime passes the address of
// an object holding the chain pointer; this reads the chain the same way the
// standard library's contract_violation does.
struct Violation {
  const __cxxabiv1::__cxa_contract_data_block* chain;

  template <class T>
  T value(std::uint8_t id, T def) const {
    return __cxxabiv1::__cxa_find_field_value<T>(chain, id, def);
  }
  std::uint8_t kind() const { return value<std::uint8_t>(__cxxabiv1::CXA_FIELD_ASSERTION_KIND, 0); }
  std::uint8_t semantic() const { return value<std::uint8_t>(__cxxabiv1::CXA_FIELD_EVALUATION_SEMANTIC, 0); }
  std::uint8_t mode() const { return value<std::uint8_t>(__cxxabiv1::CXA_FIELD_DETECTION_MODE, 0); }
  void* exception() const { return value<void*>(__cxxabiv1::CXA_FIELD_EXCEPTION_PTR, nullptr); }
  const char* comment() const {
    const char* const* p = __cxxabiv1::__cxa_find_field_ptr<const char*>(chain, __cxxabiv1::CXA_FIELD_COMMENT);
    return p ? *p : "";
  }
  const char* message() const {
    const char* const* p = __cxxabiv1::__cxa_find_field_ptr<const char*>(chain, __cxxabiv1::CXA_FIELD_MESSAGE);
    return p ? *p : "";
  }
  const __cxxabiv1::__cxa_source_location* location() const {
    return __cxxabiv1::__cxa_find_field_ptr<__cxxabiv1::__cxa_source_location>(
        chain, __cxxabiv1::CXA_FIELD_SOURCE_LOCATION);
  }
};

// The replacement handler is reached through the C-linkage symbol the compiler
// emits as an alias of ::handle_contract_violation. The tests define the alias
// directly, which does not depend on the compiler's support for contracts.
#define CONTRACTS_TEST_HANDLER() extern "C" void __handle_contract_violation([[maybe_unused]] const void* violation_ptr)

#endif // LIBCXXABI_TEST_CONTRACTS_SUPPORT_H
