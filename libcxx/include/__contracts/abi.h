// -*- C++ -*-
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef _LIBCPP___CONTRACTS_ABI_H
#define _LIBCPP___CONTRACTS_ABI_H

// The wire format of the Itanium C++ Contracts ABI.
//
// This header defines the binary interface between the code the compiler emits
// for a contract assertion and the contract-violation runtime: the data block
// chain that describes a violation, the descriptor tables that make the blocks
// self-describing, the enumerations encoded in them, and the entry points the
// compiler calls when a violation is detected. The entry points are provided
// by libc++abi; libc++'s std::contracts::contract_violation is a wrapper
// around the chain pointer that reads the data through __cxa_find_field.
//
// Every type, enumerator value and symbol name here is ABI: it must stay
// byte-compatible with the specification
// (https://github.com/notadragon/efcs_contracts-abi-specification) and with
// the other implementations of it (GCC's bits/contracts_abi.h).

#include <__config>
#include <__cstddef/size_t.h>

#if !defined(_LIBCPP_HAS_NO_PRAGMA_SYSTEM_HEADER)
#  pragma GCC system_header
#endif

#if _LIBCPP_STD_VER >= 26 || defined(_LIBCPP_BUILDING_LIBRARY)

namespace __cxxabiv1 {

// The source location of an assertion. This has the layout of the data
// std::source_location refers to (std::source_location::__impl in libc++ and
// libstdc++), so that a source_location can refer to it directly.
struct __cxa_source_location {
  const char* file_name;
  const char* function_name;
  unsigned line;
  unsigned column;
};

// The ABI encodings of the std::contracts enumerations. Each has fixed
// enumerator values and a fixed underlying type, independent of the standard
// library's types, and 0 means "unspecified": the value of a field that is
// absent from the chain.

enum __cxa_assertion_kind_t : __UINT8_TYPE__ {
  CXA_AK_UNSPECIFIED  = 0x00,
  CXA_AK_PRE          = 0x01,
  CXA_AK_POST         = 0x02,
  CXA_AK_ASSERT       = 0x03,
  CXA_AK_MANUAL       = 0x04, // P3290 manual violation API
  CXA_AK_CASSERT      = 0x05, // C assert()
  CXA_AK_POST_CAPTURE = 0x06, // P3098 postcondition capture
  CXA_AK_IMPLICIT     = 0x07, // P3100 implicit contract assertion
};

enum __cxa_evaluation_semantic_t : __UINT8_TYPE__ {
  CXA_ES_UNSPECIFIED   = 0x00,
  CXA_ES_IGNORE        = 0x01,
  CXA_ES_OBSERVE       = 0x02,
  CXA_ES_ENFORCE       = 0x03,
  CXA_ES_QUICK_ENFORCE = 0x04,
};

enum __cxa_detection_mode_t : __UINT8_TYPE__ {
  CXA_DM_UNSPECIFIED          = 0x00,
  CXA_DM_PREDICATE_FALSE      = 0x01,
  CXA_DM_EVALUATION_EXCEPTION = 0x02,
};

// Field IDs. Each ID identifies a piece of information together with its
// encoding; a logical field may get a new ID for a new encoding, in which
// case the accessors look for both and take the first found in the chain.
enum __cxa_field_id_t : __UINT8_TYPE__ {
  // Standard fields (< 0x40): the descriptor entry is the byte offset of the
  // field's value from the start of the data block.
  CXA_FIELD_SOURCE_LOCATION     = 0x01, // __cxa_source_location
  CXA_FIELD_COMMENT             = 0x02, // const char*: the predicate text
  CXA_FIELD_MESSAGE             = 0x03, // const char*: P3099 message
  CXA_FIELD_LOCAL_HANDLER       = 0x04, // __cxa_local_handler_fn_t (P3400)
  CXA_FIELD_QUERY_FUNCTION      = 0x05, // __cxa_query_fn_t (P3400)
  CXA_FIELD_LABEL_PTR           = 0x06, // const void* (P3400)
  CXA_FIELD_ASSERTION_KIND      = 0x07, // __UINT8_TYPE__ (__cxa_assertion_kind_t)
  CXA_FIELD_EVALUATION_SEMANTIC = 0x08, // __UINT8_TYPE__ (__cxa_evaluation_semantic_t)
  CXA_FIELD_DETECTION_MODE      = 0x09, // __UINT8_TYPE__ (__cxa_detection_mode_t)
  CXA_FIELD_EXCEPTION_PTR       = 0x0A, // void*: the exception the predicate exited with
  CXA_FIELD_REPORT              = 0x0B, // __cxa_contract_report_populator
  // 0x0C - 0x3F: reserved for future standard fields.

  // Extended fields (>= 0x40): the descriptor entry is a pointer to the field's
  // value. 0x40 - 0x7F are vendor-specific, 0x80 - 0xFF reserved.
  CXA_FIELD_EXTENDED = 0x40,
};

// Vendor IDs, packed into the low 4 bits of the descriptor table header.
enum __cxa_vendor_id_t : __UINT8_TYPE__ {
  CXA_VENDOR_GENERIC = 0x0,
  CXA_VENDOR_GCC     = 0x1,
  CXA_VENDOR_CLANG   = 0x2,
  CXA_VENDOR_MSVC    = 0x3,
};

// The entry of a descriptor table for a field: the byte offset of the value in
// the data block (standard fields), or a pointer to it (extended fields).
union __cxa_descriptor_data_t {
  __UINTPTR_TYPE__ offset;
  const void* pointer;
};

// A descriptor table describes which fields a data block contains and where.
//
//   byte 0:       [table_version : 4 bits][vendor_id : 4 bits]
//   byte 1:       num_entries
//   bytes 2..:    field_ids[num_entries]
//   <padding to alignof(__cxa_descriptor_data_t)>
//   data[num_entries]
//
// Fields may appear in any order, a field ID appears at most once, and a
// reader ignores the field IDs it does not know.
struct __cxa_descriptor_table_t {
  __UINT8_TYPE__ header;
  __UINT8_TYPE__ num_entries;
  __UINT8_TYPE__ field_ids[];

  _LIBCPP_HIDE_FROM_ABI __UINT8_TYPE__ table_version() const { return header >> 4; }
  _LIBCPP_HIDE_FROM_ABI __UINT8_TYPE__ vendor_id() const { return header & 0x0F; }

  _LIBCPP_HIDE_FROM_ABI const __cxa_descriptor_data_t* data() const {
    __UINTPTR_TYPE__ __p = reinterpret_cast<__UINTPTR_TYPE__>(field_ids + num_entries);
    __p                  = (__p + alignof(__cxa_descriptor_data_t) - 1) & ~(alignof(__cxa_descriptor_data_t) - 1);
    return reinterpret_cast<const __cxa_descriptor_data_t*>(__p);
  }
};

// A data block: one node of the chain of violation data. The field data
// follows the two pointers, at the byte offsets given by the descriptor.
struct __cxa_contract_data_block {
  const __cxa_descriptor_table_t* descriptor;
  const __cxa_contract_data_block* next;
};

// P3400 local handler and query functions, stored in the data block as
// type-erased function pointers.
using __cxa_local_handler_fn_t = int (*)(const void* /*label_ptr*/, const void* /*contract_violation_ptr*/);
using __cxa_query_fn_t         = void* (*)(const void* /*label_ptr*/, const void* /*key*/, std::size_t /*index*/);

// A lazily produced report (CXA_FIELD_REPORT): populate(ctx) returns a string
// owned by the producer that is valid for the lifetime of the violation.
struct __cxa_contract_report_populator {
  const char* (*populate)(const void* __ctx);
  const void* ctx;
};

extern "C" {

// Walk the chain head-to-tail and return a pointer to the value of the first
// field found whose ID is one of the __num_ids IDs in __ids, or nullptr.
_LIBCPP_OVERRIDABLE_FUNC_VIS const void*
__cxa_find_field(const __cxa_contract_data_block* __chain, const __UINT8_TYPE__* __ids, __UINT8_TYPE__ __num_ids);

// The default contract-violation handler, callable regardless of whether the
// program replaced ::handle_contract_violation. The argument is the address
// of a std::contracts::contract_violation.
_LIBCPP_OVERRIDABLE_FUNC_VIS void __contract_invoke_default_handler(const void* __violation);

} // extern "C"

// The value of the scalar field __field_id, or __default if the chain has none.
template <class _Tp>
_LIBCPP_HIDE_FROM_ABI _Tp
__cxa_find_field_value(const __cxa_contract_data_block* __chain, __UINT8_TYPE__ __field_id, _Tp __default) {
  __UINT8_TYPE__ __ids[] = {__field_id};
  if (const void* __p = __cxa_find_field(__chain, __ids, 1))
    return *static_cast<const _Tp*>(__p);
  return __default;
}

// A pointer to the value of the field __field_id, or nullptr if the chain has
// none.
template <class _Tp>
_LIBCPP_HIDE_FROM_ABI const _Tp*
__cxa_find_field_ptr(const __cxa_contract_data_block* __chain, __UINT8_TYPE__ __field_id) {
  __UINT8_TYPE__ __ids[] = {__field_id};
  return static_cast<const _Tp*>(__cxa_find_field(__chain, __ids, 1));
}

} // namespace __cxxabiv1

// The entry points the compiler calls when a contract violation is detected.
// Each takes the address of the head of a data block chain and dispatches the
// violation to the contract-violation handler.
//
// The universal entry points take the assertion kind, evaluation semantic and
// detection mode from the chain. The specialized entry points
//
//   __cxa_contract_violation_{kind}_{semantic}_{mode}[_noexcept]
//
// encode them in their name and prepend an override block to the chain. The
// 'enforce' variants terminate the program (std::abort()) when the handler
// returns; the '_noexcept' variants do not unwind, and terminate the program
// (std::terminate()) if the handler exits via an exception.
extern "C" {

_LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation(void* __data);
_LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_noexcept(void* __data) noexcept;

#  define _LIBCPP_CONTRACT_VIOLATION_ENTRY_POINTS(_Kind)                                                                 \
    _LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_##_Kind##_observe_pf(void* __data);                          \
    _LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_##_Kind##_observe_pf_noexcept(void* __data) noexcept;        \
    _LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_##_Kind##_observe_ex(void* __data);                          \
    _LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_##_Kind##_observe_ex_noexcept(void* __data) noexcept;        \
    [[noreturn]] _LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_##_Kind##_enforce_pf(void* __data);             \
    [[noreturn]] _LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_##_Kind##_enforce_pf_noexcept(                  \
        void* __data) noexcept;                                                                                        \
    [[noreturn]] _LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_##_Kind##_enforce_ex(void* __data);             \
    [[noreturn]] _LIBCPP_OVERRIDABLE_FUNC_VIS void __cxa_contract_violation_##_Kind##_enforce_ex_noexcept(                  \
        void* __data) noexcept;

_LIBCPP_CONTRACT_VIOLATION_ENTRY_POINTS(pre)
_LIBCPP_CONTRACT_VIOLATION_ENTRY_POINTS(post)
_LIBCPP_CONTRACT_VIOLATION_ENTRY_POINTS(assert)
_LIBCPP_CONTRACT_VIOLATION_ENTRY_POINTS(post_capture)
_LIBCPP_CONTRACT_VIOLATION_ENTRY_POINTS(implicit)

#  undef _LIBCPP_CONTRACT_VIOLATION_ENTRY_POINTS

} // extern "C"

#endif // _LIBCPP_STD_VER >= 26 || defined(_LIBCPP_BUILDING_LIBRARY)

#endif // _LIBCPP___CONTRACTS_ABI_H
