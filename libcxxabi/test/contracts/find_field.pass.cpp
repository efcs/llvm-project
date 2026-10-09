//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23

// __cxa_find_field walks a chain of data blocks head-to-tail and returns the
// value of the first field found with one of the requested IDs: the first
// block that has the field wins, fields the reader does not know are skipped,
// extended fields are pointers rather than offsets, and blocks without a
// descriptor are skipped.

#include <cassert>
#include <cstdint>
#include <cstring>

#include "support.h"

using namespace __cxxabiv1;

int main(int, char**) {
  BasicBlock basic = make_basic_block("file.cpp", "f", 42, 7, "x > 0", "message");

  // Each field of a basic block is found at its offset.
  const __cxa_source_location* loc = __cxa_find_field_ptr<__cxa_source_location>(basic.get(), CXA_FIELD_SOURCE_LOCATION);
  assert(loc == &basic.location);
  assert(std::strcmp(loc->file_name, "file.cpp") == 0);
  assert(std::strcmp(loc->function_name, "f") == 0);
  assert(loc->line == 42 && loc->column == 7);
  assert(*__cxa_find_field_ptr<const char*>(basic.get(), CXA_FIELD_COMMENT) == basic.comment);
  assert(*__cxa_find_field_ptr<const char*>(basic.get(), CXA_FIELD_MESSAGE) == basic.message);

  // A field the chain does not have is not found, and the typed helpers give
  // the default.
  assert(__cxa_find_field_ptr<void*>(basic.get(), CXA_FIELD_EXCEPTION_PTR) == nullptr);
  assert(__cxa_find_field_value<std::uint8_t>(basic.get(), CXA_FIELD_ASSERTION_KIND, 99) == 99);
  assert(__cxa_find_field(nullptr, nullptr, 0) == nullptr);

  // Several IDs can be looked up at once; the first block having any of them
  // provides the value.
  {
    KindBlock kinds = make_kind_block(CXA_AK_PRE, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE, basic.get());
    std::uint8_t ids[] = {CXA_FIELD_COMMENT, CXA_FIELD_ASSERTION_KIND};
    assert(__cxa_find_field(kinds.get(), ids, 2) == &kinds.kind);
    std::uint8_t ids2[] = {CXA_FIELD_MESSAGE, CXA_FIELD_EXCEPTION_PTR};
    assert(__cxa_find_field(kinds.get(), ids2, 2) == &basic.message);
  }

  // First-found-wins: a block earlier in the chain overrides a later one.
  {
    BasicBlock front = make_basic_block("front.cpp", "g", 1, 1, "front", "", basic.get());
    const char* const* comment = __cxa_find_field_ptr<const char*>(front.get(), CXA_FIELD_COMMENT);
    assert(std::strcmp(*comment, "front") == 0);
    KindBlock kinds = make_kind_block(CXA_AK_POST, CXA_ES_ENFORCE, CXA_DM_EVALUATION_EXCEPTION, front.get());
    KindBlock override = make_kind_block(CXA_AK_ASSERT, CXA_ES_OBSERVE, CXA_DM_PREDICATE_FALSE, kinds.get());
    assert(__cxa_find_field_value<std::uint8_t>(override.get(), CXA_FIELD_ASSERTION_KIND, 0) == CXA_AK_ASSERT);
    assert(__cxa_find_field_value<std::uint8_t>(override.get(), CXA_FIELD_EVALUATION_SEMANTIC, 0) == CXA_ES_OBSERVE);
    assert(__cxa_find_field_value<std::uint8_t>(override.get(), CXA_FIELD_DETECTION_MODE, 0) == CXA_DM_PREDICATE_FALSE);
    // Fields only the tail has are still found through the whole chain.
    assert(std::strcmp(*__cxa_find_field_ptr<const char*>(override.get(), CXA_FIELD_MESSAGE), "") == 0);
    assert(__cxa_find_field_ptr<__cxa_source_location>(override.get(), CXA_FIELD_SOURCE_LOCATION) == &front.location);
  }

  // Unknown field IDs, in any order, are skipped; an extended field (>= 0x40)
  // is a pointer rather than an offset; and a reserved standard field the
  // reader does not know is ignored.
  {
    struct Block {
      const __cxa_descriptor_table_t* descriptor;
      const __cxa_contract_data_block* next;
      std::uint8_t future;
      const char* comment;
    };
    static const int extended_value = 123;
    static const DescriptorTable<4> descriptor = {
        (2u << 4) | 0xF, // a future table version from an unknown vendor
        4,
        {0x3F, 0x45, 0x80, CXA_FIELD_COMMENT},
        {},
        {{offsetof(Block, future)},
         {reinterpret_cast<std::uintptr_t>(&extended_value)},
         {0},
         {offsetof(Block, comment)}},
    };
    Block block = {descriptor.get(), basic.get(), 1, "from block"};
    const __cxa_contract_data_block* chain = reinterpret_cast<const __cxa_contract_data_block*>(&block);
    assert(std::strcmp(*__cxa_find_field_ptr<const char*>(chain, CXA_FIELD_COMMENT), "from block") == 0);
    assert(__cxa_find_field_ptr<int>(chain, 0x45) == &extended_value);
    assert(__cxa_find_field_ptr<std::uint8_t>(chain, 0x3F) == &block.future);
    assert(__cxa_find_field_ptr<void*>(chain, 0x0C) == nullptr);
    // The basic block behind it still provides what the block does not have.
    assert(__cxa_find_field_ptr<__cxa_source_location>(chain, CXA_FIELD_SOURCE_LOCATION) == &basic.location);
  }

  // A block without a descriptor is skipped.
  {
    __cxa_contract_data_block empty = {nullptr, basic.get()};
    assert(__cxa_find_field_ptr<__cxa_source_location>(&empty, CXA_FIELD_SOURCE_LOCATION) == &basic.location);
  }

  // The descriptor table's accessors.
  assert(basic_descriptor.get()->table_version() == 1);
  assert(basic_descriptor.get()->vendor_id() == CXA_VENDOR_CLANG);
  assert(basic_descriptor.get()->data() == basic_descriptor.data);

  return 0;
}
