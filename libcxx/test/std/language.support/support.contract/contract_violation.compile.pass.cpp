//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23

// <contracts>

// class contract_violation {
//   // no user-accessible constructor
// public:
//   contract_violation(const contract_violation&) = delete;
//   contract_violation& operator=(const contract_violation&) = delete;
//
//   see below ~contract_violation();
//
//   const char* comment() const noexcept;
//   contracts::detection_mode detection_mode() const noexcept;
//   exception_ptr evaluation_exception() const noexcept;
//   bool is_terminating() const noexcept;
//   assertion_kind kind() const noexcept;
//   source_location location() const noexcept;
//   evaluation_semantic semantic() const noexcept;
// };
//
// void invoke_default_contract_violation_handler(const contract_violation&);

// The header and its types are usable without -fcontracts.

#include <contracts>
#include <cstddef>
#include <exception>
#include <source_location>
#include <type_traits>

using std::contracts::contract_violation;

// No user-accessible constructor, not copyable or movable.
static_assert(std::is_class_v<contract_violation>);
static_assert(!std::is_default_constructible_v<contract_violation>);
static_assert(!std::is_copy_constructible_v<contract_violation>);
static_assert(!std::is_move_constructible_v<contract_violation>);
static_assert(!std::is_copy_assignable_v<contract_violation>);
static_assert(!std::is_move_assignable_v<contract_violation>);
static_assert(!std::is_constructible_v<contract_violation, const char*>);
static_assert(!std::is_constructible_v<contract_violation, void*>);
static_assert(!std::is_constructible_v<contract_violation, std::nullptr_t>);
static_assert(!std::is_aggregate_v<contract_violation>);

void test(const contract_violation& cv) {
  static_assert(std::is_same_v<decltype(cv.comment()), const char*>);
  static_assert(std::is_same_v<decltype(cv.detection_mode()), std::contracts::detection_mode>);
  static_assert(std::is_same_v<decltype(cv.evaluation_exception()), std::exception_ptr>);
  static_assert(std::is_same_v<decltype(cv.is_terminating()), bool>);
  static_assert(std::is_same_v<decltype(cv.kind()), std::contracts::assertion_kind>);
  static_assert(std::is_same_v<decltype(cv.location()), std::source_location>);
  static_assert(std::is_same_v<decltype(cv.semantic()), std::contracts::evaluation_semantic>);

  static_assert(noexcept(cv.comment()));
  static_assert(noexcept(cv.detection_mode()));
  static_assert(noexcept(cv.evaluation_exception()));
  static_assert(noexcept(cv.is_terminating()));
  static_assert(noexcept(cv.kind()));
  static_assert(noexcept(cv.location()));
  static_assert(noexcept(cv.semantic()));

  static_assert(std::is_same_v<decltype(std::contracts::invoke_default_contract_violation_handler(cv)), void>);
}
