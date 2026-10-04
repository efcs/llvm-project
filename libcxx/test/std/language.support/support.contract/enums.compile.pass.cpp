//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23

// <contracts>

// enum class assertion_kind : unspecified {
//   pre = 1, post = 2, assert = 3
// };
// enum class evaluation_semantic : unspecified {
//   ignore = 1, observe = 2, enforce = 3, quick_enforce = 4
// };
// enum class detection_mode : unspecified {
//   predicate_false = 1, evaluation_exception = 2
// };

// The header and its types are usable without -fcontracts.

#include <cassert>
#include <contracts>
#include <type_traits>

template <class E>
constexpr bool is_scoped_enum_v = std::is_enum_v<E> && !std::is_convertible_v<E, std::underlying_type_t<E>>;

static_assert(is_scoped_enum_v<std::contracts::assertion_kind>);
static_assert(is_scoped_enum_v<std::contracts::evaluation_semantic>);
static_assert(is_scoped_enum_v<std::contracts::detection_mode>);

static_assert(static_cast<int>(std::contracts::assertion_kind::pre) == 1);
static_assert(static_cast<int>(std::contracts::assertion_kind::post) == 2);
static_assert(static_cast<int>(std::contracts::assertion_kind::assert) == 3);

static_assert(static_cast<int>(std::contracts::evaluation_semantic::ignore) == 1);
static_assert(static_cast<int>(std::contracts::evaluation_semantic::observe) == 2);
static_assert(static_cast<int>(std::contracts::evaluation_semantic::enforce) == 3);
static_assert(static_cast<int>(std::contracts::evaluation_semantic::quick_enforce) == 4);

static_assert(static_cast<int>(std::contracts::detection_mode::predicate_false) == 1);
static_assert(static_cast<int>(std::contracts::detection_mode::evaluation_exception) == 2);

// `assert` is an enumerator name even when <cassert> is included.
static_assert(std::contracts::assertion_kind::assert != std::contracts::assertion_kind::pre);
