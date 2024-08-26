// -*- C++ -*-
//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//                        Kokkos v. 4.0
//       Copyright (2022) National Technology & Engineering
//               Solutions of Sandia, LLC (NTESS).
//
// Under the terms of Contract DE-NA0003525 with NTESS,
// the U.S. Government retains certain rights in this software.
//
//===---------------------------------------------------------------------===//

#ifndef _LIBCPP___MDSPAN_LAYOUT_STRIDE_H
#  define _LIBCPP___MDSPAN_LAYOUT_STRIDE_H

#  include <__assert>
#  include <__config>
#  include <__fwd/mdspan.h>
#  include <__mdspan/extents.h>
#  include <__type_traits/is_constructible.h>
#  include <__type_traits/is_convertible.h>
#  include <__type_traits/is_nothrow_constructible.h>
#  include <__utility/as_const.h>
#  include <__utility/integer_sequence.h>
#  include <__utility/swap.h>
#  include <array>
#  include <cinttypes>
#  include <cstddef>
#  include <limits>

#  if !defined(_LIBCPP_HAS_NO_PRAGMA_SYSTEM_HEADER)
#    pragma GCC system_header
#  endif

_LIBCPP_PUSH_MACROS
#  include <__undef_macros>

_LIBCPP_BEGIN_NAMESPACE_STD

#  if _LIBCPP_STD_VER >= 23

namespace __mdspan_detail {
template <class _Layout, class _Mapping>
constexpr bool __is_mapping_of =
    is_same_v<typename _Layout::template mapping<typename _Mapping::extents_type>, _Mapping>;

template <class _Mapping>
concept __layout_mapping_alike = requires {
  requires __is_mapping_of<typename _Mapping::layout_type, _Mapping>;
  requires __is_extents_v<typename _Mapping::extents_type>;
  { _Mapping::is_always_strided() } -> same_as<bool>;
  { _Mapping::is_always_exhaustive() } -> same_as<bool>;
  { _Mapping::is_always_unique() } -> same_as<bool>;
  bool_constant<_Mapping::is_always_strided()>::value;
  bool_constant<_Mapping::is_always_exhaustive()>::value;
  bool_constant<_Mapping::is_always_unique()>::value;
};
} // namespace __mdspan_detail

template <class _Extents>
class layout_stride::mapping {
public:
  using extents_type = _Extents;
  using index_type   = typename extents_type::index_type;
  using size_type    = typename extents_type::size_type;
  using rank_type    = typename extents_type::rank_type;
  using layout_type  = layout_stride;

private:
  static constexpr rank_type __rank_ = extents_type::rank();

  // compute offset of a strided layout mapping
  template <class _StridedMapping>
  _LIBCPP_HIDE_FROM_ABI static constexpr index_type __offset(const _StridedMapping& __mapping);

public:
  _LIBCPP_HIDE_FROM_ABI constexpr mapping(const mapping&) noexcept = default;

  template <class _OtherIndexType>
  _LIBCPP_HIDE_FROM_ABI constexpr mapping(const extents_type&, span<_OtherIndexType, __rank_> __strides) noexcept {
    [&] {
      contract_assert(([&]<size_t... _Pos>(index_sequence<_Pos...>) {
        // For integrals we can do a pre-conversion check, for other types not
        return __strides[0] > 0;
      }(make_index_sequence<__rank_>())));
    }();
  }

  template <class _OtherIndexType>
    requires(is_convertible_v<const _OtherIndexType&, index_type> &&
             is_nothrow_constructible_v<index_type, const _OtherIndexType&>)
  _LIBCPP_HIDE_FROM_ABI constexpr mapping(const extents_type& __ext,
                                          const array<_OtherIndexType, __rank_>& __strides) noexcept
      : mapping(__ext, span(__strides)) {}

  template <class _StridedLayoutMapping>
    requires(__mdspan_detail::__layout_mapping_alike<_StridedLayoutMapping> &&
             is_constructible_v<extents_type, typename _StridedLayoutMapping::extents_type> &&
             _StridedLayoutMapping::is_always_unique() && _StridedLayoutMapping::is_always_strided())
  _LIBCPP_HIDE_FROM_ABI constexpr mapping(const _StridedLayoutMapping& __other) noexcept
      : __extents_(__other.extents()), __strides_([&]<size_t... _Pos>(index_sequence<_Pos...>) {
          // stride() only compiles for rank > 0
          if constexpr (__rank_ > 0) {
            return __mdspan_detail::__possibly_empty_array<index_type, __rank_>{
                static_cast<index_type>(__other.stride(_Pos))...};
          } else {
            return __mdspan_detail::__possibly_empty_array<index_type, 0>{};
          }
        }(make_index_sequence<__rank_>())) {
    // stride() only compiles for rank > 0
  }

  _LIBCPP_HIDE_FROM_ABI constexpr mapping& operator=(const mapping&) noexcept = default;

  // [mdspan.layout.stride.obs], observers
  _LIBCPP_HIDE_FROM_ABI constexpr const extents_type& extents() const noexcept { return __extents_; }

private:
  _LIBCPP_NO_UNIQUE_ADDRESS extents_type __extents_{};
  _LIBCPP_NO_UNIQUE_ADDRESS __mdspan_detail::__possibly_empty_array<index_type, __rank_> __strides_{};
};

#  endif // _LIBCPP_STD_VER >= 23

_LIBCPP_END_NAMESPACE_STD

_LIBCPP_POP_MACROS

#endif // _LIBCPP___MDSPAN_LAYOUT_STRIDE_H

//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// REQUIRES: has-unix-headers
// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20
// UNSUPPORTED: libcpp-hardening-mode=none
// XFAIL: libcpp-hardening-mode=debug && availability-verbose_abort-missing
// XFAIL: libcpp-hardening-mode=debug && target=powerpc{{.*}}le-unknown-linux-gnu

// <mdspan>

// template<class StridedLayoutMapping>
//   constexpr explicit(see below)
//     mapping(const StridedLayoutMapping& other) noexcept;
//
// Constraints:
//   - layout-mapping-alike<StridedLayoutMapping> is satisfied.
//   - is_constructible_v<extents_type, typename StridedLayoutMapping::extents_type> is true.
//   - StridedLayoutMapping::is_always_unique() is true.
//   - StridedLayoutMapping::is_always_strided() is true.
//
// Preconditions:
//   - StridedLayoutMapping meets the layout mapping requirements ([mdspan.layout.policy.reqmts]),
//   - other.stride(r) > 0 is true for every rank index r of extents(),
//   - other.required_span_size() is representable as a value of type index_type ([basic.fundamental]), and
//   - OFFSET(other) == 0 is true.
//
// Effects: Direct-non-list-initializes extents_ with other.extents(), and for all d in the range [0, rank_),
//          direct-non-list-initializes strides_[d] with other.stride(d).
//
// Remarks: The expression inside explicit is equivalent to:
//   - !(is_convertible_v<typename StridedLayoutMapping::extents_type, extents_type> &&
//       (is-mapping-of<layout_left, LayoutStrideMapping> ||
//        is-mapping-of<layout_right, LayoutStrideMapping> ||
//        is-mapping-of<layout_stride, LayoutStrideMapping>))

#include <mdspan>
#include <cassert>

#include "std/containers/views/mdspan/CustomTestLayouts.h"

int main(int, char**) {
  constexpr size_t D = std::dynamic_extent;

  // working case
  {
    std::extents<int, D, D> arg_exts{100, 5};
    std::layout_stride::mapping<std::extents<int, D, D>> arg(arg_exts, std::array<int, 2>{1, 100});
  }

  return 0;
}
