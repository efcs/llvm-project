#ifndef LIBCPP_TEST_SUPPORT_NTTP_STRING_H
#define LIBCPP_TEST_SUPPORT_NTTP_STRING_H

#include <string>
#include <string_view>
#include <cstddef>
#include <cstdlib>
#include <utility>

#include "test_macros.h"

#if TEST_STD_VER < 20
#  error "This header requires C++20 support"
#endif

struct TStr {
  static constexpr unsigned MAX_SIZE = 128;

  consteval TStr() : TStr(nullptr) {}
  consteval TStr(const char* ptr) : TStr(ptr, ptr ? __builtin_strlen(ptr) : 0) {}
  consteval TStr(const char* ptr, size_t len) : size(len), buff() { copy(ptr, len); }

  constexpr TStr(TStr const& RHS) : size(0), buff() { copy(RHS.buff, RHS.size); }

  constexpr void reset() {
    __builtin_memset(buff, 0, MAX_SIZE);
    size = 0;
  }

  constexpr void copy(const char* obuff, size_t osize) {
    if (osize >= MAX_SIZE)
      throw "Size is greater than max size";
    __builtin_memcpy(buff, obuff, osize);
    for (auto* b = buff + osize; b < buff + 128; ++b)
      *b = '\0';
    size = osize;
  }

  constexpr TStr& operator=(TStr const& RHS) {
    copy(RHS.buff, RHS.size);
    return *this;
  }

  constexpr std::string str() const { return std::string(sv()); }

  constexpr std::string_view sv() const { return std::string_view(buff, size); }

  constexpr operator std::string_view() const { return std::string_view(buff, size); }

  friend auto operator<=>(TStr const& LHS, TStr const& RHS) = default;
  constexpr auto operator<=>(std::string_view RHS) const { return sv() <=> RHS; }
  size_t size         = 0;
  char buff[MAX_SIZE] = {};
};

template <>
struct ::std::hash<::TStr> {
  size_t operator()(const ::TStr& str) const { return std::hash<std::string_view>{}(str.sv()); }
};

#endif // LIBCPP_TEST_SUPPORT_NTTP_STRING_H