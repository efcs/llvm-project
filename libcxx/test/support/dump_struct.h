#ifndef LIBCXX_TEST_SUPPORT_DUMP_STRUCT_H
#define LIBCXX_TEST_SUPPORT_DUMP_STRUCT_H

#include <string>
#include <memory>
#include <cstdio>
#include <iostream>
#include <cassert>
#include "test_macros.h"

namespace dump_struct_impl {
#ifdef __clang__
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wformat-security"
#endif
template <class... Args>
TEST_CONSTEXPR_CXX20 std::string dump_struct_printf(const char* fmt, Args... args) {
  std::string result;
  int buf_size = 1024;
  auto do_it   = [&]() {
    std::unique_ptr<char> buff(new char[buf_size]);
    int count = std::snprintf(buff.get(), buf_size, fmt, args...);
    if (count < buf_size) {
      assert(buff.get()[count] == '\0');
      result = buff.get();
      return true;
    } else {
      buf_size = count + 4;
      return false;
    }
  };

  for (int i = 0; i < 3; ++i) {
    if (do_it()) {
      break;
    }
  }
  return result;
}
#ifdef __clang__
#  pragma clang diagnostic pop
#endif

template <class... Args>
TEST_CONSTEXPR_CXX20 void dump_struct_formatter(std::string& out, const char* fmt, Args... args) {
  std::string result = dump_struct_printf(fmt, args...);
  out += result;
}

} // namespace dump_struct_impl

template <class T>
std::string dump_struct(T* obj) {
  std::string result;
  __builtin_dump_struct(obj, ::dump_struct_impl::dump_struct_formatter, result);
  return result;
}

template <class T>
void print_struct(T* obj) {
  std::cout << dump_struct(obj) << std::endl;
}

#endif // LIBCXX_TEST_SUPPORT_DUMP_STRUCT_H