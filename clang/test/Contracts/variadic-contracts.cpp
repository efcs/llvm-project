// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -verify %s
// expected-no-diagnostics

// Contracts on variadic functions

#include <stdarg.h>

int sum(int count, ...)
  pre (count > 0)
  post (r: r >= 0)
{
  va_list args;
  va_start(args, count);
  int total = 0;
  for (int i = 0; i < count; ++i)
    total += va_arg(args, int);
  va_end(args);
  return total;
}

// Variadic with pointer parameter in precondition
int formatted(const char *fmt, ...)
  pre (fmt != nullptr)
{
  return 0;
}

// Template variadic (parameter pack, not C varargs)
template<typename... Args>
int count_args(Args... args)
  pre (sizeof...(args) > 0)
{
  return sizeof...(args);
}

void test() {
  sum(3, 1, 2, 3);
  formatted("hello %d", 42);
  count_args(1, 2, 3);
}
