// The contracts feature-test macro has the expected value under -fcontracts.
// (GCC asserts __cpp_contracts >= 202502L in several cpp26 tests.)
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// expected-no-diagnostics

#if !defined(__cpp_contracts)
#error "__cpp_contracts must be defined with -fcontracts"
#endif
static_assert(__cpp_contracts == 202502L, "unexpected __cpp_contracts value");
