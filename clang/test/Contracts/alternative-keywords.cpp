// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts

// expected-no-diagnostics

int f(const int x) __pre(x) __post(x > 0) {
  __contract_assert(x);
  return x;
}

