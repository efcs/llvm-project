// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++20 -fsyntax-only -verify %s

// Without -fcontracts none of the contract keywords (including the reserved
// spellings) are recognized.
int pre = 1, post = 2, __pre = 3, __post = 4;
int contract_assert = 5, __contract_assert = 6;

void f(int x) __pre(x > 0); // expected-error {{expected function body after function declarator}}
