// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// P2900R14 has no message operand: the predicate is a single
// conditional-expression, so a trailing string literal is a syntax error.

void f(int x) pre(x > 0, "must be positive") {} // expected-error {{expected ')'}} expected-note {{to match}}

int g(int x) post(r: r >= 0, "non-negative") { return x; } // expected-error {{expected ')'}} expected-note {{to match}}

void h() {
  contract_assert(true, "always true"); // expected-error {{expected ')'}} expected-note {{to match}}
}
