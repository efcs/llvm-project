// A parameter used only inside a contract predicate is considered used: no
// spurious -Wunused-parameter warning. (GCC mirror:
// g++.dg/contracts/cpp26/unused_warning.C.)
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wall -Wextra -verify %s
// expected-no-diagnostics

void f(int x) pre(x > 0) {}
int g(const int y) post(r : r > y) { return 1; }
void h(int z) { contract_assert(z > 0); }
