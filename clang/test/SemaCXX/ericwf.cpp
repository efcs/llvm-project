// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts


void foo(int x) {
  int y = x;
  contract_assert(++y);
  ++y;
}
