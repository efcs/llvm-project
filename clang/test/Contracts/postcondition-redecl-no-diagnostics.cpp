// RUN:  %clang_cc1 -std=c++26 -fcontracts  -fsyntax-only -verify %s

int f() post(r: r > 0);
int f() post(r: r > 0); // expected-no-diagnostics
