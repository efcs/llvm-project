// RUN:  %clang_cc1 -std=c++26 -fcontracts  -fcolor-diagnostics -fsyntax-only -verify %s
// expected-no-diagnostics

int f() post(r: r > 0);
int f() post(r: r > 0);
int f() post(w : w > 0); // Also OK