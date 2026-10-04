// Test that mismatched function contracts on definitions of the same entity in
// different modules are diagnosed as ODR violations.
//
// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodules -fimplicit-module-maps \
// RUN:   -fmodules-cache-path=%t/cache -I%t %t/use.cpp -verify
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodules -fimplicit-module-maps \
// RUN:   -fmodules-cache-path=%t/cache2 -I%t %t/same.cpp -verify
//
// Inline functions in the global module fragments of named modules.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   %t/m1.cppm -emit-module-interface -o %t/m1.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   %t/m2.cppm -emit-module-interface -o %t/m2.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   -fmodule-file=m1=%t/m1.pcm -fmodule-file=m2=%t/m2.pcm \
// RUN:   %t/gmf-use.cpp -fsyntax-only -verify

//--- module.modulemap
module A { header "a.h" }
module B { header "b.h" }
module C { header "c.h" }

//--- a.h
inline int f(int x) pre(x > 0) { return x; }
inline int g(const int x) pre(x > 0) { return x; }
struct S { int m(int x) pre(x > 0) { return x; } };
inline int same(const int x) pre(x > 0) post(r : r == x) { return x; }

//--- b.h
inline int f(int x) pre(x > 1) { return x; }
inline int g(const int x) post(x > 0) { return x; }
struct S { int m(int x) pre(x > 1) { return x; } };

//--- c.h
inline int same(const int x) pre(x > 0) post(r : r == x) { return x; }

//--- use.cpp
#include "a.h"
#include "b.h"

int use() { return f(1) + g(1) + S{}.m(1); }
// FIXME: These should say that the first difference is in the function
// contract specifiers rather than in the body.
// expected-error@b.h:1 {{'f' has different definitions in different modules; definition in module 'B' first difference is function body}}
// expected-note@a.h:1 {{but in 'A' found a different body}}
// expected-error@b.h:2 {{'g' has different definitions in different modules; definition in module 'B' first difference is function body}}
// expected-note@a.h:2 {{but in 'A' found a different body}}
// expected-error@a.h:3 {{'S' has different definitions in different modules; first difference is definition in module 'A' found method 'm' with body}}
// expected-note@b.h:3 {{but in 'B' found method 'm' with different body}}

//--- same.cpp
// expected-no-diagnostics
#include "a.h"
#include "c.h"
int use() { return same(1); }

//--- gmf.h
inline int gmf(int x) pre(x > COND) { return x; }

//--- m1.cppm
module;
#define COND 0
#include "gmf.h"
export module m1;
export inline int use1(int x) { return gmf(x); }

//--- m2.cppm
module;
#define COND 10
#include "gmf.h"
export module m2;
export inline int use2(int x) { return gmf(x); }

//--- gmf-use.cpp
import m1;
import m2;
int main() { return use1(1) + use2(20); }
// expected-error@gmf.h:1 {{'gmf' has different definitions in different modules; definition in module 'm2.<global>' first difference is function body}}
// expected-note@gmf.h:1 {{but in 'm1.<global>' found a different body}}
