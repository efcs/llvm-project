// Test function contracts across a chain of precompiled headers: the
// redeclaration checks of [dcl.contract.func]p4 apply to declarations from
// any link of the chain, and the contracts of a first declaration apply to a
// definition in a later link.
//
// RUN: rm -rf %t
// RUN: split-file %s %t
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -x c++-header -emit-pch %t/h1.h -o %t/h1.pch
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -x c++-header -include-pch %t/h1.pch -emit-pch %t/h2.h -o %t/h2.pch
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -include-pch %t/h2.pch %t/main.cpp -fsyntax-only -verify
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -include-pch %t/h2.pch %t/use.cpp -emit-llvm -o - | FileCheck %t/use.cpp
//
// A mismatch in the second link of the chain is diagnosed when it is built.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -x c++-header -include-pch %t/h1.pch -emit-pch %t/bad.h -o %t/bad.pch \
// RUN:   -verify

//--- h1.h
int same(const int x) pre(x > 0) post(r : r > x);
int omitted(int x) pre(x > 1);
int defined_later(int x) pre(x > 2);
int mismatch(int x) pre(x > 3);
int lambda(int x) pre([](int v) { return v > 4; }(x));
template <class T> T tmpl(T x) pre(x > T{});
template <class T> T trepeat(T x) pre(x > T{});
struct S {
  int m(int x) pre(x > 5) post(r : r > 5);
};

//--- h2.h
int same(const int y) pre(y > 0) post(s : s > y);
int omitted(int x);
inline int defined_later(int y) { return y; }
int lambda(int y) pre([](int v) { return v > 4; }(y));
template <class U> U tmpl(U y) { return y; }
template <class U> U trepeat(U y) pre(y > U{});
inline int S::m(int y) { return y; }

//--- bad.h
int mismatch(int x) pre(x > 30); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
// expected-note@h1.h:4 {{contract previously specified with a non-equivalent condition}}
int same(const int y) pre(y > 0); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@h1.h:1 {{previously declared with additional contracts here (2 vs 1)}}

//--- main.cpp
int same(const int z) pre(z > 0) post(t : t > z) { return z + 1; }
int omitted(int x) { return x; }
int lambda(int z) pre([](int v) { return v > 4; }(z)) { return z; }
int mismatch(int x) pre(x > 30) { return x; } // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
// expected-note@h1.h:4 {{contract previously specified with a non-equivalent condition}}
int lambda(int x) pre([](int v) { return v > 40; }(x)); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
// expected-note@h1.h:5 {{contract previously specified with a non-equivalent condition}}
template <class U> U tmpl(U y) pre(y >= U{}); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
// expected-note@h1.h:6 {{contract previously specified with a non-equivalent condition}}
template <class V> V trepeat(V z) pre(z > V{}) { return z; }

//--- use.cpp
int omitted(int x) { return x; }
int main() { return omitted(1) + defined_later(2) + tmpl(3) + S().m(4); }

// The contracts of the first declarations in the first header are checked by
// the definitions in the second header and in the main file.
// CHECK-DAG: c"x > 1\00"
// CHECK-DAG: c"x > 2\00"
// CHECK-DAG: c"x > T{}\00"
// CHECK-DAG: c"x > 5\00"
// CHECK-DAG: c"r : r > 5\00"
