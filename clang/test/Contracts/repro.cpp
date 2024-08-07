// RUN: %clang_cc1 -fcontracts -std=c++26 %s -fsyntax-only -flate-parsed-contracts -verify=expected
// RUN: %clang_cc1 -fcontracts -std=c++26 %s -fsyntax-only  -fno-late-parsed-contracts -verify=expected
constexpr int g(int x) pre(x); // expected-note 0-1 {{with fewer contracts here (1 vs 2)}}
constexpr int g(int y) pre(y);

template <typename T>
constexpr int f() pre(T{}) {};

template <typename U>
constexpr int f() pre(U{} + 1);
//..

constexpr int g(int z) pre(z); // expected-note 0-1 {{with fewer contracts here (1 vs 2)}}
constexpr int g(int x) pre(0) post(1) // expected-error {{redeclaration differs}}
{} // expected-warning {{return a value}}

constexpr int z(int x);  // expected-note {{previously declared without contracts here}}
constexpr int z(int x) pre(x); // expected-error {{redeclaration differs}}
