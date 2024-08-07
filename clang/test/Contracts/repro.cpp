// RUN: %clang_cc1 -fcontracts -std=c++26 %s -fsyntax-only -flate-parsed-contracts -verify=expected -verify=lateerr
// RUN: %clang_cc1 -fcontracts -std=c++26 %s -fsyntax-only  -fno-late-parsed-contracts -verify=todoerr -verify=expected
constexpr int g(int x) pre(x); // expected-note 0-1 {{with fewer contracts here (1 vs 2)}}
constexpr int g(int y) pre(y);

template <typename T>
constexpr int f() pre(T{}) {};
// todoerr-note@-1 {{contract previously specified with a non-equivalent condition}}
// todoerr-warning@-2 0 {{return a value}}

template <typename U>
constexpr int f() pre(U{} + 1); // todoerr-error {{function redeclaration differs in contract specifier sequence}}
// todoerr-note@-1 {{contract specified here}}
//..

constexpr int g(int z) pre(z); // expected-note 0-1 {{with fewer contracts here (1 vs 2)}}
constexpr int g(int x) pre(0) post(1) // expected-error {{redeclaration differs}}
{}
// lateerr-warning@-1 {{return a value}}

constexpr int z(int x);  // expected-note {{previously declared without contracts here}}
constexpr int z(int x) pre(x); // expected-error {{redeclaration differs}}
