// [dcl.contract.func]p4: First declarations of a function in different
// translation units shall specify the same function-contract-specifier-seq,
// no diagnostic required. Check that we diagnose a mismatch when the
// declarations are merged, for header units, Clang modules and (if the ODR
// checks for it are enabled) the global module fragment.
//
// RUN: rm -rf %t
// RUN: split-file %s %t
//
// Header units.
// RUN: %clang_cc1 -std=c++26 -fcontracts -emit-header-unit -xc++-user-header \
// RUN:   %t/a.h -o %t/a.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -emit-header-unit -xc++-user-header \
// RUN:   %t/b.h -o %t/b.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodule-file=%t/a.pcm \
// RUN:   -fmodule-file=%t/b.pcm %t/use-header-units.cpp -fsyntax-only \
// RUN:   -verify=expected,units -Wno-experimental-header-units
//
// Clang modules.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodules -fimplicit-module-maps \
// RUN:   -fmodules-cache-path=%t/cache -I%t %t/use-modules.cpp -fsyntax-only \
// RUN:   -verify=expected,modules
//
// A textual declaration followed by the import of a module.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodules -fimplicit-module-maps \
// RUN:   -fmodules-cache-path=%t/cache -I%t %t/use-textual.cpp -fsyntax-only \
// RUN:   -verify=textual
//
// The global module fragment. The driver disables the ODR checks for it by
// default.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fskip-odr-check-in-gmf \
// RUN:   %t/m1.cppm -emit-module-interface -o %t/m1.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fskip-odr-check-in-gmf \
// RUN:   %t/m2.cppm -emit-module-interface -o %t/m2.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fskip-odr-check-in-gmf \
// RUN:   -fmodule-file=m1=%t/m1.pcm -fmodule-file=m2=%t/m2.pcm \
// RUN:   %t/use-gmf.cpp -fsyntax-only -verify=nogmf
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   %t/m1.cppm -emit-module-interface -o %t/m1.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   %t/m2.cppm -emit-module-interface -o %t/m2.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   -fmodule-file=m1=%t/m1.pcm -fmodule-file=m2=%t/m2.pcm \
// RUN:   %t/use-gmf.cpp -fsyntax-only -verify=gmf

//--- module.modulemap
module A { header "a.h" }
module B { header "b.h" }

//--- a.h
#ifndef A_H
#define A_H
int pred(int x) pre(x > 0);
int missing(int x) pre(x > 0);
int kind(const int x) pre(x > 0) post(r : r > x);
int name(const int x) post(r : x > 0);
template <class T> T tmpl(T x) pre(x > T{});
template <class T> T tmissing(T x) pre(x > T{});
struct S {
  friend int fr(S &s, int x) pre(x > 0);
};
// Renaming of the parameters, template parameters and of the result binding
// is allowed, including within lambda-expressions.
int same(const int x) pre(x > 0) post(r : r > x);
template <class T> T tsame(T x) pre(x > T{}) pre([](T v) { return v > T{}; }(x));
int lsame(int x) pre([x] { return x > 0; }()) pre([](int v) { return v > 0; }(x));
// The closure types of corresponding lambda-expressions are merged; their
// definitions are required to be the same.
int ldiff(int x) pre([](int v) { return v > 0; }(x));
// An implicit instantiation of the declaration, but not of its contracts.
inline int use_a(int x) { return tsame(x); }
#endif

//--- b.h
#ifndef B_H
#define B_H
int pred(int x) pre(x > 1);
int missing(int x);
int kind(const int x) post(r : r > x) pre(x > 0);
int name(const int x) post(x > 0);
template <class T> T tmpl(T x) pre(x >= T{});
template <class T> T tmissing(T x);
struct S {
  friend int fr(S &s, int x) pre(x > 1);
};
int same(const int y) pre(y > 0) post(s : s > y);
template <class U> U tsame(U y) pre(y > U{}) pre([](U v) { return v > U{}; }(y));
int lsame(int y) pre([y] { return y > 0; }()) pre([](int v) { return v > 0; }(y));
int ldiff(int x) pre([](int v) { return v > 1; }(x));
template <class U> U tsame(U y);
#endif

//--- use.h
inline int use() {
  S s;
  return pred(1) + missing(1) + kind(1) + name(1) + tmpl(1) + tmissing(1) +
         fr(s, 1) + same(1) + tsame(1) + lsame(1) + ldiff(1);
}
// expected-error-re@b.h:3 {{'pred' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has this predicate in its 1st function contract specifier}}
// expected-note-re@a.h:3 {{but the declaration in module '{{.*}}' has a different predicate}}
// expected-error-re@b.h:4 {{'missing' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has no function contract specifiers}}
// expected-note-re@a.h:4 {{but the declaration in module '{{.*}}' has 1 function contract specifier}}
// expected-error-re@b.h:5 {{'kind' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has a postcondition as its 1st function contract specifier}}
// expected-note-re@a.h:5 {{but the declaration in module '{{.*}}' has a precondition}}
// expected-error-re@b.h:6 {{'name' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has a 1st function contract specifier without a result name}}
// expected-note-re@a.h:6 {{but the declaration in module '{{.*}}' has a function contract specifier with a result name}}
// expected-error-re@b.h:7 {{'tmpl' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has this predicate in its 1st function contract specifier}}
// expected-note-re@a.h:7 {{but the declaration in module '{{.*}}' has a different predicate}}
// expected-error-re@b.h:8 {{'tmissing' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has no function contract specifiers}}
// expected-note-re@a.h:8 {{but the declaration in module '{{.*}}' has 1 function contract specifier}}
// expected-error-re@b.h:10 {{'fr' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has this predicate in its 1st function contract specifier}}
// expected-note-re@a.h:10 {{but the declaration in module '{{.*}}' has a different predicate}}
// expected-error-re@a.h:19 {{'(lambda at {{.*}}a.h:19:22)' has different definitions in different modules; first difference is definition in module '{{.*}}' found method 'operator()' with body}}
// expected-note-re@b.h:15 {{but in '{{.*}}' found method 'operator()' with different body}}

//--- use-header-units.cpp
import "a.h";
import "b.h";
#include "use.h"

//--- use-modules.cpp
#include "a.h"
#include "b.h"
#include "use.h"

//--- textual.h
int pred(int x) pre(x > 2);
int same(const int z) pre(z > 0) post(t : t > z);

//--- use-textual.cpp
#include "textual.h"
#include "a.h"
int use() { return pred(1) + same(1); }
// textual-error@a.h:3 {{'pred' has different function contract specifiers in different translation units; declaration in module 'A' has this predicate in its 1st function contract specifier}}
// textual-note@textual.h:1 {{but this declaration has a different predicate}}

//--- gmf.h
int gmf(int x) pre(x > COND);

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

//--- use-gmf.cpp
// nogmf-no-diagnostics
import m1;
import m2;
int main() { return use1(1) + use2(20); }
// gmf-error@gmf.h:1 {{'gmf' has different function contract specifiers in different translation units; declaration in module 'm2.<global>' has this predicate in its 1st function contract specifier}}
// gmf-note@gmf.h:1 {{but the declaration in module 'm1.<global>' has a different predicate}}
