// The predicate of a function contract specifier can name entities with
// internal or no linkage: entities in an unnamed namespace, static functions,
// namespace-scope constants, unnamed enumerations, ... Each translation unit
// has its own such entities, which are never merged, so when the function
// contract specifiers of first declarations in different translation units
// are compared ([dcl.contract.func]p4), these entities are identified by
// their names (as they are by the ODR hash of a definition) rather than by
// their identity. [basic.def.odr]p14 permits such predicates for the
// constants; the comparison is lenient for the others.
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
// RUN:   -verify -Wno-experimental-header-units
//
// Clang modules.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodules -fimplicit-module-maps \
// RUN:   -fmodules-cache-path=%t/cache -I%t %t/use-modules.cpp -fsyntax-only \
// RUN:   -verify
//
// The global module fragment, with its ODR checks enabled.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   %t/m1.cppm -emit-module-interface -o %t/m1.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   %t/m2.cppm -emit-module-interface -o %t/m2.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   -fmodule-file=m1=%t/m1.pcm -fmodule-file=m2=%t/m2.pcm \
// RUN:   %t/use-gmf.cpp -fsyntax-only -verify
//
// Module partitions whose global module fragments provide the entities.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   %t/pa.cppm -emit-module-interface -o %t/P-A.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   %t/pb.cppm -emit-module-interface -o %t/P-B.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   -fprebuilt-module-path=%t %t/p.cppm -emit-module-interface \
// RUN:   -o %t/P.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-skip-odr-check-in-gmf \
// RUN:   -fprebuilt-module-path=%t %t/use-partitions.cpp -fsyntax-only -verify

//--- module.modulemap
module A { header "a.h" }
module B { header "b.h" }

//--- tl.h
#ifndef TL_H
#define TL_H
namespace {
struct Tag { int v; };
constexpr int kAnon = 3;
constexpr int kOther = 4;
enum { kAnonEnumerator = 2 };
enum class Scoped { Zero, One };
inline bool afn(int v) { return v > 0; }
inline bool afn(long v) { return v > 1; }
template <class T> struct Wrap { static constexpr T value = T{}; };
template <class T> constexpr T kTmpl = T{};
} // namespace
static bool sfn(int v) { return v > 0; }
static const int kStatic = 1;
constexpr int kConst = 10;
enum { kLimit = 5 };
typedef struct { int v; } Anon;
#endif

//--- decls.h
#ifndef DECLS_H
#define DECLS_H
int f1(int x) pre(x > kAnon);
int f2(int x) pre(sizeof(Tag) > 0 && x > 0);
int f3(int x) pre(x > kAnonEnumerator);
int f4(int x) pre(afn(x));
int f5(int x) pre(sfn(x));
int f6(int x) pre(x > kStatic && x < kConst);
int f7(int x) pre(x < kLimit);
int f8(const Tag *t) pre(t == nullptr || t->v > 0);
int f9(int x) pre(Tag{x}.v > 0);
int f10(int x) pre(x > Wrap<int>::value);
int f11(int x) pre(x > kTmpl<int>);
int f12(int x) pre(afn(static_cast<long>(x)));
int f13(int x) pre(static_cast<Scoped>(x) != Scoped::Zero);
int f14(const Anon *a) pre(a == nullptr || a->v > 0);
int f15(int Tag::*pm) pre(pm == nullptr || &Tag::v == pm);
template <class T> T f16(T x) pre(x > T{kAnon});
template <class T> T f17(T x) pre(sizeof(Wrap<T>) > 0 && x > T{});
inline int f18(int x) pre(afn(x)) { return x; }
int f19(const int x) post(r : r > kAnon && afn(r));
#endif

//--- a.h
#include "tl.h"
#include "decls.h"
int diff(int x) pre(x > kAnon);
int diff_fn(int x) pre(afn(x));

//--- b.h
#include "tl.h"
#include "decls.h"
int diff(int x) pre(x > kOther);
int diff_fn(int x) pre(afn(static_cast<long>(x)));

//--- use.h
// (f8, f14 and f15 take parameters of TU-local types, so each translation
// unit declares a different function; they are not merged nor compared.)
inline int use() {
  return f1(1) + f2(1) + f3(1) + f4(1) + f5(1) + f6(1) + f7(1) + f9(1) +
         f10(1) + f11(1) + f12(1) + f13(1) + f16(1) + f17(1) + f18(1) +
         f19(1) + diff(1) + diff_fn(1);
}
// Different names, and a different overload, are different predicates.
// expected-error-re@b.h:3 {{'diff' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has this predicate in its 1st function contract specifier}}
// expected-note-re@a.h:3 {{but the declaration in module '{{.*}}' has a different predicate}}
// expected-error-re@b.h:4 {{'diff_fn' has different function contract specifiers in different translation units; declaration in module '{{.*}}' has this predicate in its 1st function contract specifier}}
// expected-note-re@a.h:4 {{but the declaration in module '{{.*}}' has a different predicate}}

//--- use-header-units.cpp
import "a.h";
import "b.h";
#include "use.h"

//--- use-modules.cpp
#include "a.h"
#include "b.h"
#include "use.h"

//--- m1.cppm
module;
#include "a.h"
export module m1;
export inline int use1() {
  return f1(1) + f2(1) + f3(1) + f4(1) + f5(1) + f6(1) + f7(1) + f9(1) +
         f10(1) + f11(1) + f12(1) + f13(1) + f16(1) + f17(1) + f18(1) +
         f19(1) + diff(1) + diff_fn(1);
}

//--- m2.cppm
module;
#include "b.h"
export module m2;
export inline int use2() {
  return f1(1) + f2(1) + f3(1) + f4(1) + f5(1) + f6(1) + f7(1) + f9(1) +
         f10(1) + f11(1) + f12(1) + f13(1) + f16(1) + f17(1) + f18(1) +
         f19(1) + diff(1) + diff_fn(1);
}

//--- use-gmf.cpp
import m1;
import m2;
int main() { return use1() + use2(); }
// expected-error@b.h:3 {{'diff' has different function contract specifiers in different translation units; declaration in module 'm2.<global>' has this predicate in its 1st function contract specifier}}
// expected-note@a.h:3 {{but the declaration in module 'm1.<global>' has a different predicate}}
// expected-error@b.h:4 {{'diff_fn' has different function contract specifiers in different translation units; declaration in module 'm2.<global>' has this predicate in its 1st function contract specifier}}
// expected-note@a.h:4 {{but the declaration in module 'm1.<global>' has a different predicate}}

//--- pa.cppm
module;
#include "tl.h"
export module P:A;
export {
#include "decls.h"
}
export int diff(int x) pre(x > kAnon);
export int diff_fn(int x) pre(afn(x));

//--- pb.cppm
module;
#include "tl.h"
export module P:B;
export {
#include "decls.h"
}
export int diff(int x) pre(x > kOther);
export int diff_fn(int x) pre(afn(static_cast<long>(x)));

//--- p.cppm
export module P;
export import :A;
export import :B;

//--- use-partitions.cpp
import P;
int main() {
  return f1(1) + f2(1) + f3(1) + f4(1) + f5(1) + f6(1) + f7(1) + f9(1) +
         f10(1) + f11(1) + f12(1) + f13(1) + f16(1) + f17(1) + f18(1) +
         f19(1) + diff(1) + diff_fn(1);
}
// expected-error@pb.cppm:7 {{'diff' has different function contract specifiers in different translation units; declaration in module 'P:B' has this predicate in its 1st function contract specifier}}
// expected-note@pa.cppm:7 {{but the declaration in module 'P:A' has a different predicate}}
// expected-error@pb.cppm:8 {{'diff_fn' has different function contract specifiers in different translation units; declaration in module 'P:B' has this predicate in its 1st function contract specifier}}
// expected-note@pa.cppm:8 {{but the declaration in module 'P:A' has a different predicate}}
