// The closure types of the lambda-expressions within the function contract
// specifiers of a function's first declaration are the ones its definition
// evaluates, also when the declaration and the definition are in different
// translation units (or module units), and they are numbered before the
// closure types within the definition. The closure types of a repetition of
// the contracts are never emitted.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: split-file %s %t
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   %t/M.cppm -emit-module-interface -o %t/M.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fmodule-file=M=%t/M.pcm %t/M-impl.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/M-impl.cpp
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fmodule-file=M=%t/M.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp
//
// The first declaration in a header included in the global module fragment
// and in the importing translation unit.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -I%t \
// RUN:   %t/G.cppm -emit-module-interface -o %t/G.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -I%t \
// RUN:   -fmodule-file=G=%t/G.pcm %t/G-use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/G-use.cpp
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -I%t \
// RUN:   -fmodule-file=G=%t/G.pcm -DREPEAT %t/G-use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/G-use.cpp

//--- M.cppm
export module M;

// Declared with a lambda in the precondition, defined (with a lambda in the
// body) in the implementation unit.
export int f(int x) pre([](int v) { return v > 0; }(x));

// Inline: declared, then defined in the interface.
export inline int g(int x) pre([](int v) { return v > 0; }(x));
inline int g(int x) { auto l = [](int v) { return v + 1; }; return l(x); }

// Inline, defined with the contracts.
export inline int h(int x) pre([](int v) { return v > 0; }(x)) {
  auto l = [](int v) { return v + 1; };
  return l(x);
}

export template <class T> T t(T x) pre([](T v) { return v > 0; }(x));
template <class T> T t(T x) { auto l = [](T v) { return v + 1; }; return l(x); }

//--- M-impl.cpp
module M;
int f(int x) { auto l = [](int v) { return v + 1; }; return l(x); }

// CHECK-LABEL: define {{.*}} i32 @_ZW1M1fi(
// CHECK: call {{.*}} i1 @_ZZW1M1fiENKUliE_clEi(
// CHECK: call {{.*}} i32 @_ZZW1M1fiENKUliE0_clEi(
// CHECK-LABEL: define {{.*}} i1 @_ZZW1M1fiENKUliE_clEi(
// CHECK-LABEL: define {{.*}} i32 @_ZZW1M1fiENKUliE0_clEi(

//--- Use.cpp
import M;
int use(int a) { return f(a) + g(a) + h(a) + t(a); }

// CHECK-LABEL: define {{.*}} i32 @_ZW1M1gi(
// CHECK: call {{.*}} i1 @_ZZW1M1giENKUliE_clEi(
// CHECK: call {{.*}} i32 @_ZZW1M1giENKUliE0_clEi(
// CHECK-LABEL: define {{.*}} i32 @_ZW1M1hi(
// CHECK: call {{.*}} i1 @_ZZW1M1hiENKUliE_clEi(
// CHECK: call {{.*}} i32 @_ZZW1M1hiENKUliE0_clEi(
// CHECK-LABEL: define {{.*}} i32 @_ZW1M1tIiET_S1_(
// CHECK: call {{.*}} i1 @_ZZW1M1tIiET_S1_ENKUliE_clEi(
// CHECK: call {{.*}} i32 @_ZZW1M1tIiET_S1_ENKUliE0_clEi(

//--- hdr.h
#ifndef HDR_H
#define HDR_H
inline int hf(int x) pre([](int v) { return v > 0; }(x));
inline int hg(int x) pre([](int v) { return v > 0; }(x));
#ifdef REPEAT
inline int hf(int x) pre([](int v) { return v > 0; }(x)) {
#else
inline int hf(int x) {
#endif
  auto l = [](int v) { return v + 1; };
  return l(x);
}
#endif

//--- G.cppm
module;
#include "hdr.h"
export module G;
export int gf(int x) { return hf(x) + hg(x); }

//--- G-use.cpp
#include "hdr.h"
import G;
inline int hg(int x) { auto l = [](int v) { return v + 1; }; return l(x); }
int use(int a) { return gf(a) + hf(a) + hg(a); }

// CHECK-LABEL: define {{.*}} i32 @_Z2hfi(
// CHECK: call {{.*}} i1 @_ZZ2hfiENKUliE_clEi(
// CHECK: call {{.*}} i32 @_ZZ2hfiENKUliE0_clEi(
// CHECK-LABEL: define {{.*}} i32 @_Z2hgi(
// CHECK: call {{.*}} i1 @_ZZ2hgiENKUliE_clEi(
// CHECK: call {{.*}} i32 @_ZZ2hgiENKUliE0_clEi(
// CHECK-LABEL: define {{.*}} i1 @_ZZ2hfiENKUliE_clEi(
// CHECK-LABEL: define {{.*}} i32 @_ZZ2hfiENKUliE0_clEi(
// CHECK-LABEL: define {{.*}} i1 @_ZZ2hgiENKUliE_clEi(
// CHECK-LABEL: define {{.*}} i32 @_ZZ2hgiENKUliE0_clEi(
