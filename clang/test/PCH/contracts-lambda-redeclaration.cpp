// The closure types of the lambda-expressions within the function contract
// specifiers of a function's first declaration are the ones its definition
// evaluates, and they are numbered before the closure types within the
// definition, also when the first declaration comes from a precompiled header
// (whose mangling numbers were assigned in another translation unit).
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -emit-pch -o %t.pch %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -include-pch %t.pch -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -include-pch %t.pch -DREPEAT -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -include-pch %t.pch -ast-dump-all -ast-dump-filter=f %s \
// RUN:   | FileCheck %s --check-prefix=DUMP

#ifndef HEADER
#define HEADER

inline int f(int x) pre([](int v) { return v > 0; }(x));
template <class T> T t(T x) pre([](T v) { return v > 0; }(x));

#else

#ifdef REPEAT
inline int f(int x) pre([](int v) { return v > 0; }(x)) {
#else
inline int f(int x) {
#endif
  auto l = [](int v) { return v + 1; };
  return l(x);
}
template <class T> T t(T x) {
  auto l = [](T v) { return v + 1; };
  return l(x);
}
int use(int a) { return f(a) + t(a); }

// The contract lambda is the first closure type of the function, the one of
// the body is the second.
// CHECK: define {{.*}} i1 @_ZZ1fiENKUliE_clEi(
// CHECK: define {{.*}} i32 @_ZZ1fiENKUliE0_clEi(
// CHECK: define {{.*}} i1 @_ZZ1tIiET_S0_ENKUliE_clEi(
// CHECK: define {{.*}} i32 @_ZZ1tIiET_S0_ENKUliE0_clEi(

// The definition refers to the contracts of the first declaration.
// DUMP: FunctionDecl {{.*}} imported {{.*}}f 'int (int)'
// DUMP: ContractSpecifierDecl [[CSD:0x[0-9a-f]+]]
// DUMP: FunctionDecl {{.*}} prev {{.*}} f 'int (int)' inline contracts [[CSD]]

#endif
