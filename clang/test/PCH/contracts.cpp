// Test function contracts through a PCH.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -emit-pch -o %t %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -include-pch %t -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -include-pch %t -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -include-pch %t -ast-print %s | FileCheck %s --check-prefix=PRINT

#ifndef HEADER
#define HEADER

int f(const int x) pre(x > 0) post(r : r > x);

inline int g(const int x) pre(x > 0) post(r : r == x) {
  contract_assert(x != 3);
  return x;
}

template <class T> T t(const T x) pre(x > 0) post(r : r == x) { return x; }

struct S {
  int m(int y) pre(y > 0) { return y; }
  int get() const post(r : r == v) { return v; }
  int v = 0;
};

constexpr int ce(int x) pre(x > 0) { return x; }

inline int lam(int x) pre([](int y) { return y > 0; }(x)) {
  contract_assert([&] { return x > 0; }());
  auto l = [](int y) pre(y > 0) { return y; };
  return l(x);
}

inline int attr(int x) pre [[clang::contract_group("grp")]] (x > 0) {
  return x;
}

#else

// expected-no-diagnostics

static_assert(ce(1) == 1);

int use() { return g(1) + t(2) + S{}.m(3) + S{}.get() + lam(4) + attr(5); }

// CHECK-LABEL: define {{.*}}@_Z1gi(
// CHECK: call {{.*}}contract_violation
// CHECK-LABEL: define {{.*}}@_Z1tIiET_S0_(
// CHECK: call {{.*}}contract_violation

// PRINT: int f(const int x) pre(x > 0) post(r : r > x);
// PRINT: inline int g(const int x) pre(x > 0) post(r : r == x) {
// PRINT: inline int attr(int x) pre {{\[\[}}clang::contract_group("grp"){{\]\]}} (x > 0) {

#endif
