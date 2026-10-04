// RUN: %clang_cc1 -std=c++26 -fcontracts -ast-print %s -o %t.cpp
// RUN: FileCheck %s --input-file %t.cpp
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Werror %t.cpp
// RUN: %clang_cc1 -std=c++26 -fcontracts -ast-print %t.cpp -o %t2.cpp
// RUN: diff %t.cpp %t2.cpp

int g(int);

// CHECK: int f(int a) pre(a > 0) pre(a < 100) post(r : r > 0) {
// CHECK-NEXT: {{^    }}contract_assert(a != 5);
// CHECK-NEXT: {{^    }}return a;
int f(int a) pre(a > 0) pre(a < 100) post(r : r > 0) {
  contract_assert(a != 5);
  return a;
}

// CHECK: int decl(const int a) pre(a > 0) post(r : r > a);
int decl(const int a) pre(a > 0) post(r : r > a);

// CHECK: struct S {
struct S {
  int x;
  // Contracts come before the ctor-initializer.
  // CHECK: S(int y) pre(y > 0) : x(y) {
  S(int y) pre(y > 0) : x(y) {}
  // CHECK: int get() const post(r : r == this->x) {
  int get() const post(r : r == x) { return x; }
};

struct B {
  // CHECK: virtual int v(int a) const pre(a > 0) = 0;
  virtual int v(int a) const pre(a > 0) = 0;
};
struct D : B {
  // CHECK: int v(int a) const override pre(a > 0) post(r : r > 0) {
  int v(int a) const override pre(a > 0) post(r : r > 0) { return a; }
};

// CHECK: template <class T> void k(T x) requires true pre(x > 0);
template <class T> void k(T x) requires true pre(x > 0);

// CHECK: template <class T> T tf(T x) pre(x > 0) {
template <class T> T tf(T x) pre(x > 0) { return x; }

// CHECK: auto lam = [](int q) pre {{\[\[}}clang::contract_group("foo"){{\]\]}} (g(q) > 0) post(r : r > 0) {
// CHECK-NEXT: {{^    }}contract_assert(q);
auto lam = [](int q) pre [[clang::contract_group("foo")]] (g(q) > 0)
               post(r : r > 0) {
  contract_assert(q);
  return q;
};

// CHECK: void h(int z) {
// CHECK-NEXT: {{^    }}if (z)
// CHECK-NEXT: {{^        }}contract_assert(z > 1);
// CHECK-NEXT: {{^    }}{
// CHECK-NEXT: {{^        }}contract_assert {{\[\[}}clang::contract_message("hi", 1){{\]\]}} (z > 2);
// CHECK-NEXT: {{^    }}}
// CHECK-NEXT: }
void h(int z) {
  if (z)
    contract_assert(z > 1);
  {
    contract_assert [[clang::contract_message("hi")]] (z > 2);
  }
}
