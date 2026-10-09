// The text of the predicate of a contract assertion is stored in the AST. A
// translation unit which includes a precompiled header doesn't re-read it
// from the source, which may no longer be what the header was built from.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: split-file %s %t
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -x c++-header -emit-pch %t/h.h -o %t/h.pch
//
// Build a second header on top of the first one.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -x c++-header -include-pch %t/h.pch -emit-pch %t/h2.h -o %t/h2.pch
//
// Change the spelling of the predicates in the headers, keeping the size of
// the files and the position of everything in them.
// RUN: sed -e 's/x > 0/y < 9/' -e 's/GT(x/LT(y/' -e 's/r > 1/s < 8/' \
// RUN:     -e 's/x != 3/y == 7/' -e 's/x > T{}/y < U{}/' %t/h.h > %t/h.tmp
// RUN: cp %t/h.tmp %t/h.h
// RUN: sed -e 's/x > 5/y < 4/' %t/h2.h > %t/h2.tmp
// RUN: cp %t/h2.tmp %t/h2.h
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fno-validate-pch -include-pch %t/h2.pch %t/use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/use.cpp

//--- h.h
#define GT(a, b) ((a) > (b))
inline int f(const int x)
  pre(x > 0)
  pre(GT(x,
         1))
  post(r   :   r > 1) {
  contract_assert(x != 3);
  return x;
}
template <class T> T t(T x) pre(x > T{}) { return x; }
int decl_only(int x) pre(x > 0);

//--- h2.h
inline int g(int x) pre(x > 5) { return x; }
// The contracts of the first declaration, from the first header.
inline int decl_only(int x) { return x; }

//--- use.cpp
int main() { return f(1) + t(2) + decl_only(3) + g(4); }

// CHECK-DAG: c"x > 0\00"
// CHECK-DAG: c"GT(x, 1)\00"
// CHECK-DAG: c"r : r > 1\00"
// CHECK-DAG: c"x != 3\00"
// CHECK-DAG: c"x > T{}\00"
// CHECK-DAG: c"x > 5\00"
// CHECK-NOT: y < 9
// CHECK-NOT: LT(y
// CHECK-NOT: s < 8
// CHECK-NOT: y == 7
// CHECK-NOT: y < U{}
// CHECK-NOT: y < 4
