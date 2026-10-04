// The text of the predicate of a contract assertion is stored in the AST. A
// translation unit which imports a module doesn't re-read it from the source
// of the module, which may no longer be the source the module was built from.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: split-file %s %t
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   %t/M.cppm -emit-module-interface -o %t/M.pcm
//
// Change the spelling of the predicates in the source of the module, keeping
// the size of the file and the position of everything in it.
// RUN: sed -e 's/x > 0/y < 9/' -e 's/GT(x/LT(y/' -e 's/r > 1/s < 8/' \
// RUN:     -e 's/x != 3/y == 7/' -e 's/x > T{}/y < U{}/' %t/M.cppm > %t/M.tmp
// RUN: cp %t/M.tmp %t/M.cppm
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fmodule-file=M=%t/M.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fmodule-file=M=%t/M.pcm %t/Use.cpp -ast-dump-all \
// RUN:   -ast-dump-filter=decl_only | FileCheck %t/Use.cpp --check-prefix=DUMP

//--- M.cppm
export module M;
#define GT(a, b) ((a) > (b))
export inline int f(const int x)
  pre(x > 0)
  pre(GT(x,
         1))
  post(r   :   r > 1) {
  contract_assert(x != 3);
  return x;
}
export template <class T> T t(T x) pre(x > T{}) { return x; }
export int decl_only(int x) pre(x > 0);
export inline int message(int x)
  pre [[clang::contract_message("custom")]] (x > 0)
  pre [[clang::contract_message("only", false)]] (x > 0) {
  return x;
}

//--- Use.cpp
import M;
int main() { return f(1) + t(2) + decl_only(3) + message(4); }

// CHECK-DAG: c"x > 0\00"
// CHECK-DAG: c"GT(x, 1)\00"
// CHECK-DAG: c"r : r > 1\00"
// CHECK-DAG: c"x != 3\00"
// CHECK-DAG: c"x > T{}\00"
// CHECK-DAG: c"x > 0: \22custom\22\00"
// CHECK-DAG: c"\22only\22\00"
// CHECK-NOT: y < 9
// CHECK-NOT: LT(y
// CHECK-NOT: s < 8
// CHECK-NOT: y == 7
// CHECK-NOT: y < U{}

// DUMP: ContractStmt {{.*}} pre enforce "x > 0"
