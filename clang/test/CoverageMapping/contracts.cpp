// The predicates of the function contract assertions of a function are mapped
// like its body (they are evaluated when the function is entered and when it
// returns), with branch regions for the conditions they contain. Ignored
// contract assertions are not evaluated and are not mapped.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -fprofile-instrument=clang -fcoverage-mapping -dump-coverage-mapping \
// RUN:   -emit-llvm-only -triple x86_64-linux-gnu -main-file-name contracts.cpp %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce \
// RUN:   -fprofile-instrument=clang -fcoverage-mapping -fcoverage-mcdc -dump-coverage-mapping \
// RUN:   -emit-llvm-only -triple x86_64-linux-gnu -main-file-name contracts.cpp %s | FileCheck %s --check-prefix=MCDC
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore \
// RUN:   -fprofile-instrument=clang -fcoverage-mapping -dump-coverage-mapping \
// RUN:   -emit-llvm-only -triple x86_64-linux-gnu -main-file-name contracts.cpp %s | FileCheck %s --check-prefix=IGNORE

bool check(int);

int f(const int x, int y)
    pre(x > 0 && check(x))
    post(r : r > 0 ? check(r) : x == 0) {
  contract_assert(x || y);
  return x + y;
}

// CHECK-LABEL: _Z1fii:
// CHECK-NEXT: File 0, [[@LINE-7]]:9 -> [[@LINE-7]]:26 = #0
// CHECK-NEXT: File 0, [[@LINE-8]]:9 -> [[@LINE-8]]:14 = #0
// CHECK-NEXT: Branch,File 0, [[@LINE-9]]:9 -> [[@LINE-9]]:14 = #1, (#0 - #1)
// CHECK-NEXT: Gap,File 0, [[@LINE-10]]:14 -> [[@LINE-10]]:18 = #1
// CHECK-NEXT: File 0, [[@LINE-11]]:18 -> [[@LINE-11]]:26 = #1
// CHECK-NEXT: Branch,File 0, [[@LINE-12]]:18 -> [[@LINE-12]]:26 = #2, (#1 - #2)
// CHECK-NEXT: File 0, [[@LINE-12]]:14 -> [[@LINE-12]]:39 = #0
// CHECK-NEXT: File 0, [[@LINE-13]]:14 -> [[@LINE-13]]:19 = #0
// CHECK-NEXT: Branch,File 0, [[@LINE-14]]:14 -> [[@LINE-14]]:19 = #3, (#0 - #3)
// CHECK-NEXT: Gap,File 0, [[@LINE-15]]:21 -> [[@LINE-15]]:22 = #3
// CHECK-NEXT: File 0, [[@LINE-16]]:22 -> [[@LINE-16]]:30 = #3
// CHECK-NEXT: File 0, [[@LINE-17]]:33 -> [[@LINE-17]]:39 = (#0 - #3)
// CHECK-NEXT: File 0, [[@LINE-18]]:41 -> [[@LINE-15]]:2 = #0
// The conditions of a contract_assert are mapped too, unless it is ignored.
// CHECK-NEXT: File 0, [[@LINE-19]]:19 -> [[@LINE-19]]:20 = #0
// CHECK-NEXT: Branch,File 0, [[@LINE-20]]:19 -> [[@LINE-20]]:20 = (#0 - #4), #4
// CHECK-NEXT: Gap,File 0, [[@LINE-21]]:20 -> [[@LINE-21]]:24 = #4
// CHECK-NEXT: File 0, [[@LINE-22]]:24 -> [[@LINE-22]]:25 = #4
// CHECK-NEXT: Branch,File 0, [[@LINE-23]]:24 -> [[@LINE-23]]:25 = (#4 - #5), #5
//
// The conditions are measured for MC/DC like any other.
// MCDC-LABEL: _Z1fii:
// MCDC: Decision,File 0, [[@LINE-29]]:9 -> [[@LINE-29]]:26 = M:{{[0-9]+}}, C:2
// MCDC-NEXT: Branch,File 0, [[@LINE-30]]:9 -> [[@LINE-30]]:14 = #1, (#0 - #1) [1,2,0]
// MCDC: Branch,File 0, [[@LINE-31]]:18 -> [[@LINE-31]]:26 = #2, (#1 - #2) [2,0,0]
// MCDC: Decision,File 0, [[@LINE-30]]:19 -> [[@LINE-30]]:25 = M:{{[0-9]+}}, C:2
//
// IGNORE-LABEL: _Z1fii:
// IGNORE-NEXT: File 0, [[@LINE-34]]:41 -> [[@LINE-31]]:2 = #0
// IGNORE-NEXT: _Z1gv:
// A contract whose predicate was specified on another declaration in the same
// file is mapped as well.
int g() post(r : r > 0);
int g() { return 1; }

// CHECK-LABEL: _Z1gv:
// CHECK-NEXT: File 0, [[@LINE-4]]:18 -> [[@LINE-4]]:23 = #0
// CHECK-NEXT: File 0, [[@LINE-4]]:9 -> [[@LINE-4]]:22 = #0
