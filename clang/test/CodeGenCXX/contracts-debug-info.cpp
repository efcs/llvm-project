// The checks of the function contract assertions carry the source location
// of the assertion, so that stepping through a function goes from the
// preconditions to the body to the postconditions, and a violation is
// reported at the assertion.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -triple x86_64-linux-gnu -debug-info-kind=limited -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce \
// RUN:   -fcxx-exceptions -fexceptions -triple x86_64-linux-gnu -debug-info-kind=limited \
// RUN:   -emit-llvm -o - %s | FileCheck %s
//
// Inlining must not leave debug locations attached to the wrong subprogram.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -fcxx-exceptions -fexceptions -triple x86_64-linux-gnu -debug-info-kind=limited \
// RUN:   -O2 -emit-llvm -o /dev/null %s

int work(int);

__attribute__((always_inline)) inline int inlined(const int x)
    pre(x > 0)
    post(r : r > x) {
  return work(x);
}

// CHECK-LABEL: define {{.*}} @_Z1fi(
int f(const int x)
    pre(x > 0)
    post(r : r > x)
{
  return work(x);
}

int g(const int x) { return inlined(x); }

// CHECK: br i1 %{{.*}}, label %contract.end, label %contract.violation{{.*}}, !dbg ![[PRE:[0-9]+]]
// CHECK: contract.violation:
// CHECK-NEXT: call void @{{.*}}, !dbg ![[PRE]]
// CHECK: call {{.*}} @_Z4worki({{.*}}, !dbg ![[BODY:[0-9]+]]
// CHECK: br i1 %{{.*}}, label %contract.end{{.*}}, label %contract.violation{{.*}}, !dbg ![[POST:[0-9]+]]
// CHECK: contract.violation{{[0-9]+}}:
// CHECK-NEXT: call void @{{.*}}, !dbg ![[POST]]
// CHECK: ret i32 %{{.*}}, !dbg ![[END:[0-9]+]]
//
// CHECK-DAG: ![[PRE]] = !DILocation(line: [[@LINE-17]], column: 5,
// CHECK-DAG: ![[BODY]] = !DILocation(line: [[@LINE-15]], column: 10,
// CHECK-DAG: ![[POST]] = !DILocation(line: [[@LINE-18]], column: 5,
// CHECK-DAG: ![[END]] = !DILocation(line: [[@LINE-16]], column: 1,
