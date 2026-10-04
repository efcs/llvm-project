// A naked function has no prologue or epilogue, so its function contract
// assertions cannot be evaluated: there are neither parameters nor a result
// object to evaluate them with. Check that we don't try to.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce \
// RUN:   -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -triple aarch64-linux-gnu -emit-llvm -o - %s | FileCheck %s

bool check(int);

// CHECK-LABEL: define {{.*}} @_Z5nakedi(
// CHECK-NOT: check
// CHECK: call void asm
// CHECK-NOT: check
// CHECK: unreachable
// CHECK-NOT: check
// CHECK: }
__attribute__((naked)) int naked(int x) pre(check(x)) post(r : check(r)) {
  __asm__("ret");
}
