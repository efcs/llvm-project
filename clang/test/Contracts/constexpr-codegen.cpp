// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -Wno-division-by-zero -fcontract-evaluation-semantic=ignore -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,IGNORE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -Wno-division-by-zero -fcontract-evaluation-semantic=observe -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,CHECKED
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -Wno-division-by-zero -fcontract-evaluation-semantic=enforce -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,CHECKED
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -Wno-division-by-zero -fcontract-evaluation-semantic=quick_enforce -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,CHECKED
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -Wno-division-by-zero -fcontract-evaluation-semantic=enforce -emit-llvm -o - %s -fexperimental-new-constant-interpreter | FileCheck %s --check-prefixes=CHECK,CHECKED

// Whether a variable is constant-initialized does not depend on the
// evaluation semantic of the contract assertions that its initializer
// evaluates ([expr.const.init]p4), and a call whose contract assertions are
// violated or can't be evaluated at compile time is never constant-folded
// when the contract assertions are checked: the violation has to be handled
// at run time.

constexpr int l(int c) pre(c >= 2) {
  return (c % 2 == 0) ? c / 0 : c;
}

int runtime(int);
constexpr int checked_at_runtime(int c) pre(runtime(c) > 0) { return c; }

// Dynamic initialization: the initializer is not a constant expression even
// if the precondition is ignored.
// CHECK-DAG: @dynamic0 = global i32 0, align 4
int dynamic0 = l(0);
// CHECK-DAG: @dynamic2 = global i32 0, align 4
int dynamic2 = l(2);

// Constant initialization, with any evaluation semantic.
// CHECK-DAG: @static3 = global i32 3, align 4
int static3 = l(3);
// CHECK-DAG: @_ZL13const_static3 = internal constant i32 3, align 4
const int const_static3 = l(3);
const int *use_const_static3 = &const_static3;

// The initializers of the dynamically initialized variables call the
// function, which checks its contract assertions.
// CHECK: define internal void @__cxx_global_var_init
// CHECK: call noundef i32 @_Z1li(i32 noundef 0)
// CHECK: define internal void @__cxx_global_var_init
// CHECK: call noundef i32 @_Z1li(i32 noundef 2)

// CHECK-LABEL: define {{.*}} @_Z5locali(
int local(int x) {
  // A violated contract assertion prevents constant folding.
  // IGNORE-NOT: call noundef i32 @_Z1li(
  // CHECKED: call noundef i32 @_Z1li(i32 noundef 1)
  if (l(1))
    return 1;
  // CHECKED: call noundef i32 @_Z1li(i32 noundef 1)
  switch (x) {
  case 0:
    return l(1) ? 2 : 3;
  }
  // And so does one whose predicate can't be evaluated at compile time.
  // CHECKED: call noundef i32 @_Z18checked_at_runtimei(i32 noundef 1)
  if (checked_at_runtime(1))
    return 4;
  return 0;
}
