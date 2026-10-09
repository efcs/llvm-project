// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -fcontract-evaluation-semantic=ignore -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,IGNORE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -fcontract-evaluation-semantic=observe -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,CHECKED
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -fcontract-evaluation-semantic=enforce -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,CHECKED
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -fcontract-evaluation-semantic=quick_enforce -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,CHECKED
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-unknown-linux-gnu -fcontract-evaluation-semantic=enforce -emit-llvm -o - %s -fexperimental-new-constant-interpreter | FileCheck %s --check-prefixes=CHECK,CHECKED

// Whether a variable has constant destruction is determined with all contract
// assertions ignored. If the destructor evaluates a contract assertion with a
// checking semantic, the variable doesn't have constant destruction after
// all: the destructor is registered to run at exit, where it checks the
// contract assertion. With the ignore semantic the destructor is empty, so
// no destructor runs at exit.

struct D {
  int v;
  constexpr D(int v) : v(v) {}
  constexpr ~D() { contract_assert(v > 0); }
};

// All of these but 'dynamic' are constant-initialized.
// CHECK-DAG: @violated = global %struct.D zeroinitializer
// CHECK-DAG: @_ZL4also = internal global %struct.D zeroinitializer
// CHECK-DAG: @fine = global %struct.D { i32 1 }
// CHECK-DAG: @dynamic = global %struct.D zeroinitializer
D violated(0);
static D also(0);
D fine(1);
int runtime();
D dynamic(runtime());
D *use = &also;

// IGNORE-NOT: @__cxa_atexit
// CHECKED: call i32 @__cxa_atexit(ptr @_ZN1DD1Ev, ptr @violated,
// CHECKED: call i32 @__cxa_atexit(ptr @_ZN1DD1Ev, ptr @_ZL4also,
// CHECKED: call i32 @__cxa_atexit(ptr @_ZN1DD1Ev, ptr @fine,
// CHECKED: call i32 @__cxa_atexit(ptr @_ZN1DD1Ev, ptr @dynamic,
// IGNORE-NOT: @__cxa_atexit
