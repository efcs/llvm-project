// [basic.contract.eval]p17: If a contract-violation handler invoked from the
// evaluation of a function contract assertion exits via an exception, the
// behavior is as if the function body exits via that same exception. A
// function-try-block is the function body, so its handlers do not get to
// handle the exception.
//
// Nothing in the function-try-blocks below can throw, so their handlers must
// not be reachable at all: no code is emitted for them.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-windows-msvc \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple arm64-apple-macosx14 \
// RUN:   -emit-llvm -o - %s | FileCheck %s

bool check() noexcept;
void body() noexcept;
void in_catch_handler() noexcept;

struct Member {
  Member() noexcept;
  ~Member();
};

// CHECK-NOT: in_catch_handler
// CHECK-NOT: catchpad
// CHECK-NOT: __cxa_begin_catch

// CHECK-LABEL: define {{.*}}function
// CHECK: call {{.*}}check
// CHECK: call {{.*}}body
// CHECK: call {{.*}}check
// CHECK: ret i32
int function() pre(check()) post(check()) try {
  body();
  return 1;
} catch (...) {
  in_catch_handler();
  return 2;
}

struct S {
  Member M;
  S() pre(check()) post(check());
  ~S() noexcept(false) pre(check()) post(check());
};

// CHECK-LABEL: define {{.*}}S
// CHECK: call {{.*}}check
// CHECK: call {{.*}}Member
// CHECK: call {{.*}}body
// CHECK: call {{.*}}check
// CHECK: ret
S::S() try : M() {
  body();
} catch (...) {
  in_catch_handler();
}

// CHECK-LABEL: define {{.*}}S
// CHECK: call {{.*}}check
// CHECK: call {{.*}}body
// CHECK: call {{.*}}Member
// CHECK: call {{.*}}check
// CHECK: ret
S::~S() noexcept(false) try {
  body();
} catch (...) {
  in_catch_handler();
}

// CHECK-NOT: in_catch_handler
// CHECK-NOT: catchpad
// CHECK-NOT: __cxa_begin_catch
