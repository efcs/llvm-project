// A destructor whose body is empty but which has function contract assertions
// to evaluate cannot be emitted as an alias of the destructor of its base
// class.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -triple x86_64-linux-gnu -mconstructor-aliases -O1 -disable-llvm-passes \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ITANIUM
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=quick_enforce \
// RUN:   -triple x86_64-windows-msvc -mconstructor-aliases -O1 -disable-llvm-passes \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,MSVC
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore \
// RUN:   -triple x86_64-linux-gnu -mconstructor-aliases -O1 -disable-llvm-passes \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=IGNORE

bool check(const void *);

struct Base {
  ~Base();
};
Base::~Base() {}

struct Derived : Base {
  ~Derived() pre(check(this)) post(check(this));
};

// CHECK-NOT: @_ZN7DerivedD2Ev = {{.*}}alias
// IGNORE: @_ZN7DerivedD2Ev = {{.*}}alias {{.*}} @_ZN4BaseD2Ev

// ITANIUM-LABEL: define {{.*}} @_ZN7DerivedD2Ev(
// MSVC-LABEL: define {{.*}} @"??1Derived@@QEAA@XZ"(
// CHECK: call {{.*}}check
// CHECK: call {{.*}}Base
// CHECK: call {{.*}}check
// CHECK: ret void
Derived::~Derived() {}
