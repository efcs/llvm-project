// Function contract assertions must be evaluated exactly once per call: in the
// function that runs the user-written body, and never in a structor variant
// that merely delegates to another variant. Postconditions must also be
// evaluated for functions returning void and for constructors and destructors
// regardless of whether the ABI makes them return 'this'.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -mconstructor-aliases -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ALIAS
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,NOALIAS
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple arm64-apple-macosx14 \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,NOALIAS
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-windows-msvc \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=MSVC

// ALIAS-DAG: @_ZN5PlainC1Ei = {{.*}}alias {{.*}} @_ZN5PlainC2Ei
// ALIAS-DAG: @_ZN5PlainD1Ev = {{.*}}alias {{.*}} @_ZN5PlainD2Ev

bool pre_check();
bool post_check();

// CHECK-LABEL: define {{.*}}void @_Z7void_fnv()
// CHECK: {{call|invoke}} {{.*}} @_Z10post_checkv()
// MSVC-LABEL: define {{.*}}void @"?void_fn@@YAXXZ"()
// MSVC: {{call|invoke}} {{.*}} @"?post_check@@YA_NXZ"()
void void_fn() post(post_check()) {}

// CHECK-LABEL: define {{.*}}void @_Z13void_fn_earlyb(
// CHECK: {{call|invoke}} {{.*}} @_Z10post_checkv()
// CHECK-NOT: @_Z10post_checkv()
// CHECK: ret void
void void_fn_early(bool B) post(post_check()) {
  if (B)
    return;
  pre_check();
}

struct Plain {
  Plain(int) pre(pre_check()) post(post_check());
  ~Plain() pre(pre_check()) post(post_check());
};

// CHECK-LABEL: define {{.*}} @_ZN5PlainC2Ei(
// CHECK: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// CHECK: {{call|invoke}} {{.*}} @_Z10post_checkv()
// CHECK: ret
//
// NOALIAS-LABEL: define {{.*}} @_ZN5PlainC1Ei(
// NOALIAS-NOT: _check
// NOALIAS: {{call|invoke}} {{.*}} @_ZN5PlainC2Ei(
// NOALIAS-NOT: _check
// NOALIAS: ret
//
// MSVC-LABEL: define {{.*}} @"??0Plain@@QEAA@H@Z"(
// MSVC: {{call|invoke}} {{.*}} @"?pre_check@@YA_NXZ"()
// MSVC: {{call|invoke}} {{.*}} @"?post_check@@YA_NXZ"()
// MSVC: ret
Plain::Plain(int) {}

// CHECK-LABEL: define {{.*}} @_ZN5PlainD2Ev(
// CHECK: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// CHECK: {{call|invoke}} {{.*}} @_Z10post_checkv()
// CHECK: ret
//
// NOALIAS-LABEL: define {{.*}} @_ZN5PlainD1Ev(
// NOALIAS-NOT: _check
// NOALIAS: {{call|invoke}} {{.*}} @_ZN5PlainD2Ev(
// NOALIAS-NOT: _check
// NOALIAS: ret
//
// MSVC-LABEL: define {{.*}} @"??1Plain@@QEAA@XZ"(
// MSVC: {{call|invoke}} {{.*}} @"?pre_check@@YA_NXZ"()
// MSVC: {{call|invoke}} {{.*}} @"?post_check@@YA_NXZ"()
// MSVC: ret
Plain::~Plain() {}

// VBase is deliberately not polymorphic: a virtual destructor would make
// ~WithVBase virtual, and virtual functions cannot have contracts.
struct VBase {
  VBase();
  ~VBase();
};
struct WithVBase : virtual VBase {
  WithVBase(int) pre(pre_check()) post(post_check());
  ~WithVBase() pre(pre_check()) post(post_check());
};

// With a virtual base, both the base and complete object constructors run the
// user-written body, so both evaluate the contracts.
//
// CHECK-LABEL: define {{.*}} @_ZN9WithVBaseC2Ei(
// CHECK: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// CHECK: {{call|invoke}} {{.*}} @_Z10post_checkv()
// CHECK: ret
//
// CHECK-LABEL: define {{.*}} @_ZN9WithVBaseC1Ei(
// CHECK: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// CHECK: {{call|invoke}} {{.*}} @_ZN5VBaseC2Ev(
// CHECK: {{call|invoke}} {{.*}} @_Z10post_checkv()
// CHECK: ret
//
// MSVC-LABEL: define {{.*}} @"??0WithVBase@@QEAA@H@Z"(
// MSVC: {{call|invoke}} {{.*}} @"?pre_check@@YA_NXZ"()
// MSVC: {{call|invoke}} {{.*}} @"?post_check@@YA_NXZ"()
// MSVC: ret
WithVBase::WithVBase(int) {}

// The complete destructor delegates to the base destructor.
//
// CHECK-LABEL: define {{.*}} @_ZN9WithVBaseD2Ev(
// CHECK: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// CHECK: {{call|invoke}} {{.*}} @_Z10post_checkv()
// CHECK: ret
//
// CHECK-LABEL: define {{.*}} @_ZN9WithVBaseD1Ev(
// CHECK-NOT: _check
// CHECK: {{call|invoke}} {{.*}} @_ZN9WithVBaseD2Ev(
// CHECK-NOT: _check
// CHECK: ret
//
// MSVC-LABEL: define {{.*}} @"??1WithVBase@@QEAA@XZ"(
// MSVC: {{call|invoke}} {{.*}} @"?pre_check@@YA_NXZ"()
// MSVC: {{call|invoke}} {{.*}} @"?post_check@@YA_NXZ"()
// MSVC: ret
//
// MSVC-LABEL: define {{.*}} @"??_DWithVBase@@QEAAXXZ"(
// MSVC-NOT: _check
// MSVC: ret
WithVBase::~WithVBase() {}

// Destroying a complete object requires the complete (Itanium D1) / vbase
// (MSVC ??_D) destructor to be emitted.
void use_withvbase() { WithVBase W(1); }

// An inheriting constructor calls the inherited constructor, which evaluates
// the contracts.
struct Inheriting : Plain {
  using Plain::Plain;
};
// CHECK-LABEL: define {{.*}} @_Z14use_inheritingv()
void use_inheriting() { Inheriting I(1); }
// CHECK-LABEL: define {{.*}} @_ZN10InheritingCI25PlainEi(
// CHECK-NOT: _check
// CHECK: {{call|invoke}} {{.*}} @_ZN5PlainC2Ei(
// CHECK-NOT: _check
// CHECK: ret
