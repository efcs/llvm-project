// The postcondition assertions of a destructor are evaluated after all of the
// subobjects of the object being destroyed have been destroyed. For a complete
// object this includes the virtual bases, which are destroyed by the complete
// object destructor rather than the base object destructor. The complete
// object destructor therefore cannot delegate to the base object destructor
// when there are postconditions to evaluate; it runs the destructor body
// itself, so that each assertion is evaluated exactly once per destructor
// call.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -mconstructor-aliases -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ITANIUM
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ITANIUM
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple arm64-apple-macosx14 \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ITANIUM
// RUN: %clang_cc1 -std=c++26 -fcontracts \
// RUN:   -fcontract-evaluation-semantic=quick_enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ITANIUM
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-windows-msvc \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=MSVC
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple i686-windows-msvc \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=MSVC
//
// When the contracts are ignored, the complete object destructor delegates as
// usual.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=ignore -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=IGNORE

bool pre_check();
bool post_check();

struct VBase {
  VBase();
  ~VBase();
};
struct Member {
  Member();
  ~Member();
};

struct WithVBase : virtual VBase {
  Member M;
  ~WithVBase() pre(pre_check()) post(post_check());
};

// The base object destructor does not destroy the virtual base.
//
// ITANIUM-LABEL: define {{.*}} @_ZN9WithVBaseD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @_ZN6MemberD1Ev(
// ITANIUM-NOT: @_ZN5VBaseD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z10post_checkv()
// ITANIUM-NOT: _check
// ITANIUM: ret {{void|ptr}}
//
// The complete object destructor evaluates the postcondition after destroying
// the virtual base, and does not call the base object destructor.
//
// ITANIUM-LABEL: define {{.*}} @_ZN9WithVBaseD1Ev(
// ITANIUM-NOT: @_ZN9WithVBaseD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @_ZN6MemberD1Ev(
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @_ZN5VBaseD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z10post_checkv()
// ITANIUM-NOT: _check
// ITANIUM-NOT: @_ZN9WithVBaseD2Ev(
// ITANIUM: ret {{void|ptr}}
//
// MSVC-LABEL: define {{.*}} @"??1WithVBase@@{{[^"]*}}"(
// MSVC: {{call|invoke}} {{.*}} @"?pre_check@@{{[^"]*}}"()
// MSVC-NOT: _check
// MSVC: call {{.*}} @"??1Member@@{{[^"]*}}"(
// MSVC-NOT: @"??1VBase@@
// MSVC: {{call|invoke}} {{.*}} @"?post_check@@{{[^"]*}}"()
// MSVC-NOT: _check
// MSVC: ret void
//
// IGNORE-LABEL: define {{.*}} @_ZN9WithVBaseD2Ev(
// IGNORE-NOT: _check
// IGNORE: ret void
// IGNORE-LABEL: define {{.*}} @_ZN9WithVBaseD1Ev(
// IGNORE-NOT: _check
// IGNORE: call void @_ZN9WithVBaseD2Ev(
// IGNORE-NEXT: call void @_ZN5VBaseD2Ev(
// IGNORE-NOT: _check
// IGNORE: ret void
WithVBase::~WithVBase() {}

// Without postconditions the complete object destructor can delegate to the
// base object destructor, which evaluates the preconditions.
struct OnlyPre : virtual VBase {
  ~OnlyPre() pre(pre_check());
};

// ITANIUM-LABEL: define {{.*}} @_ZN7OnlyPreD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// ITANIUM: ret {{void|ptr}}
//
// ITANIUM-LABEL: define {{.*}} @_ZN7OnlyPreD1Ev(
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @_ZN7OnlyPreD2Ev(
// ITANIUM-NEXT: call {{.*}} @_ZN5VBaseD2Ev(
// ITANIUM-NOT: _check
// ITANIUM: ret {{void|ptr}}
OnlyPre::~OnlyPre() {}

// The complete object destructor of an abstract class is never called.
struct Abstract : virtual VBase {
  virtual void f() = 0;
  ~Abstract() pre(pre_check()) post(post_check());
};

// ITANIUM-LABEL: define {{.*}} @_ZN8AbstractD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// ITANIUM: {{call|invoke}} {{.*}} @_Z10post_checkv()
// ITANIUM: ret {{void|ptr}}
//
// ITANIUM-LABEL: define {{.*}} @_ZN8AbstractD1Ev(
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @llvm.trap()
// ITANIUM-NOT: _check
// ITANIUM: {{^}}}
Abstract::~Abstract() {}

// A class derived from a class with a virtual base destroys the base class
// subobject with the base object destructor, which evaluates the contracts of
// the base class destructor after the non-virtual subobjects of the base class
// have been destroyed. The contracts of the derived class destructor are
// evaluated after the virtual base has been destroyed.
struct Derived : WithVBase {
  Member M2;
  ~Derived() pre(pre_check()) post(post_check());
};

// ITANIUM-LABEL: define {{.*}} @_ZN7DerivedD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @_ZN6MemberD1Ev(
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @_ZN9WithVBaseD2Ev(
// ITANIUM-NOT: @_ZN5VBaseD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z10post_checkv()
// ITANIUM-NOT: _check
// ITANIUM: ret {{void|ptr}}
//
// ITANIUM-LABEL: define {{.*}} @_ZN7DerivedD1Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z9pre_checkv()
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @_ZN6MemberD1Ev(
// ITANIUM-NOT: _check
// ITANIUM: call {{.*}} @_ZN9WithVBaseD2Ev(
// ITANIUM-NEXT: call {{.*}} @_ZN5VBaseD2Ev(
// ITANIUM: {{call|invoke}} {{.*}} @_Z10post_checkv()
// ITANIUM-NOT: _check
// ITANIUM: ret {{void|ptr}}
Derived::~Derived() {}

// Destroying complete objects uses the complete object destructor (the vbase
// destructor in the Microsoft ABI), including for delete expressions.
//
// CHECK-LABEL: define {{.*}} @_Z3useP9WithVBase(
// CHECK: call {{.*}} @_ZN9WithVBaseD1Ev(
// CHECK: call {{.*}} @_ZdlPvm(
// CHECK: call {{.*}} @_ZN9WithVBaseD1Ev(
//
// MSVC-LABEL: define {{.*}} @"?use@@{{[^"]*}}"(
// MSVC: call {{.*}} @"??_DWithVBase@@{{[^"]*}}"(
// MSVC: call {{.*}} @"??3@{{[^"]*}}"(
// MSVC: call {{.*}} @"??_DWithVBase@@{{[^"]*}}"(
void use(WithVBase *P) {
  WithVBase W;
  delete P;
}

// In the Microsoft ABI the vbase destructor is emitted where it is used.
// When the destructor is defined in the translation unit it evaluates the
// contracts itself, after destroying the virtual base.
//
// MSVC-LABEL: define linkonce_odr {{.*}} @"??_DWithVBase@@{{[^"]*}}"(
// MSVC-NOT: @"??1WithVBase@@
// MSVC: {{call|invoke}} {{.*}} @"?pre_check@@{{[^"]*}}"()
// MSVC-NOT: _check
// MSVC: call {{.*}} @"??1Member@@{{[^"]*}}"(
// MSVC-NOT: _check
// MSVC: call {{.*}} @"??1VBase@@{{[^"]*}}"(
// MSVC: {{call|invoke}} {{.*}} @"?post_check@@{{[^"]*}}"()
// MSVC-NOT: _check
// MSVC-NOT: @"??1WithVBase@@
// MSVC: ret void

// If the destructor is not defined in the translation unit, the vbase
// destructor has to delegate to the base destructor, which evaluates the
// contracts.
struct NotDefined : virtual VBase {
  ~NotDefined() pre(pre_check()) post(post_check());
};
void use_not_defined() { NotDefined N; }

// MSVC-LABEL: define linkonce_odr {{.*}} @"??_DNotDefined@@{{[^"]*}}"(
// MSVC-NOT: _check
// MSVC: call {{.*}} @"??1NotDefined@@{{[^"]*}}"(
// MSVC-NOT: _check
// MSVC: call {{.*}} @"??1VBase@@{{[^"]*}}"(
// MSVC-NOT: _check
// MSVC: ret void
