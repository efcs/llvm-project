// If the contract-violation handler invoked for a function contract assertion
// exits via an exception, the behavior is as if the function body exited via
// that exception ([basic.contract.eval]p17), except that a function-try-block
// does not get to handle it. For constructors and destructors this means that
// ([except.ctor]p3):
//  - if a postcondition assertion of a constructor exits via an exception, the
//    subobjects that the constructor initialized are destroyed, and
//  - if a precondition assertion of a destructor exits via an exception, the
//    subobjects are still destroyed.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,EH
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,EH
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=quick_enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,NOEH
// RUN: %clang_cc1 -std=c++26 -fcontracts \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,NOEH

// Nothing but the contract-violation handler can throw in this test.
bool check() noexcept;
void body() noexcept;

struct Member {
  Member() noexcept;
  ~Member();
};
struct Base {
  Base() noexcept;
  ~Base();
};
struct VBase {
  VBase() noexcept;
  ~VBase();
};

struct S : Base, virtual VBase {
  Member M1;
  Member M2[2];
  int I;
  S() post(check());
  S(int) post(check());
  S(long) post(check());
  ~S() noexcept(false) pre(check());
};

// The base object constructor does not initialize the virtual base.
//
// CHECK-LABEL: define {{.*}} @_ZN1SC2Ev(
// CHECK: call {{.*}} @_Z4bodyv()
// CHECK: call {{.*}} @_Z5checkv()
// NOEH-NOT: landingpad
// EH: landingpad { ptr, i32 }
// EH-NEXT: cleanup
// EH: arraydestroy.body:
// EH: call void @_ZN6MemberD1Ev(
// EH: arraydestroy.done{{.*}}:
// EH: call void @_ZN6MemberD1Ev(
// EH: call void @_ZN4BaseD2Ev(
// EH-NOT: @_ZN5VBaseD2Ev(
// EH: resume
// CHECK: {{^}}}
//
// CHECK-LABEL: define {{.*}} @_ZN1SC1Ev(
// CHECK: call {{.*}} @_Z4bodyv()
// CHECK: call {{.*}} @_Z5checkv()
// NOEH-NOT: landingpad
// EH: landingpad { ptr, i32 }
// EH-NEXT: cleanup
// EH: arraydestroy.body:
// EH: call void @_ZN6MemberD1Ev(
// EH: arraydestroy.done{{.*}}:
// EH: call void @_ZN6MemberD1Ev(
// EH: call void @_ZN4BaseD2Ev(
// EH: call void @_ZN5VBaseD2Ev(
// EH: resume
// CHECK: {{^}}}
S::S() { body(); }

// The handlers of a function-try-block are not considered.
//
// CHECK-LABEL: define {{.*}} @_ZN1SC2Ei(
// CHECK: call {{.*}} @_Z4bodyv()
// CHECK: call {{.*}} @_Z5checkv()
// CHECK-NOT: __cxa_begin_catch
// NOEH-NOT: landingpad
// EH: landingpad { ptr, i32 }
// EH-NEXT: cleanup
// EH: call void @_ZN6MemberD1Ev(
// EH: call void @_ZN4BaseD2Ev(
// EH: resume
// CHECK-NOT: __cxa_begin_catch
// CHECK: {{^}}}
#if __cpp_exceptions
S::S(int) try { body(); } catch (...) { body(); }
#else
S::S(int) { body(); }
#endif

// CHECK-LABEL: define {{.*}} @_ZN1SD2Ev(
// CHECK: call {{.*}} @_Z5checkv()
// NOEH-NOT: landingpad
// EH: landingpad { ptr, i32 }
// EH-NEXT: cleanup
// EH: arraydestroy.body{{.*}}:
// EH: call void @_ZN6MemberD1Ev(
// EH: arraydestroy.done{{.*}}:
// EH: call void @_ZN6MemberD1Ev(
// EH: call void @_ZN4BaseD2Ev(
// EH-NOT: @_ZN5VBaseD2Ev(
// CHECK: {{^}}}
//
// The complete object destructor delegates to the base object destructor, and
// destroys the virtual base if that exits via an exception.
//
// CHECK-LABEL: define {{.*}} @_ZN1SD1Ev(
// CHECK-NOT: _Z5checkv
// CHECK: {{call|invoke}} void @_ZN1SD2Ev(
// CHECK: call void @_ZN5VBaseD2Ev(
// CHECK: {{^}}}
S::~S() noexcept(false) { body(); }

// If a delegating constructor exits via an exception after the target
// constructor has completed, the destructor is called.
//
// CHECK-LABEL: define {{.*}} @_ZN1SC2El(
// CHECK: call {{.*}} @_ZN1SC2Ev(
// CHECK: call {{.*}} @_Z5checkv()
// NOEH-NOT: landingpad
// EH: landingpad { ptr, i32 }
// EH-NEXT: cleanup
// EH: {{call|invoke}} void @_ZN1SD2Ev(
// EH: resume
// CHECK: {{^}}}
//
// CHECK-LABEL: define {{.*}} @_ZN1SC1El(
// CHECK: call {{.*}} @_ZN1SC1Ev(
// CHECK: call {{.*}} @_Z5checkv()
// NOEH-NOT: landingpad
// EH: landingpad { ptr, i32 }
// EH-NEXT: cleanup
// EH: {{call|invoke}} void @_ZN1SD1Ev(
// EH: resume
// CHECK: {{^}}}
S::S(long) : S() {}

struct T : Base, virtual VBase {
  Member M;
  ~T() noexcept(false) pre(check()) post(check());
};

// A complete object destructor which evaluates the contracts itself also
// destroys the virtual bases if a precondition exits via an exception. There
// is nothing left to destroy when the postcondition is evaluated.
//
// CHECK-LABEL: define {{.*}} @_ZN1TD1Ev(
// CHECK-NOT: __cxa_begin_catch
// CHECK: call {{.*}} @_Z5checkv()
// CHECK-NOT: __cxa_begin_catch
// NOEH-NOT: landingpad
// EH: landingpad { ptr, i32 }
// EH-NEXT: cleanup
// EH: call void @_ZN6MemberD1Ev(
// EH: call void @_ZN4BaseD2Ev(
// EH: call void @_ZN5VBaseD2Ev(
// CHECK-NOT: __cxa_begin_catch
// CHECK: {{^}}}
#if __cpp_exceptions
T::~T() noexcept(false) try { body(); } catch (...) { body(); }
#else
T::~T() noexcept(false) { body(); }
#endif

// The members of a union are not destroyed by its destructor, but the member
// initialized by a constructor is destroyed if the constructor exits via an
// exception.
union U {
  Member M;
  int I;
  U() post(check());
  ~U() noexcept(false) pre(check());
};

// CHECK-LABEL: define {{.*}} @_ZN1UC2Ev(
// CHECK: call {{.*}} @_Z5checkv()
// NOEH-NOT: landingpad
// EH: landingpad { ptr, i32 }
// EH-NEXT: cleanup
// EH: call void @_ZN6MemberD1Ev(
// EH: resume
// CHECK: {{^}}}
U::U() : M() {}

// CHECK-LABEL: define {{.*}} @_ZN1UD2Ev(
// CHECK: call {{.*}} @_Z5checkv()
// CHECK-NOT: landingpad
// CHECK-NOT: @_ZN6MemberD1Ev(
// CHECK: {{^}}}
U::~U() noexcept(false) {}
