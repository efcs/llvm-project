// When parameters are destroyed by the callee (the Microsoft ABI, or
// [[clang::trivial_abi]] types), the postconditions must be evaluated before
// the parameters are destroyed.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-windows-msvc \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=MSVC
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=ITANIUM

struct P {
  int v;
  P(int);
  P(const P &);
  ~P();
};
struct [[clang::trivial_abi]] TA {
  int v;
  TA(int);
  TA(const TA &);
  ~TA();
};
bool check(const P &);
bool check(const TA &);

// MSVC-LABEL: define {{.*}} @"?f@@YAHUP@@H@Z"(
// MSVC: contract.post:
// MSVC: invoke {{.*}} @"?check@@YA_NAEBUP@@@Z"(ptr {{.*}} %p)
// MSVC: contract.end:
// MSVC-NEXT: call void @"??1P@@QEAA@XZ"(ptr {{.*}} %p)
// MSVC: ret i32
int f(const P p, int x) post(r : check(p)) {
  if (x)
    return 1;
  return 2;
}

// MSVC-LABEL: define {{.*}} @"?g@@YAXUTA@@@Z"(
// MSVC: invoke {{.*}} @"?check@@YA_NAEBUTA@@@Z"(ptr {{.*}} %t)
// MSVC: contract.end:
// MSVC-NEXT: call void @"??1TA@@QEAA@XZ"(ptr {{.*}} %t)
// MSVC: ret void
//
// ITANIUM-LABEL: define {{.*}} @_Z1g2TA(
// ITANIUM: invoke {{.*}} @_Z5checkRK2TA(ptr {{.*}} %t)
// ITANIUM: contract.end:
// ITANIUM-NEXT: call void @_ZN2TAD1Ev(ptr {{.*}} %t)
// ITANIUM: ret void
void g(const TA t) post(check(t)) {}

// The variables declared in the outermost block of the function body are
// destroyed before the postconditions are evaluated, and exactly once.
struct Local {
  Local();
  ~Local();
};

// ITANIUM-LABEL: define {{.*}} @_Z1h2TAi(
// ITANIUM: cleanup:
// ITANIUM-NEXT: call void @_ZN5LocalD1Ev(
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: invoke {{.*}} @_Z5checkRK2TA(ptr {{.*}} %t)
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: contract.end:
// ITANIUM-NEXT: call void @_ZN2TAD1Ev(ptr {{.*}} %t)
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: ret i32
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: {{^}}}
//
// MSVC-LABEL: define {{.*}} @"?h@@YAHUTA@@H@Z"(
// MSVC: cleanup:
// MSVC-NEXT: call void @"??1Local@@QEAA@XZ"(
// MSVC: invoke {{.*}} @"?check@@YA_NAEBUTA@@@Z"(ptr {{.*}} %t)
// MSVC: contract.end:
// MSVC-NEXT: call void @"??1TA@@QEAA@XZ"(ptr {{.*}} %t)
// MSVC: ret i32
int h(const TA t, int x) post(r : check(t)) {
  Local L;
  if (x)
    return 1;
  return 2;
}

// ITANIUM-LABEL: define {{.*}} @_Z1i2TA(
// ITANIUM: call void @_ZN5LocalD1Ev(
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: invoke {{.*}} @_Z5checkRK2TA(ptr {{.*}} %t)
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: contract.end:
// ITANIUM-NEXT: call void @_ZN2TAD1Ev(ptr {{.*}} %t)
// ITANIUM-NEXT: ret void
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: {{^}}}
void i(const TA t) post(check(t)) { Local L; }

// A return statement nested in other scopes with cleanups.
//
// ITANIUM-LABEL: define {{.*}} @_Z1j2TAi(
// ITANIUM: call void @_ZN5LocalD1Ev(ptr {{.*}} %L3)
// ITANIUM: call void @_ZN5LocalD1Ev(ptr {{.*}} %L1)
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: invoke {{.*}} @_Z5checkRK2TA(ptr {{.*}} %t)
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: contract.end:
// ITANIUM-NEXT: call void @_ZN2TAD1Ev(ptr {{.*}} %t)
// ITANIUM-NEXT: ret void
// ITANIUM-NOT: @_ZN5LocalD1Ev(
// ITANIUM: {{^}}}
void j(const TA t, int x) post(check(t)) {
  Local L1;
  if (x) {
    Local L2;
    if (x > 1)
      return;
  }
  Local L3;
}
