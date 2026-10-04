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
