// [class.cdtor]p4: Virtual functions called during the evaluation of a
// precondition assertion of a destructor use the final overrider in the
// destructor's class, so the vtable pointers have to be set before the
// preconditions are evaluated, even if the body of the destructor is empty.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -triple x86_64-linux-gnu -O1 -disable-llvm-passes \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ITANIUM
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=quick_enforce \
// RUN:   -triple x86_64-windows-msvc -O1 -disable-llvm-passes \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,MSVC
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -triple x86_64-linux-gnu -O1 -disable-llvm-passes -fstrict-vtable-pointers \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=STRICT
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore \
// RUN:   -triple x86_64-linux-gnu -O1 -disable-llvm-passes \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=IGNORE

bool check(const void *);
void body();

struct Poly {
  virtual void f();
  ~Poly() pre(check(this));
};

// ITANIUM-LABEL: define {{.*}} @_ZN4PolyD2Ev(
// ITANIUM: store ptr {{.*}}@_ZTV4Poly{{.*}}, ptr %
// MSVC-LABEL: define {{.*}} @"??1Poly@@QEAA@XZ"(
// MSVC: store ptr @"??_7Poly@@6B@", ptr %this
// CHECK-NOT: store ptr {{.*}}Poly
// CHECK: call {{.*}}check
// CHECK-NOT: store ptr {{.*}}Poly
// CHECK: ret void
//
// STRICT-LABEL: define {{.*}} @_ZN4PolyD2Ev(
// STRICT: %[[THIS:.*]] = call ptr @llvm.launder.invariant.group.p0(ptr %this
// STRICT-NEXT: store ptr {{.*}}@_ZTV4Poly{{.*}}, ptr %[[THIS]]
// STRICT-NOT: store ptr {{.*}}@_ZTV4Poly
// STRICT: call {{.*}}check
// STRICT-NOT: store ptr {{.*}}@_ZTV4Poly
// STRICT: ret void
//
// IGNORE-LABEL: define {{.*}} @_ZN4PolyD2Ev(
// IGNORE-NOT: store ptr {{.*}}@_ZTV4Poly
// IGNORE: ret void
Poly::~Poly() {}

// The vtable pointers are only set once.
struct PolyWithBody {
  virtual void f();
  ~PolyWithBody() pre(check(this));
};

// ITANIUM-LABEL: define {{.*}} @_ZN12PolyWithBodyD2Ev(
// ITANIUM: store ptr {{.*}}@_ZTV12PolyWithBody{{.*}}, ptr %
// MSVC-LABEL: define {{.*}} @"??1PolyWithBody@@QEAA@XZ"(
// MSVC: store ptr @"??_7PolyWithBody@@6B@", ptr %this
// CHECK-NOT: store ptr {{.*}}PolyWithBody
// CHECK: call {{.*}}check
// CHECK-NOT: store ptr {{.*}}PolyWithBody
// CHECK: call {{.*}}body
// CHECK-NOT: store ptr {{.*}}PolyWithBody
// CHECK: ret void
PolyWithBody::~PolyWithBody() { body(); }

// The destructor of a member can observe the dynamic type of the enclosing
// object through its contract assertions.
struct Field {
  ~Field() pre(check(this)) {}
};
struct Outer {
  virtual void f();
  Field F;
  ~Outer();
};

// ITANIUM-LABEL: define {{.*}} @_ZN5OuterD2Ev(
// ITANIUM: store ptr {{.*}}@_ZTV5Outer{{.*}}, ptr %
// MSVC-LABEL: define {{.*}} @"??1Outer@@QEAA@XZ"(
// MSVC: store ptr @"??_7Outer@@6B@", ptr %this
// CHECK: call {{.*}}Field
//
// IGNORE-LABEL: define {{.*}} @_ZN5OuterD2Ev(
// IGNORE-NOT: store ptr {{.*}}@_ZTV5Outer
// IGNORE: ret void
Outer::~Outer() {}
