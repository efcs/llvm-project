// A function which is explicitly defaulted after its first declaration may
// have function contract specifiers ([dcl.contract.func]p6). The contracts
// are evaluated by the implicitly defined function, so the function has to be
// called even where it could otherwise be replaced by a memcpy.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce \
// RUN:   -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,CHECKED
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore \
// RUN:   -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,IGNORE

bool pre_check();
bool post_check();

struct Member {
  Member();
  Member(const Member &);
  Member &operator=(const Member &);
  ~Member();
  bool operator==(const Member &) const;
};

struct S {
  Member M;
  S() pre(pre_check()) post(post_check());
  S(const S &) pre(pre_check()) post(post_check());
  S &operator=(const S &) pre(pre_check()) post(post_check());
  ~S() pre(pre_check()) post(post_check());
  bool operator==(const S &) const pre(pre_check()) post(post_check());
};

// CHECK-LABEL: define {{.*}} @_ZN1SC2Ev(
// CHECKED: call {{.*}} @_Z9pre_checkv()
// CHECK: call void @_ZN6MemberC1Ev(
// CHECKED: call {{.*}} @_Z10post_checkv()
// CHECK: ret void
S::S() = default;

// CHECK-LABEL: define {{.*}} @_ZN1SC2ERKS_(
// CHECKED: call {{.*}} @_Z9pre_checkv()
// CHECK: call void @_ZN6MemberC1ERKS_(
// CHECKED: call {{.*}} @_Z10post_checkv()
// CHECK: ret void
S::S(const S &) = default;

// CHECK-LABEL: define {{.*}} @_ZN1SaSERKS_(
// CHECKED: call {{.*}} @_Z9pre_checkv()
// CHECK: call {{.*}} @_ZN6MemberaSERKS_(
// CHECKED: call {{.*}} @_Z10post_checkv()
// CHECK: ret ptr
S &S::operator=(const S &) = default;

// CHECK-LABEL: define {{.*}} @_ZN1SD2Ev(
// CHECKED: call {{.*}} @_Z9pre_checkv()
// CHECK: call void @_ZN6MemberD1Ev(
// CHECKED: call {{.*}} @_Z10post_checkv()
// CHECK: ret void
S::~S() = default;

// CHECK-LABEL: define {{.*}} @_ZNK1SeqERKS_(
// CHECKED: call {{.*}} @_Z9pre_checkv()
// CHECK: call {{.*}} @_ZNK6MembereqERKS_(
// CHECKED: call {{.*}} @_Z10post_checkv()
// CHECK: ret i1
bool S::operator==(const S &) const = default;

// The implicitly-defined copy operations of a union copy the object
// representation. The AST does not model that copy; it is usually performed
// by the caller instead of calling the function.
union U {
  int I;
  float F;
  U(const U &) pre(pre_check()) post(post_check());
  U &operator=(const U &) pre(pre_check()) post(post_check());
  ~U() pre(pre_check()) post(post_check());
};

// CHECK-LABEL: define {{.*}} @_ZN1UC2ERKS_(
// CHECKED: call {{.*}} @_Z9pre_checkv()
// CHECK: call void @llvm.memcpy.p0.p0.i64(ptr align 4 %this1, ptr align 4 %{{.*}}, i64 4, i1 false)
// CHECKED: call {{.*}} @_Z10post_checkv()
// CHECK: ret void
//
// CHECK-LABEL: define {{.*}} @_ZN1UC1ERKS_(
// CHECKED-NOT: _check
// CHECKED: call void @_ZN1UC2ERKS_(
// CHECKED-NOT: _check
// IGNORE: call void @llvm.memcpy
// CHECK: ret void
U::U(const U &) = default;

// CHECK-LABEL: define {{.*}} @_ZN1UaSERKS_(
// CHECKED: call {{.*}} @_Z9pre_checkv()
// CHECK: call void @llvm.memcpy.p0.p0.i64(ptr align 4 %this1, ptr align 4 %{{.*}}, i64 4, i1 false)
// CHECKED: call {{.*}} @_Z10post_checkv()
// CHECK: ret ptr %this1
U &U::operator=(const U &) = default;

// CHECK-LABEL: define {{.*}} @_ZN1UD2Ev(
// CHECKED: call {{.*}} @_Z9pre_checkv()
// CHECKED: call {{.*}} @_Z10post_checkv()
// CHECK: ret void
U::~U() = default;

// CHECK-LABEL: define {{.*}} @_Z8use_copyR1U(
// CHECKED: call void @_ZN1UC1ERKS_(
// IGNORE: call void @llvm.memcpy
// CHECK: call void @_ZN1UD1Ev(
void use_copy(U &A) { U B(A); }

// CHECK-LABEL: define {{.*}} @_Z10use_assignR1US0_(
// CHECKED: call {{.*}} @_ZN1UaSERKS_(
// IGNORE: call void @llvm.memcpy
// CHECK: ret void
void use_assign(U &A, U &B) { A = B; }

// CHECK-LABEL: define {{.*}} @_Z11use_destroyR1U(
// CHECKED: call void @_ZN1UD1Ev(
// IGNORE-NOT: call
// CHECK: ret void
void use_destroy(U &A) { A.~U(); }
