// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s

// Lambdas in the contracts of non-member functions are local to the function:
// they capture its parameters, and in an inline function they get a mangling
// with linkage (not an internal '$_N' name).

// CHECK-LABEL: define {{.*}} @_Z2f1i(
// CHECK: call {{.*}} @"_ZZ2f1iENK3$_0clEv"(ptr {{.*}} %ref.tmp)
// CHECK-LABEL: define internal {{.*}} @"_ZZ2f1iENK3$_0clEv"(ptr {{.*}} dereferenceable(4) %this)
int f1(const int x) pre([x] { return x > 0; }()) { return x; }

// CHECK-LABEL: define internal {{.*}} @"_ZZ2f2iENK3$_0clEv"(ptr {{.*}} dereferenceable(8) %this)
int f2(const int x) pre([&] { return x > 0; }()) { return x; }

// CHECK-LABEL: define linkonce_odr {{.*}} @_Z2f3i(
// CHECK: call {{.*}} @_ZZ2f3iENKUlvE_clEv(
inline int f3(int x) pre([] { return true; }()) { return x; }

// CHECK-LABEL: define linkonce_odr {{.*}} @_ZZ1tIiEiT_ENKUliE_clEi(
// CHECK: br i1 {{.*}}, label %contract.end, label %contract.violation.handler
template <class T> int t(T v) {
  auto l = [](T x) pre(x > 0) { return x; };
  return l(v);
}

int use() { return f1(1) + f2(1) + f3(1) + t(1); }
