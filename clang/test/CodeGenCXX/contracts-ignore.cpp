// With the ignore evaluation semantic a contract assertion has no effect
// ([basic.contract.eval]p4): nothing at all is emitted for it.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore \
// RUN:   -fcxx-exceptions -fexceptions -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore \
// RUN:   -fcxx-exceptions -fexceptions -triple x86_64-windows-msvc -emit-llvm -o - %s | FileCheck %s

struct S {
  S();
  S(const S &);
  ~S();
  bool ok() const;
};
bool check(const S &);

// CHECK-NOT: contract.
// CHECK-NOT: handle_contract
// CHECK-NOT: @llvm.trap
// CHECK-NOT: landingpad
// CHECK-NOT: catchpad

// CHECK-LABEL: define {{.*}}f
// CHECK-NOT: {{call|invoke}}
// CHECK: ret void
void f(const int x) pre(check(S())) post(x > 0) { contract_assert(S().ok()); }

// CHECK-LABEL: define {{.*}}g
// CHECK: call {{.*}}S
// CHECK-NOT: {{call|invoke}}
// CHECK: ret void
S g(const int x) pre(check(S())) post(r : r.ok() && check(S())) {
  contract_assert(S().ok());
  return S();
}

// CHECK-NOT: contract.
// CHECK-NOT: handle_contract
// CHECK-NOT: @llvm.trap
// CHECK-NOT: landingpad
// CHECK-NOT: catchpad
