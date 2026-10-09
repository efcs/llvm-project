// Test that function contracts (pre, post, contract_assert) survive
// serialization through C++20 named modules.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: split-file %s %t
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   %t/M.cppm -emit-module-interface -o %t/M.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fprebuilt-module-path=%t %t/Use.cpp -fsyntax-only -verify
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fprebuilt-module-path=%t %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp --check-prefix=IR
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fprebuilt-module-path=%t %t/Dump.cpp -fsyntax-only -ast-dump-all \
// RUN:   -fcontract-group-evaluation-semantic=grp=ignore \
// RUN:   -ast-dump-filter=ns | FileCheck %t/Dump.cpp
//
// Test again with reduced BMI.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   %t/M.cppm -emit-reduced-module-interface -o %t/M.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fprebuilt-module-path=%t %t/Use.cpp -fsyntax-only -verify
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fprebuilt-module-path=%t %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp --check-prefix=IR

//--- M.cppm
export module M;

export namespace ns {
inline int pre_post(const int x) pre(x > 0) post(r : r == x) {
  contract_assert(x != 3);
  return x;
}

int decl_only(const int x) pre(x > 0) post(r : r > x);

// Multiple result names; the second refers to the first as canonical.
inline int two_results(const int x) post(a : a >= x) post(b : b <= x + 1) {
  return x;
}

constexpr int ce(int x) pre(x > 0) { return x; }

struct S {
  int v = 1;
  int m(int y) pre(y > 0) { return y + v; }
  int get() const post(r : r == v) { return v; }
  S(int y) pre(y > 0) : v(y) {}
};

template <class T> T tmpl(const T x) pre(x > 0) post(r : r == x) {
  return x;
}

// Lambdas inside contracts, including one capturing a variable across the
// contract boundary.
inline int lambda_in_contract(int x) pre([](int y) { return y > 0; }(x)) {
  int z = x;
  contract_assert([&] { return z > 0; }());
  return z;
}

// A lambda with contracts of its own.
inline int lambda_with_contracts(int x) {
  auto l = [](const int y) pre(y > 0) post(r : r == y) { return y; };
  return l(x);
}

inline int with_attr(int x) pre [[clang::contract_group("grp")]] (x > 0) {
  return x;
}
} // namespace ns

//--- Use.cpp
// expected-no-diagnostics
import M;

static_assert(ns::ce(1) == 1);

int use() {
  ns::S s(1);
  return ns::pre_post(1) + ns::decl_only(2) + ns::two_results(3) + s.m(4) +
         s.get() + ns::tmpl(5) + ns::tmpl(6.0) + ns::lambda_in_contract(7) +
         ns::lambda_with_contracts(8) + ns::with_attr(9);
}

// The contracts of the imported inline functions are checked.
// IR-LABEL: define {{.*}}@_ZN2nsW1M8pre_postEi(
// IR: call {{.*}}contract_violation
// IR-LABEL: define {{.*}}@_ZN2nsW1M4tmplIiEET_S2_(
// IR: call {{.*}}contract_violation

//--- Dump.cpp
import M;

// CHECK-LABEL: FunctionDecl {{.*}} pre_post 'int (const int)'
// CHECK: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} pre
// CHECK: ContractStmt {{.*}} post
// CHECK-NEXT: ResultNameDecl {{.*}} r 'int'
// CHECK: ContractStmt {{.*}} contract_assert

// CHECK-LABEL: FunctionDecl {{.*}} two_results 'int (const int)'
// CHECK: ResultNameDecl {{.*}} a 'int'
// CHECK: ResultNameDecl {{.*}} b 'int'

// The contract_group attribute survives serialization.
// CHECK-LABEL: FunctionDecl {{.*}} with_attr 'int (int)'
// CHECK: ContractStmt {{.*}} pre ignore
