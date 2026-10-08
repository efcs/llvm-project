// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -ast-dump %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -emit-llvm -o - %s | FileCheck --check-prefix=CG %s
// expected-no-diagnostics

// An immediate invocation as the whole predicate of a contract assertion is
// already a full-expression, so Sema does not finish the condition again; the
// cleanups of the predicate (here for the closure object of the lambda) must
// nevertheless be attached to the predicate, not to a statement expression
// wrapped around the contract assertion (which was then mistaken for the
// contract assertion: crashes when dumping the AST, when comparing the
// contracts of a redeclaration, and when rebuilding them for a definition).

int f(int x) pre([]() consteval { return true; }());
int f(int x) pre([]() consteval { return true; }());
int f(int x) { return x; }

// CHECK: FunctionDecl {{.*}} f 'int (int)'
// CHECK: ContractSpecifierDecl {{.*}} <col:14, col:51> col:14
// CHECK-NEXT: ContractStmt {{.*}} <col:14, col:51> pre
// CHECK-NEXT: ExprWithCleanups {{.*}} 'bool'
// CHECK-NEXT: ConstantExpr {{.*}} 'bool'
// CHECK-NEXT: value: Int 1

struct S {
  int m(int x) pre([]() consteval { return true; }());
  int n(int x) post(r: []() consteval { return true; }());
};
int S::m(int x) { return x; }
int S::n(int x) { return x; }

consteval bool ce() { return true; }
int g(int x) pre(ce()) post(r: ce()) { contract_assert(ce()); return x; }

void h(int x) {
  contract_assert([]() consteval { return true; }());
  contract_assert(x >= 0 && []() consteval { return true; }());
}

// The predicate is still evaluated as a contract assertion.
// CG-LABEL: define {{.*}} @_Z1fi(
// CG: call {{.*}}contract_violation
// CG-LABEL: define {{.*}} @_Z1hi(
// CG: call {{.*}}contract_violation
