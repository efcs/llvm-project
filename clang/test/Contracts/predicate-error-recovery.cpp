// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -fno-recovery-ast %s
// RUN: not %clang_cc1 -std=c++26 -fcontracts -ast-dump %s 2>/dev/null | FileCheck %s

// A predicate which contains errors is kept in the AST as a recovery
// expression, but the contract assertion is invalid: in particular, it is not
// compared with the contracts of another declaration of the function (the
// comparison used to assert on the recovery expression), and dumping it must
// not crash either (the lambda-expression's cleanups used to be attached to a
// statement expression wrapped around the contract assertion, which was then
// treated as the contract assertion).

int h(int x) pre([x]{ return return x; }()); // expected-error {{expected expression}} \
                                             // expected-error {{value of type 'void' is not contextually convertible to 'bool'}}
int h(int x) pre([x]{ return x; }());

// CHECK: FunctionDecl {{.*}} h 'int (int)'
// CHECK: ContractSpecifierDecl {{.*}} <col:14, col:43> col:14 invalid
// CHECK-NEXT: ContractStmt {{.*}} <col:14, col:43> pre
// CHECK-NEXT: ExprWithCleanups {{.*}} 'bool' contains-errors
// CHECK-NEXT: RecoveryExpr {{.*}} 'bool' contains-errors

int h2(int x) pre(x > ); // expected-error {{expected expression}}
int h2(int x) pre(x > 0);

int h3(int x) pre(x > 0);
int h3(int x) pre(x > ); // expected-error {{expected expression}}

// With -fno-recovery-ast there is no predicate at all; the contract assertion
// is dropped.
void g(int x) {
  contract_assert(x > ); // expected-error {{expected expression}}
  contract_assert([x]{ return return x; }()); // expected-error {{expected expression}} \
                                              // expected-error {{value of type 'void' is not contextually convertible to 'bool'}}
}
