// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: not %clang_cc1 -std=c++26 -fcontracts -ast-dump %s 2>/dev/null | FileCheck %s

// [dcl.decl.general]p4+a, [class.mem.general]p1+a: a function-contract-
// specifier-seq shall be present only if the declarator declares a function.
// A declarator whose type is a function type named by a typedef-name declares
// a function, even though it has no function declarator chunk: the decision
// is made once the declaration has been built. (These used to be rejected as
// "only allowed on function declarations".)

using F = int(int);
typedef bool T(int);

F f pre(true);
F (g) pre(true);
F h post(r: r > 0);
int i, j() pre(true), (*k)() pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}

// CHECK: FunctionDecl {{.*}} f 'F':'int (int)'
// CHECK-NEXT: ParmVarDecl {{.*}} implicit 'int'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} pre
// CHECK: FunctionDecl {{.*}} g 'F':'int (int)'
// CHECK-NEXT: ParmVarDecl {{.*}} implicit 'int'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} pre
// CHECK: FunctionDecl {{.*}} h 'F':'int (int)'
// CHECK-NEXT: ParmVarDecl {{.*}} implicit 'int'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} post
// CHECK: FunctionDecl {{.*}} j 'int ()'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} pre

struct S {
  T m pre(true);
  T (n) pre(true);
  T o post(r: r);
  static T sm pre(true);
  F *fp pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
  F &fr pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
  int d, e pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
};

// CHECK: CXXRecordDecl {{.*}} struct S
// CHECK: CXXMethodDecl {{.*}} m 'T':'bool (int)'
// CHECK-NEXT: ParmVarDecl {{.*}} implicit 'int'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} pre
// CHECK: CXXMethodDecl {{.*}} n 'T':'bool (int)'
// CHECK-NEXT: ParmVarDecl {{.*}} implicit 'int'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} pre
// CHECK: CXXMethodDecl {{.*}} o 'T':'bool (int)'
// CHECK-NEXT: ParmVarDecl {{.*}} implicit 'int'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} post
// CHECK: CXXMethodDecl {{.*}} sm 'T':'bool (int)' static
// CHECK-NEXT: ParmVarDecl {{.*}} implicit 'int'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} pre

// The contracts are checked like those of any other function: the parameters
// are unnamed, so the predicate cannot refer to them, and the member is a
// member function (whose 'this' is const in the predicate).
struct U {
  int v;
  T p pre(v > 0) post(r: r == (v > 0));
  T q pre(x > 0); // expected-error {{use of undeclared identifier 'x'}}
};
template <class X> struct V {
  X v;
  T p pre(v > X());
};
template struct V<int>;

// The remaining declarators which do not declare a function are still
// rejected, in the parser where possible.
auto (*ptr)() -> int pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
int (*fptr)() pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
int arr[2] pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
int (S::*mptr)(int) pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
int var pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
F *fpvar pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
F &fref pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}} \
                   // expected-error {{declaration of reference variable 'fref' requires an initializer}}
typedef F FT pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
void takes_fn(int bar() pre(true)); // expected-error {{function contract specifiers are not allowed on a parameter declaration}}
void takes_int(int x pre(x > 0)); // expected-error {{function contract specifiers are not allowed on a parameter declaration}}
void takes_typedef(F fn pre(true)); // expected-error {{function contract specifiers are not allowed on a parameter declaration}}
auto lam = [](int x pre(x > 0)) { return x; }; // expected-error {{function contract specifiers are not allowed on a parameter declaration}}
