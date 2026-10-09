// RUN: %clang_cc1 -std=c++26 -fcontracts -ast-dump %s | FileCheck %s
//
// The function contract specifiers of a function belong to its first
// declaration. A declaration which omits them refers to them and has no copy;
// a declaration which repeats them keeps its own copy, which is not the one in
// effect. The contracts of a template specialization are instantiated from
// its pattern when they are needed.

// CHECK: FunctionDecl [[F1:0x[0-9a-f]+]] {{.*}} f 'int (const int)'
// CHECK-NEXT: ParmVarDecl {{.*}} x 'const int'
// CHECK-NEXT: ContractSpecifierDecl [[CSD:0x[0-9a-f]+]]
// CHECK-NEXT: ContractStmt {{.*}} pre enforce "x > 0"
// CHECK: ContractStmt {{.*}} post enforce "r : r > x"
int f(const int x) pre(x > 0) post(r : r > x);

// The definition omits the contracts: it refers to those of the first
// declaration and has no ContractSpecifierDecl of its own.
// CHECK: FunctionDecl {{.*}} prev [[F1]] {{.*}} f 'int (const int)' contracts [[CSD]]
// CHECK-NEXT: ParmVarDecl {{.*}} y 'const int'
// CHECK-NEXT: CompoundStmt
int f(const int y) { return y + 1; }

// A redeclaration which repeats the contracts has its own copy, as written.
// CHECK: FunctionDecl [[G1:0x[0-9a-f]+]] {{.*}} g 'int (const int)'
// CHECK-NEXT: ParmVarDecl {{.*}} x 'const int'
// CHECK-NEXT: ContractSpecifierDecl [[GCSD:0x[0-9a-f]+]]
// CHECK-NEXT: ContractStmt {{.*}} pre enforce "x > 0"
int g(const int x) pre(x > 0);
// CHECK: FunctionDecl {{.*}} prev [[G1]] {{.*}} g 'int (const int)'
// CHECK-NOT: contracts {{0x}}
// CHECK-NEXT: ParmVarDecl {{.*}} y 'const int'
// CHECK-NEXT: ContractSpecifierDecl {{0x[0-9a-f]+}}
// CHECK-NOT: ContractSpecifierDecl [[GCSD]]
// CHECK-NEXT: ContractStmt {{.*}} pre enforce "y > 0"
int g(const int y) pre(y > 0) { return y; }

// The contracts of a function template specialization are instantiated when
// the specialization is odr-used, from the first declaration of the template.
// CHECK: FunctionTemplateDecl {{.*}} t
// CHECK: ContractStmt {{.*}} pre enforce "x > T{}"
template <class T> T t(const T x) pre(x > T{});
template <class T> T t(const T x) { return x; }

// Only referenced in an unevaluated operand: not instantiated.
// CHECK: FunctionDecl {{.*}} t 'int (const int)' implicit_instantiation{{.*}} contracts-uninstantiated
// CHECK-NEXT: TemplateArgument type 'int'
// CHECK: ParmVarDecl {{.*}} x 'const int'
// CHECK-NOT: ContractSpecifierDecl
// CHECK: FunctionDecl {{.*}} t 'long (const long)' implicit_instantiation
// CHECK-NOT: contracts-uninstantiated
// CHECK: ParmVarDecl {{.*}} x 'const long'
// CHECK-NEXT: ContractSpecifierDecl
// CHECK-NEXT: ContractStmt {{.*}} pre enforce "x > T{}"
using T1 = decltype(t(1));
long use_t = t(1L);
