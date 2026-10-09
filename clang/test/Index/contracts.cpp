int global;
bool check(int);
struct S {
  int m;
  bool ok() const;
};

int f(const int x, const S s)
  pre(x > 0 && check(global))
  post(r : r > x)
  post [[clang::contract_group("g")]] (r2 : r2 > s.m) {
  contract_assert(s.ok());
  return x;
}

auto l = [](int x) pre(x > 0) post(r : r > 0) { return x; };

// RUN: c-index-test -index-file %s -std=c++26 -fcontracts | FileCheck %s
// The function contract specifiers are indexed like the body: the entities
// named in the predicates are referenced (function-local entities like the
// parameters and the result names are not indexed by default).
// CHECK: [indexDeclaration]: kind: function | name: f | USR: c:@F@f#I#$@S@S# | lang: C | cursor: FunctionDecl=f:8:5 (Definition)
// CHECK-NEXT: [indexEntityReference]: kind: struct | name: S | USR: c:@S@S | lang: C++ | cursor: TypeRef=struct S:3:8 | loc: 8:26
// CHECK-NEXT: [indexEntityReference]: kind: function | name: check | USR: c:@F@check#I# | lang: C | cursor: DeclRefExpr=check:2:6 | loc: 9:16
// CHECK-NEXT: [indexEntityReference]: kind: variable | name: global | USR: c:@global | lang: C | cursor: DeclRefExpr=global:1:5 | loc: 9:22
// CHECK-NEXT: [indexEntityReference]: kind: field | name: m | USR: c:@S@S@FI@m | lang: C++ | cursor: MemberRefExpr=m:4:7 {{.*}} | loc: 11:52
// CHECK-NEXT: [indexEntityReference]: kind: c++-instance-method | name: ok | USR: c:@S@S@F@ok#1 | lang: C++ | cursor: MemberRefExpr=ok:5:8 {{.*}} | loc: 12:21
// CHECK-NEXT: [indexDeclaration]: kind: variable | name: l

// RUN: c-index-test -test-load-source all %s -std=c++26 -fcontracts | FileCheck %s --check-prefix=LOAD
// The contract assertions are visited as (unexposed) statements, between the
// declarator and the body, and the result names as their children.
// LOAD: contracts.cpp:8:5: FunctionDecl=f:8:5 (Definition) Extent=[8:1 - 14:2]
// LOAD-NEXT: contracts.cpp:8:17: ParmDecl=x:8:17 (Definition) Extent=[8:7 - 8:18]
// LOAD-NEXT: contracts.cpp:8:28: ParmDecl=s:8:28 (Definition) Extent=[8:20 - 8:29]
// LOAD-NEXT: contracts.cpp:8:26: TypeRef=struct S:3:8 Extent=[8:26 - 8:27]
// LOAD-NEXT: contracts.cpp:9:3: UnexposedStmt= Extent=[9:3 - 9:30]
// LOAD-NEXT: contracts.cpp:9:7: BinaryOperator=&& Extent=[9:7 - 9:29]
// LOAD: contracts.cpp:9:16: DeclRefExpr=check:2:6 Extent=[9:16 - 9:21]
// LOAD: contracts.cpp:9:22: DeclRefExpr=global:1:5 Extent=[9:22 - 9:28]
// LOAD-NEXT: contracts.cpp:10:3: UnexposedStmt= Extent=[10:3 - 10:18]
// LOAD-NEXT: contracts.cpp:10:8: DeclStmt= Extent=[10:8 - 10:9]
// LOAD-NEXT: contracts.cpp:10:8: UnexposedDecl=r:10:8 (Definition) Extent=[10:8 - 10:9]
// LOAD-NEXT: contracts.cpp:10:12: BinaryOperator=> Extent=[10:12 - 10:17]
// LOAD-NEXT: contracts.cpp:10:12: UnexposedExpr=r:10:8 Extent=[10:12 - 10:13]
// LOAD-NEXT: contracts.cpp:10:12: DeclRefExpr=r:10:8 Extent=[10:12 - 10:13]
// LOAD: contracts.cpp:11:3: UnexposedStmt= Extent=[11:3 - 11:54]
// LOAD-NEXT: contracts.cpp:11:40: DeclStmt= Extent=[11:40 - 11:42]
// LOAD-NEXT: contracts.cpp:11:40: UnexposedDecl=r2:11:40 (Definition) Extent=[11:40 - 11:42]
// LOAD: contracts.cpp:11:52: MemberRefExpr=m:4:7 SingleRefName=[11:52 - 11:53] RefName=[11:52 - 11:53] Extent=[11:50 - 11:53]
// LOAD-NEXT: contracts.cpp:11:50: DeclRefExpr=s:8:28 Extent=[11:50 - 11:51]
// LOAD-NEXT: contracts.cpp:11:55: CompoundStmt= Extent=[11:55 - 14:2]
// LOAD-NEXT: contracts.cpp:12:3: UnexposedStmt= Extent=[12:3 - 12:26]
// LOAD-NEXT: contracts.cpp:12:19: CallExpr=ok:5:8 Extent=[12:19 - 12:25]
// LOAD: contracts.cpp:16:10: LambdaExpr= Extent=[16:10 - 16:60]
// LOAD-NEXT: contracts.cpp:16:17: ParmDecl=x:16:17 (Definition) Extent=[16:13 - 16:18]
// LOAD-NEXT: contracts.cpp:16:20: UnexposedStmt= Extent=[16:20 - 16:30]
// LOAD-NEXT: contracts.cpp:16:24: BinaryOperator=> Extent=[16:24 - 16:29]
// LOAD: contracts.cpp:16:31: UnexposedStmt= Extent=[16:31 - 16:46]
// LOAD-NEXT: contracts.cpp:16:36: DeclStmt= Extent=[16:36 - 16:37]
// LOAD-NEXT: contracts.cpp:16:36: UnexposedDecl=r:16:36 (Definition) Extent=[16:36 - 16:37]
// LOAD-NEXT: contracts.cpp:16:40: BinaryOperator=> Extent=[16:40 - 16:45]
// LOAD-NEXT: contracts.cpp:16:40: UnexposedExpr=r:16:36 Extent=[16:40 - 16:41]
// LOAD-NEXT: contracts.cpp:16:40: DeclRefExpr=r:16:36 Extent=[16:40 - 16:41]
// LOAD: contracts.cpp:16:47: CompoundStmt= Extent=[16:47 - 16:60]
