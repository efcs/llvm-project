// RUN: %clang_cc1 -std=c++26 -fcontracts -ast-dump %s \
// RUN:   -fcontract-group-evaluation-semantic=grp=ignore | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -ast-dump=json %s \
// RUN:   -fcontract-group-evaluation-semantic=grp=ignore | FileCheck %s --check-prefix=JSON

int f(const int x)
  pre(x > 0)
  post(r : r > x)
  post [[clang::contract_group("grp")]] [[clang::contract_message("msg")]] (r2 : r2 > 0) {
  contract_assert [[clang::contract_message("m", false)]] (x != 3);
  return x;
}

// The source range of a contract assertion extends to its closing parenthesis;
// its attributes and result name are dumped as children.
// CHECK: ContractSpecifierDecl {{.*}} <line:7:3, line:9:88> line:7:3
// CHECK-NEXT: ContractStmt {{.*}} <col:3, col:12> pre enforce "x > 0"
// CHECK-NEXT: BinaryOperator
// CHECK: ContractStmt {{.*}} <line:8:3, col:17> post enforce "r : r > x"
// CHECK-NEXT: ResultNameDecl {{.*}} <col:8> col:8 referenced r 'int'
// CHECK-NEXT: BinaryOperator
// CHECK: ContractStmt {{.*}} <line:9:3, col:88> post ignore "r2 : r2 > 0"
// CHECK-NEXT: ContractGroupAttr {{.*}} "grp"
// CHECK-NEXT: ContractMessageAttr {{.*}} "msg" IncludeSourceText
// CHECK-NEXT: ResultNameDecl {{.*}} <col:77> col:77 referenced r2 'int' canonical
// CHECK: ContractStmt {{.*}} <line:10:3, col:66> contract_assert enforce "x != 3"
// CHECK-NEXT: ContractMessageAttr {{.*}} "m"
// CHECK-NOT: IncludeSourceText

// JSON: "kind": "ContractSpecifierDecl"
// JSON: "kind": "ContractStmt"
// JSON: "contractKind": "pre"
// JSON-NEXT: "semantic": "enforce"
// JSON-NEXT: "predicate": "x > 0"
// JSON: "kind": "ContractStmt"
// JSON: "contractKind": "post"
// JSON-NEXT: "semantic": "enforce"
// JSON-NEXT: "hasResultName": true
// JSON-NEXT: "predicate": "r : r > x"
// JSON: "kind": "ResultNameDecl"
// JSON: "name": "r"
// JSON-NEXT: "type": {
// JSON-NEXT: "qualType": "int"
// JSON: "contractKind": "post"
// JSON-NEXT: "semantic": "ignore"
// JSON-NEXT: "hasResultName": true
// JSON-NEXT: "predicate": "r2 : r2 > 0"
// JSON: "kind": "ContractGroupAttr"
// JSON: "kind": "ContractMessageAttr"
// JSON: "kind": "ResultNameDecl"
// JSON: "name": "r2"
// JSON-NEXT: "type": {
// JSON-NEXT: "qualType": "int"
// JSON-NEXT: }
// JSON-NEXT: "canonicalResultNameId":
// JSON: "contractKind": "contract_assert"
// JSON-NEXT: "semantic": "enforce"
// JSON-NEXT: "predicate": "x != 3"
