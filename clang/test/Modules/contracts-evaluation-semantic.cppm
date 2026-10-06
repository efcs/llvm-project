// The evaluation semantic of a contract assertion is a property of the
// translation unit that evaluates it, not of the module file the contract was
// read from: a module built with -fcontract-evaluation-semantic=ignore still
// has its contracts checked in an importer built with enforce, and vice
// versa. The group overrides of the importer apply as well.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: split-file %s %t
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=ignore \
// RUN:   %t/M.cppm -emit-module-interface -o %t/M.ignore.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce \
// RUN:   %t/M.cppm -emit-module-interface -o %t/M.enforce.pcm
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce \
// RUN:   -fmodule-file=M=%t/M.ignore.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp --check-prefixes=CHECK,ENFORCE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=ignore \
// RUN:   -fmodule-file=M=%t/M.enforce.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp --check-prefixes=CHECK,IGNORE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=observe \
// RUN:   -fmodule-file=M=%t/M.enforce.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp --check-prefixes=CHECK,OBSERVE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=quick_enforce \
// RUN:   -fmodule-file=M=%t/M.ignore.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp --check-prefixes=CHECK,QUICK
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce \
// RUN:   -fcontract-group-evaluation-semantic=grp=ignore \
// RUN:   -fmodule-file=M=%t/M.enforce.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp --check-prefixes=CHECK,GROUP
//
// The AST dump of the imported function shows the importer's semantic.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce \
// RUN:   -fmodule-file=M=%t/M.ignore.pcm %t/Use.cpp -ast-dump-all \
// RUN:   -ast-dump-filter=f | FileCheck %t/Use.cpp --check-prefix=DUMP

//--- M.cppm
export module M;
export inline int f(const int x) pre(x > 0) post(r : r == x) {
  contract_assert(x != 3);
  return x;
}
export template <class T> T t(const T x) pre(x > 0) { return x; }
export inline int grouped(int x) pre [[clang::contract_group("grp")]] (x > 0) {
  return x;
}

//--- Use.cpp
import M;
int use(int v) { return f(v) + t(v) + grouped(v); }

// CHECK-LABEL: define {{.*}} @_ZW1M1fi(
// ENFORCE: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1,
// ENFORCE-NEXT: unreachable
// OBSERVE: call void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 1,
// OBSERVE-NEXT: br label %contract.end
// QUICK: contract.violation.trap.handler:
// QUICK-NEXT: call void @llvm.trap()
// GROUP: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1,
// IGNORE-NOT: contract
// IGNORE: ret i32

// CHECK-LABEL: define {{.*}} @_ZW1M1tIiET_S1_(
// ENFORCE: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1,
// OBSERVE: call void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 1,
// QUICK: call void @llvm.trap()
// GROUP: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1,
// IGNORE-NOT: contract
// IGNORE: ret i32

// CHECK-LABEL: define {{.*}} @_ZW1M7groupedi(
// ENFORCE: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1,
// OBSERVE: call void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 1,
// QUICK: call void @llvm.trap()
// GROUP-NOT: contract
// GROUP: ret i32
// IGNORE-NOT: contract
// IGNORE: ret i32

// DUMP: FunctionDecl {{.*}} f 'int (const int)'
// DUMP: ContractStmt {{.*}} pre enforce "x > 0"
// DUMP: ContractStmt {{.*}} post enforce "r : r == x"
// DUMP: ContractStmt {{.*}} contract_assert enforce "x != 3"
