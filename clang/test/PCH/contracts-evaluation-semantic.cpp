// The evaluation semantic of a contract assertion comes from the translation
// unit that evaluates it; a precompiled header built with a different
// -fcontract-evaluation-semantic does not pin the semantic of its contracts.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=ignore -emit-pch -o %t.ignore.pch %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce -emit-pch -o %t.enforce.pch %s
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce -include-pch %t.ignore.pch \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ENFORCE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=ignore -include-pch %t.enforce.pch \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,IGNORE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=observe \
// RUN:   -fcontract-group-evaluation-semantic=grp=ignore \
// RUN:   -include-pch %t.enforce.pch \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,OBSERVE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce -include-pch %t.ignore.pch \
// RUN:   -ast-dump-all -ast-dump-filter=f %s | FileCheck %s --check-prefix=DUMP

#ifndef HEADER
#define HEADER

inline int f(const int x) pre(x > 0) post(r : r == x) {
  contract_assert(x != 3);
  return x;
}
template <class T> T t(const T x) pre(x > 0) { return x; }
inline int grouped(int x) pre [[clang::contract_group("grp")]] (x > 0) {
  return x;
}

#else

int use(int v) { return f(v) + t(v) + grouped(v); }

// CHECK-LABEL: define {{.*}} @_Z1fi(
// ENFORCE: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1,
// ENFORCE-NEXT: unreachable
// OBSERVE: call void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 1,
// OBSERVE-NEXT: br label %contract.end
// IGNORE-NOT: contract
// IGNORE: ret i32

// CHECK-LABEL: define {{.*}} @_Z1tIiET_S0_(
// ENFORCE: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1,
// OBSERVE: call void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 1,
// IGNORE-NOT: contract
// IGNORE: ret i32

// CHECK-LABEL: define {{.*}} @_Z7groupedi(
// ENFORCE: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1,
// OBSERVE-NOT: contract
// OBSERVE: ret i32
// IGNORE-NOT: contract
// IGNORE: ret i32

// DUMP: FunctionDecl {{.*}} f 'int (const int)'
// DUMP: ContractStmt {{.*}} pre enforce "x > 0"
// DUMP: ContractStmt {{.*}} post enforce "r : r == x"
// DUMP: ContractStmt {{.*}} contract_assert enforce "x != 3"

#endif
