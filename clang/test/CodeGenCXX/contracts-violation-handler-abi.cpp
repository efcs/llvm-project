// The violation handler entry point takes the evaluation semantic and the
// detection mode as 'unsigned char', matching the underlying type of
// std::contracts::evaluation_semantic and std::contracts::detection_mode in
// the runtime library's definition of __handle_contract_violation_v3.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,OBSERVE
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,ENFORCE
// RUN: %clang_cc1 -std=c++26 -fcontracts \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,NOEXC

bool p(int);

// CHECK-LABEL: define {{.*}} @_Z1fi(
// OBSERVE: {{call|invoke}} void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 2, ptr noundef @contract.loc)
// OBSERVE: call void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 1, ptr noundef @contract.loc)
// ENFORCE: {{call|invoke}} void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 2, ptr noundef @contract.loc) #[[NORETURN:[0-9]+]]
// ENFORCE: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1, ptr noundef @contract.loc) #[[NORETURN]]
// ENFORCE-NEXT: unreachable
// NOEXC: contract.violation.handler:
// NOEXC-NEXT: %[[INFO:.*]] = phi ptr [ @contract.loc,
// NOEXC-NEXT: call void @__handle_contract_violation_v3(i8 noundef zeroext 1, i8 noundef zeroext 1, ptr noundef %[[INFO]]) #[[NORETURN:[0-9]+]]
// NOEXC-NEXT: unreachable
void f(int x) pre(p(x)) {}

// CHECK: declare void @__handle_contract_violation_v3(i8 noundef zeroext, i8 noundef zeroext, ptr noundef)
// ENFORCE: attributes #[[NORETURN]] = { noreturn }
// NOEXC: attributes #[[NORETURN]] = { noreturn }
