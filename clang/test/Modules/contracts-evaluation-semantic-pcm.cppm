// The evaluation semantic of a contract assertion is a property of the
// translation unit that evaluates it (see contracts-evaluation-semantic.cppm
// for importers). This also holds for the two-phase build of a named module,
// where the module file produced by the first phase (--precompile) is compiled
// itself in the second phase (-c m.pcm): the semantic (and the group
// overrides) of the second phase apply to the functions defined in the module,
// whatever the first phase was run with. The language options read from the
// module file do not carry a semantic.
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
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce \
// RUN:   -fcontract-group-evaluation-semantic=grp=ignore \
// RUN:   %t/M.cppm -emit-module-interface -o %t/M.group.pcm
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce \
// RUN:   %t/M.ignore.pcm -emit-llvm -o - \
// RUN:   | FileCheck %t/M.cppm --check-prefixes=CHECK,ENFORCE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=ignore \
// RUN:   %t/M.enforce.pcm -emit-llvm -o - \
// RUN:   | FileCheck %t/M.cppm --check-prefixes=CHECK,IGNORE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=observe \
// RUN:   %t/M.enforce.pcm -emit-llvm -o - \
// RUN:   | FileCheck %t/M.cppm --check-prefixes=CHECK,OBSERVE
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=quick_enforce \
// RUN:   %t/M.ignore.pcm -emit-llvm -o - \
// RUN:   | FileCheck %t/M.cppm --check-prefixes=CHECK,QUICK
//
// The group overrides of the second phase apply; those of the first phase
// do not.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce \
// RUN:   -fcontract-group-evaluation-semantic=grp=ignore \
// RUN:   %t/M.enforce.pcm -emit-llvm -o - \
// RUN:   | FileCheck %t/M.cppm --check-prefixes=CHECK,GROUP
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=ignore \
// RUN:   -fcontract-group-evaluation-semantic=grp=enforce \
// RUN:   %t/M.group.pcm -emit-llvm -o - \
// RUN:   | FileCheck %t/M.cppm --check-prefixes=CHECK,GROUPONLY
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=enforce \
// RUN:   %t/M.group.pcm -emit-llvm -o - \
// RUN:   | FileCheck %t/M.cppm --check-prefixes=CHECK,ENFORCE
//
// The AST dump of the module file shows the semantic of the second phase.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fcontract-evaluation-semantic=observe \
// RUN:   %t/M.enforce.pcm -ast-dump-all -ast-dump-filter=f \
// RUN:   | FileCheck %t/M.cppm --check-prefix=DUMP

//--- M.cppm
export module M;
export int f(const int x) pre(x > 0) post(r : r == x) {
  contract_assert(x != 3);
  return x;
}
export template <class T> T t(const T x) pre(x > 0) { return x; }
template int t<int>(int);
export int grouped(int x) pre [[clang::contract_group("grp")]] (x > 0) {
  return x;
}

// CHECK-LABEL: define {{.*}} @_ZW1M1fi(
// ENFORCE: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr @__contract_data
// ENFORCE-NEXT: unreachable
// OBSERVE: call void @__cxa_contract_violation_pre_observe_pf_noexcept(ptr @__contract_data
// OBSERVE-NEXT: br label %contract.end
// QUICK: contract.violation.trap.handler:
// QUICK-NEXT: call void @llvm.trap()
// GROUP: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr @__contract_data
// GROUPONLY-NOT: contract
// GROUPONLY: ret i32
// IGNORE-NOT: contract
// IGNORE: ret i32

// CHECK-LABEL: define {{.*}} @_ZW1M1tIiET_S1_(
// ENFORCE: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr @__contract_data
// OBSERVE: call void @__cxa_contract_violation_pre_observe_pf_noexcept(ptr @__contract_data
// QUICK: call void @llvm.trap()
// GROUP: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr @__contract_data
// GROUPONLY-NOT: contract
// GROUPONLY: ret i32
// IGNORE-NOT: contract
// IGNORE: ret i32

// CHECK-LABEL: define {{.*}} @_ZW1M7groupedi(
// ENFORCE: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr @__contract_data
// OBSERVE: call void @__cxa_contract_violation_pre_observe_pf_noexcept(ptr @__contract_data
// QUICK: call void @llvm.trap()
// GROUP-NOT: contract
// GROUP: ret i32
// GROUPONLY: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr @__contract_data
// IGNORE-NOT: contract
// IGNORE: ret i32

// DUMP: FunctionDecl {{.*}} f 'int (const int)'
// DUMP: ContractStmt {{.*}} pre observe "x > 0"
// DUMP: ContractStmt {{.*}} post observe "r : r == x"
// DUMP: ContractStmt {{.*}} contract_assert observe "x != 3"
