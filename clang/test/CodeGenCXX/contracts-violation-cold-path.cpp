// The code which handles a contract violation is on the cold path: the branch
// on the predicate is annotated so that the fast path is the fall-through
// one. With the observe semantic the optimizer learns nothing from a check
// (there is no llvm.assume), and the predicate is evaluated only once.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -fcxx-exceptions -fexceptions -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce \
// RUN:   -fcxx-exceptions -fexceptions -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce \
// RUN:   -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=quick_enforce \
// RUN:   -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,QUICK
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -fcxx-exceptions -fexceptions -triple x86_64-linux-gnu -O2 -emit-llvm -o - %s | FileCheck %s --check-prefix=OPT

bool check(int) noexcept;
int work(int);

// CHECK-LABEL: define {{.*}} @_Z1fi(
// CHECK: br i1 %{{.*}}, label %contract.end, label %contract.violation{{.*}}, !prof ![[WEIGHTS:[0-9]+]]
// CHECK: br i1 %{{.*}}, label %contract.end{{.*}}, label %contract.violation{{.*}}, !prof ![[WEIGHTS]]
// CHECK: br i1 %{{.*}}, label %contract.end{{.*}}, label %contract.violation{{.*}}, !prof ![[WEIGHTS]]
// CHECK: ret i32
//
// With the quick_enforce semantic all violations in a function share one
// trap.
// QUICK: contract.violation.trap.handler:
// QUICK-NEXT: call void @llvm.trap()
// QUICK-NEXT: unreachable
// QUICK-NOT: call void @llvm.trap()
//
// CHECK: ![[WEIGHTS]] = !{!"branch_weights", i32 [[#LIKELY:]], i32 [[#UNLIKELY:]]}
//
// OPT-LABEL: define {{.*}} @_Z1fi(
// OPT-COUNT-3: call {{.*}} @_Z5checki(
// OPT-NOT: call {{.*}} @_Z5checki(
// OPT: ret i32
// OPT-NOT: llvm.assume
int f(const int x) pre(check(x)) post(r : check(r)) {
  contract_assert(check(x + 1));
  return work(x);
}
