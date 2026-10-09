// Tests for instrumentation of the predicates of function contract assertions.
//
// RUN: %clang_cc1 %s -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -o - -emit-llvm -fprofile-instrument=clang -triple x86_64-linux-gnu > %tgen
// RUN: FileCheck --input-file=%tgen -check-prefix=GEN %s
// RUN: %clang_cc1 %s -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -o - -emit-llvm -fprofile-instrument=clang -triple x86_64-linux-gnu > %tgenignore
// RUN: FileCheck --input-file=%tgenignore -check-prefix=GENIGNORE %s

// RUN: llvm-profdata merge %S/Inputs/cxx-contracts.proftext -o %t.profdata
// RUN: %clang_cc1 %s -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -o - -emit-llvm -fprofile-instrument-use=clang -fprofile-instrument-use-path=%t.profdata -triple x86_64-linux-gnu > %tuse
// RUN: FileCheck --input-file=%tuse -check-prefix=USE %s
// The profile can be used no matter which evaluation semantic was selected
// when it was collected: the same counters are assigned either way.
// RUN: %clang_cc1 %s -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -o - -emit-llvm -fprofile-instrument-use=clang -fprofile-instrument-use-path=%t.profdata -triple x86_64-linux-gnu 2>&1 > %tuseignore
// RUN: FileCheck --input-file=%tuseignore -check-prefix=USEIGNORE %s

bool check(int);

// The function is counted as entered before its preconditions are evaluated,
// and the conditions in the predicates have their own counters.
//
// GEN-LABEL: define {{.*}} @_Z1fii(
// GEN: store {{.*}} @[[FC:__profc__Z1fii]], align 8
// GEN: store {{.*}} @[[FC]], i64 8)
// GEN: call {{.*}} @_Z5checki(
// GEN: store {{.*}} @[[FC]], i64 16)
// GEN: contract.end:
// GEN: store {{.*}} @[[FC]], i64 40)
// GEN: store {{.*}} @[[FC]], i64 24)
// GEN: call {{.*}} @_Z5checki(
// GEN: store {{.*}} @[[FC]], i64 32)
// GEN-NOT: store {{.*}} @[[FC]],
// GEN: ret i32
//
// The counters are assigned but unused if the contracts are ignored.
// GENIGNORE: @[[FC:__profc__Z1fii]] = {{.*}} global [6 x i64] zeroinitializer
// GENIGNORE-LABEL: define {{.*}} @_Z1fii(
// GENIGNORE: store {{.*}} @[[FC]], align 8
// GENIGNORE: store {{.*}} @[[FC]], i64 40)
// GENIGNORE-NOT: store {{.*}} @[[FC]],
// GENIGNORE: ret i32
//
// USE-LABEL: define {{.*}} @_Z1fii(
// USE: br i1 %cmp, label %land.rhs, label %land.end, !prof ![[PRE:[0-9]+]]
// USE: br i1 %tobool, label %if.then, label %if.end, !prof ![[IF:[0-9]+]]
// USE: br i1 %cmp{{[0-9]+}}, label %lor.end, label %lor.rhs, !prof ![[POST:[0-9]+]]
// USE: ret i32
//
// USEIGNORE-LABEL: define {{.*}} @_Z1fii(
// USEIGNORE-NOT: _Z5checki
// USEIGNORE: br i1 %tobool, label %if.then, label %if.end, !prof ![[IF:[0-9]+]]
// USEIGNORE-NOT: _Z5checki
// USEIGNORE: ret i32
int f(const int x, int y)
    pre(x > 0 && check(x))
    post(r : r > 0 || check(r)) {
  if (y)
    return x;
  return y;
}

// A destructor of a class with virtual bases which has postconditions runs
// its body in both the complete and the base object destructor, so both are
// instrumented.
struct VBase {
  ~VBase();
};
struct S : virtual VBase {
  ~S() pre(check(1)) post(check(2));
};
// GEN-LABEL: define {{.*}} @_ZN1SD2Ev(
// GEN: store {{.*}} @__profc__ZN1SD2Ev, align 8
// GEN-LABEL: define {{.*}} @_ZN1SD1Ev(
// GEN: store {{.*}} @__profc__ZN1SD1Ev, align 8
S::~S() {}

// USE-DAG: ![[PRE]] = !{!"branch_weights", i32 91, i32 11}
// USE-DAG: ![[IF]] = !{!"branch_weights", i32 61, i32 41}
// USE-DAG: ![[POST]] = !{!"branch_weights", i32 71, i32 31}
// USEIGNORE-DAG: ![[IF]] = !{!"branch_weights", i32 61, i32 41}
