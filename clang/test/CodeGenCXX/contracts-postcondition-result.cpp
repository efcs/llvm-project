// Postconditions are evaluated after the return value has been initialized
// and the result name refers to it.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -O2 -emit-llvm -o - %s | FileCheck %s --check-prefix=OPT

// CHECK-LABEL: define {{.*}} i32 @_Z6scalari(i32 noundef %x)
// CHECK: %[[RETVAL:.*]] = alloca i32
// CHECK: store i32 %{{.*}}, ptr %[[RETVAL]]
// CHECK: %[[R:.*]] = load i32, ptr %[[RETVAL]]
// CHECK: icmp eq i32 %[[R]],
// CHECK: contract.end:
// CHECK: %[[RV:.*]] = load i32, ptr %[[RETVAL]]
// CHECK: ret i32 %[[RV]]
//
// OPT-LABEL: define {{.*}} i32 @_Z6scalari(i32 noundef returned %x)
// OPT-NEXT: entry:
// OPT-NEXT: ret i32 %x
int scalar(const int x) post(r : r == x) { return x; }

int gi;
// CHECK-LABEL: define {{.*}} ptr @_Z3refv()
// CHECK: %[[RETVAL:.*]] = alloca ptr
// CHECK: store ptr @gi, ptr %[[RETVAL]]
// CHECK: %[[R:.*]] = load ptr, ptr %[[RETVAL]]
// CHECK: icmp eq ptr %[[R]], @gi
// CHECK: ret ptr
//
// OPT-LABEL: define {{.*}} ptr @_Z3refv()
// OPT-NEXT: entry:
// OPT-NEXT: ret ptr @gi
int &ref() post(r : &r == &gi) { return gi; }

// CHECK-LABEL: define {{.*}} ptr @_Z8read_refv()
// CHECK: %[[RETVAL:.*]] = alloca ptr
// CHECK: %[[P:.*]] = load ptr, ptr %[[RETVAL]]
// CHECK: load i32, ptr %[[P]]
// CHECK: icmp eq i32
int &read_ref() post(r : r == 7) { return gi; }

// CHECK-LABEL: define {{.*}} void @_Z7void_fnv()
// CHECK: call {{.*}} @_Z5checkv()
// CHECK: ret void
bool check();
void void_fn() post(check()) {}
