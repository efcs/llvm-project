// The postcondition assertions of a function are only evaluated when the
// function returns normally. If the end of the function is unreachable, no
// code is emitted for them.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=quick_enforce -triple x86_64-windows-msvc \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -O2 -emit-llvm -o /dev/null %s

bool pre_check();
bool post_check();
[[noreturn]] void die();
void work();

struct [[clang::trivial_abi]] TA {
  TA();
  TA(const TA &);
  ~TA();
};

// CHECK-LABEL: define {{.*}}loop
// CHECK: pre_check
// CHECK-NOT: post_check
// CHECK: {{^}}}
[[noreturn]] void loop() pre(pre_check()) post(post_check()) {
  for (;;)
    work();
}

// CHECK-LABEL: define {{.*}}aborts
// CHECK: pre_check
// CHECK-NOT: post_check
// CHECK: {{^}}}
[[noreturn]] void aborts() pre(pre_check()) post(post_check()) {
  __builtin_abort();
}

// CHECK-LABEL: define {{.*}}calls_noreturn
// CHECK: pre_check
// CHECK-NOT: post_check
// CHECK: {{^}}}
int calls_noreturn() pre(pre_check()) post(r : r == 0 && post_check()) {
  die();
}

// CHECK-LABEL: define {{.*}}with_param
// CHECK-NOT: post_check
// CHECK: {{^}}}
void with_param(TA t) post(post_check()) { die(); }

// Only the path that returns evaluates the postcondition.
//
// CHECK-LABEL: define {{.*}}sometimes
// CHECK: post_check
// CHECK-NOT: post_check
// CHECK: {{^}}}
int sometimes(bool b) post(post_check()) {
  if (b)
    return 1;
  die();
}
