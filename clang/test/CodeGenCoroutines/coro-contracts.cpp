// The function contract assertions of a coroutine apply to the function as
// seen by its callers (the "ramp"), not to the coroutine body:
//  - The preconditions are evaluated before anything else, in particular
//    before the coroutine state is allocated and the parameters are copied.
//  - The postconditions are evaluated when the ramp returns to its caller,
//    after coro.end, and before the parameters are destroyed.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -I %S/Inputs -disable-llvm-passes -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=ignore -triple x86_64-linux-gnu \
// RUN:   -I %S/Inputs -disable-llvm-passes -emit-llvm -o - %s | FileCheck %s --check-prefix=IGNORE
//
// Make sure that the coroutine passes can deal with the result.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -I %S/Inputs -O0 -emit-llvm -o /dev/null %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -I %S/Inputs -O2 -emit-llvm -o /dev/null %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=quick_enforce -triple x86_64-windows-msvc \
// RUN:   -I %S/Inputs -O2 -emit-llvm -o /dev/null %s

#include "coroutine.h"

struct Task {
  struct promise_type {
    Task get_return_object();
    std::suspend_always initial_suspend();
    std::suspend_always final_suspend() noexcept;
    void return_void();
    void unhandled_exception();
  };
  ~Task();
  bool valid() const;
};

struct [[clang::trivial_abi]] Param {
  Param(Param &&);
  ~Param();
  int value() const;
};

bool check(int);
void body();

// CHECK-LABEL: define {{.*}} @_Z1f5ParamRKi(
// CHECK: %exn.slot = alloca ptr, align 8, !coro.outside.frame
// CHECK: %ehselector.slot = alloca i32, align 4, !coro.outside.frame
//
// The precondition refers to the original parameter object, before it is
// moved into the coroutine state.
// CHECK: call {{.*}} @_ZNK5Param5valueEv(ptr {{.*}} %P)
// CHECK: invoke {{.*}} @_Z5checki(
// CHECK: call token @llvm.coro.id(
// CHECK: call ptr @llvm.coro.begin(
// CHECK: invoke void @_ZN5ParamC1EOS_(
// CHECK: invoke void @_Z4bodyv()
//
// The postcondition refers to the result object and to the original
// parameter, not to the copy of the parameter in the coroutine state, which
// may already have been destroyed.
// CHECK: coro.ret:
// CHECK: call void @llvm.coro.end(
// CHECK-NOT: @_ZN5ParamD1Ev(
// CHECK: invoke {{.*}} @_ZNK4Task5validEv(ptr {{.*}} %agg.result)
// CHECK: load ptr, ptr %X.addr
// CHECK: invoke {{.*}} @_Z5checki(
// CHECK: call void @_ZN5ParamD1Ev(ptr {{.*}} %P)
// CHECK-NEXT: ret void
//
// The result object is destroyed if a postcondition exits via an exception.
// CHECK: call void @_ZN4TaskD1Ev(ptr {{.*}} %agg.result)
// CHECK: {{^}}}
//
// IGNORE-LABEL: define {{.*}} @_Z1f5ParamRKi(
// IGNORE: %exn.slot = alloca ptr, align 8{{$}}
// IGNORE-NOT: check
// IGNORE: {{^}}}
Task f(Param P, const int &X)
    pre(check(P.value()))
    post(R : R.valid() && check(X)) {
  body();
  co_return;
}

struct NoThrowTask {
  struct promise_type {
    NoThrowTask get_return_object();
    static NoThrowTask get_return_object_on_allocation_failure();
    std::suspend_always initial_suspend();
    std::suspend_always final_suspend() noexcept;
    void return_void();
    void unhandled_exception();
  };
  bool valid() const noexcept;
};

namespace std {
struct nothrow_t {};
constexpr nothrow_t nothrow = {};
} // namespace std
void *operator new(decltype(sizeof(0)), const std::nothrow_t &) noexcept;

// Both the object returned when the allocation fails and the object returned
// when the coroutine first suspends are result objects of the function.
//
// CHECK-LABEL: define {{.*}} @_Z1gv(
// CHECK: coro.ret.on.failure:
// CHECK-NEXT: call void @_ZN11NoThrowTask12promise_type39get_return_object_on_allocation_failureEv(
// CHECK-NEXT: br label %[[POST:.+]]
// CHECK: coro.ret:
// CHECK: call void @llvm.coro.end(
// CHECK-NEXT: br label %[[POST]]
// CHECK: [[POST]]:
// CHECK-NEXT: %{{.*}} = call {{.*}} @_ZNK11NoThrowTask5validEv(
NoThrowTask g() post(R : R.valid()) {
  body();
  co_return;
}
