// The function contract assertions of a noexcept coroutine are evaluated by
// the ramp, which is noexcept: their violations are reported through the
// '_noexcept' entry points, which do not unwind. A contract assertion in the
// coroutine body is inside the implicit try block whose handler calls
// promise_type::unhandled_exception, so an exception thrown by the violation
// handler is caught there: the throwing entry points are used.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -I %S/Inputs -disable-llvm-passes -emit-llvm -o - %s | FileCheck %s

#include "coroutine.h"

struct Task {
  struct promise_type {
    Task get_return_object();
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void return_void();
    void unhandled_exception();
  };
};

bool p(int);

Task coro(const int x) noexcept pre(p(x)) post(p(0)) {
  contract_assert(p(x + 1));
  co_return;
}
// CHECK-LABEL: define {{.*}} @_Z4coroi(
// CHECK: call void @__cxa_contract_violation_pre_observe_pf_noexcept(
// CHECK: call ptr @llvm.coro.begin(
// CHECK: invoke void @__cxa_contract_violation_assert_observe_ex(
// CHECK: @_ZN4Task12promise_type19unhandled_exceptionEv(
// CHECK: invoke void @__cxa_contract_violation_assert_observe_pf(
// CHECK: call void @llvm.coro.end(
// CHECK: call void @__cxa_contract_violation_post_observe_pf_noexcept(

// In a coroutine that is not noexcept, the ramp reports violations through the
// throwing entry points too.
Task throwing_coro(const int x) pre(p(x)) {
  contract_assert(p(x + 1));
  co_return;
}
// CHECK-LABEL: define {{.*}} @_Z13throwing_coroi(
// CHECK: {{call|invoke}} void @__cxa_contract_violation_pre_observe_pf(
// CHECK: call ptr @llvm.coro.begin(
// CHECK: invoke void @__cxa_contract_violation_assert_observe_pf(
