// The entry point through which a contract violation is reported depends on
// whether an exception thrown by the contract-violation handler could be
// caught: where it would terminate the program anyway (a noexcept function,
// unless the assertion is inside a try block of it; or a translation unit
// without C++ exception handling), the '_noexcept' entry points are used, which
// do not unwind and terminate if the handler throws. Cleanups between the
// assertion and the noexcept boundary do not matter ([except.terminate]).
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,EH
//
// -fexceptions without -fcxx-exceptions (e.g. Objective-C++ with only
// Objective-C exceptions) does not catch C++ exceptions and gives noexcept
// functions no terminate scope, so every violation uses a '_noexcept' entry
// point, as without -fexceptions.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,NOCXXEH
// RUN: %clang_cc1 -std=c++26 -fcontracts \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,NOCXXEH

bool p(int);
struct D {
  ~D();
};
struct S {
  ~S();
  S() noexcept pre(p(2));
  void m() noexcept(false);
  __attribute__((nothrow)) void nt();
  void nx() noexcept;
};

// A destructor is implicitly noexcept. (The violation of a predicate that
// exits via an exception is reported from within the handler for it, which
// comes first in the output; the '_noexcept' entry points are plain calls.)
S::~S() { contract_assert(p(1)); }
// CHECK-LABEL: define {{.*}} @_ZN1SD2Ev(
// EH: call ptr @__cxa_begin_catch(
// EH-NEXT: call void @__cxa_contract_violation_assert_observe_ex_noexcept(
// CHECK: call void @__cxa_contract_violation_assert_observe_pf_noexcept(

// The preconditions of a noexcept constructor.
S::S() noexcept pre(p(2)) {}
// CHECK-LABEL: define {{.*}} @_ZN1SC2Ev(
// CHECK: call void @__cxa_contract_violation_pre_observe_pf_noexcept(

// noexcept(false) is not noexcept. (The throwing entry points are invoked
// only where a landing pad is needed; what matters is the entry point.)
void S::m() noexcept(false) { contract_assert(p(3)); }
// CHECK-LABEL: define {{.*}} @_ZN1S1mEv(
// EH: call ptr @__cxa_begin_catch(
// EH-NEXT: invoke void @__cxa_contract_violation_assert_observe_ex(
// EH: {{call|invoke}} void @__cxa_contract_violation_assert_observe_pf(
// NOCXXEH: call void @__cxa_contract_violation_assert_observe_pf_noexcept(

// __attribute__((nothrow)) gives the function a non-throwing exception
// specification, like noexcept.
__attribute__((nothrow)) void S::nt() { contract_assert(p(4)); }
// CHECK-LABEL: define {{.*}} @_ZN1S2ntEv(
// CHECK: call void @__cxa_contract_violation_assert_observe_pf_noexcept(

void S::nx() noexcept { contract_assert(p(5)); }
// CHECK-LABEL: define {{.*}} @_ZN1S2nxEv(
// EH: call ptr @__cxa_begin_catch(
// EH-NEXT: call void @__cxa_contract_violation_assert_observe_ex_noexcept(
// CHECK: call void @__cxa_contract_violation_assert_observe_pf_noexcept(

// throw() is noexcept since C++17.
void throw_spec() throw() { contract_assert(p(6)); }
// CHECK-LABEL: define {{.*}} @_Z10throw_specv(
// CHECK: call void @__cxa_contract_violation_assert_observe_pf_noexcept(

// A cleanup between the assertion and the function boundary does not change
// the choice.
void cleanup_in_noexcept() noexcept {
  D d;
  contract_assert(p(7));
}
// CHECK-LABEL: define {{.*}} @_Z19cleanup_in_noexceptv(
// CHECK: call void @__cxa_contract_violation_assert_observe_pf_noexcept(
// CHECK: call void @_ZN1DD1Ev(

// The postconditions of a noexcept function.
void post_noexcept(const int x) noexcept post(p(x)) {}
// CHECK-LABEL: define {{.*}} @_Z13post_noexcepti(
// CHECK: call void @__cxa_contract_violation_post_observe_pf_noexcept(

void plain(const int x) pre(p(x)) { contract_assert(p(8)); }
// CHECK-LABEL: define {{.*}} @_Z5plaini(
// EH: {{call|invoke}} void @__cxa_contract_violation_pre_observe_pf(
// EH: {{call|invoke}} void @__cxa_contract_violation_assert_observe_pf(
// NOCXXEH: call void @__cxa_contract_violation_pre_observe_pf_noexcept(
// NOCXXEH: call void @__cxa_contract_violation_assert_observe_pf_noexcept(

#ifdef __cpp_exceptions
// Inside a try block of a noexcept function, the handler's exception can be
// caught, so the throwing entry points are used, however deeply nested.
void nested_try() noexcept {
  try {
    try {
      contract_assert(p(9));
    } catch (int) {
    }
  } catch (...) {
  }
}
// EH-LABEL: define {{.*}} @_Z10nested_tryv(
// EH: invoke void @__cxa_contract_violation_assert_observe_pf(

// Inside a catch handler of a noexcept function, there is nothing left to
// catch what the handler throws: the '_noexcept' entry points are used, and
// the exception being handled stays active for the terminate handler.
void in_catch_noexcept() noexcept {
  try {
    throw 1;
  } catch (...) {
    contract_assert(p(10));
  }
}
// EH-LABEL: define {{.*}} @_Z17in_catch_noexceptv(
// EH: call ptr @__cxa_begin_catch(
// EH: call void @__cxa_contract_violation_assert_observe_pf_noexcept(
// EH: {{call|invoke}} void @__cxa_end_catch()

// The try block of a function-try-block covers the body.
void ftb() noexcept try { contract_assert(p(11)); } catch (...) {
}
// EH-LABEL: define {{.*}} @_Z3ftbv(
// EH: invoke void @__cxa_contract_violation_assert_observe_pf(
#endif

// A lambda is a function of its own: its noexcept-ness decides, not the
// enclosing function's.
void lambda_in_plain() {
  auto l = []() noexcept { contract_assert(p(12)); };
  l();
}
void lambda_in_noexcept() noexcept {
  auto l = [] { contract_assert(p(13)); };
  l();
}
void lambda_with_pre() {
  auto l = [](int x) noexcept pre(p(x)) {};
  l(14);
}
// CHECK-LABEL: define {{.*}} @"_ZZ15lambda_in_plainvENK3$_0clEv"(
// CHECK: call void @__cxa_contract_violation_assert_observe_pf_noexcept(
// CHECK-LABEL: define {{.*}} @"_ZZ18lambda_in_noexceptvENK3$_0clEv"(
// EH: {{call|invoke}} void @__cxa_contract_violation_assert_observe_pf(
// NOCXXEH: call void @__cxa_contract_violation_assert_observe_pf_noexcept(
// CHECK-LABEL: define {{.*}} @"_ZZ15lambda_with_prevENK3$_0clEi"(
// CHECK: call void @__cxa_contract_violation_pre_observe_pf_noexcept(
