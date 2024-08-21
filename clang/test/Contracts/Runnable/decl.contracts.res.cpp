// RUN: %clangxx  -std=c++26 %s -fcontracts -o %t -fcontract-evaluation-semantic=observe -g
// RUN: %t

#include "contracts-runtime.h"
#ifdef NDEBUG
#undef NDEBUG
#endif

#include <cassert>
struct FailureFlag {
  FailureFlag() = default;
  FailureFlag(FailureFlag const&) = delete;

  void expect_violation(bool value = true) {
    assert(!have_expectation && !have_violation);
    have_expectation = true;
    is_violation_expected = value;
  }

  void expect_none() {
    assert(!have_expectation && !have_violation);
    have_expectation = true;
    is_violation_expected = false;
  }

  void observe_violation() {
    assert(have_expectation && !have_violation);
    have_violation = true;
  }

  void finish() {
    assert(have_expectation && (is_violation_expected == have_violation));
    have_expectation = false;
    have_violation = false;
  }

  bool have_expectation = false;
  bool is_violation_expected = false;
  bool have_violation = false;

  ~FailureFlag() {
    assert(!have_expectation || (have_expectation && is_violation_expected == have_violation));
  }
};

constinit FailureFlag flag;
void handle_contract_violation(const std::contracts::contract_violation& cv) {
  flag.observe_violation();
}

struct A {}; // trivially copyable
struct B {   // not trivially copyable
  B() {}
  B(const B &) {}
};
template <typename T> T f(T * const ptr) post(r : &r == ptr) { return T{}; }
int main() {
  flag.expect_violation();
  A a = f(&a); // The postcondition check may fail.
  flag.finish();
  flag.expect_none();
  B b = f(&b); // The postcondition check is guaranteed to succeed.
  flag.finish();
}
