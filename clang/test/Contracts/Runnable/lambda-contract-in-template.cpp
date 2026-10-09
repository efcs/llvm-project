// Mirror of g++.dg/contracts/cpp26/lambda-contract-in-template.C.
//
// GCC never substituted a lambda's contract specifiers when the lambda was
// instantiated as part of a template: tsubst_lambda_expr builds the call
// operator and substitutes its body itself, so it never reached the place
// that substitutes an ordinary function's contracts.  The instantiation kept
// the pattern's contract trees, whose predicate named the pattern's
// parameters and whose result binding belonged to the pattern.  Four ICEs
// came out of that one defect, one of which needed two instantiations of the
// same template to show up.
//
// Clang has never had it, so this is a regression pin, not a fix.
//
// Compiling is not the property under test: a predicate wired to the wrong
// parameter can still compile and simply read the wrong object.  Every check
// reports the value the PREDICATE saw.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t && %t

#include <contracts>
#include <cstdio>

static int viol = 0;
void handle_contract_violation(const std::contracts::contract_violation &) {
  ++viol;
}

static int seen = -1;
static int failures = 0;

// A predicate cannot assign to a variable -- what it names is const -- so
// record through a call.
static bool probe(int observed) {
  seen = observed;
  return true;
}

static void check_seen(const char *what, int want) {
  if (seen != want) {
    std::printf("FAIL: %s: predicate saw %d, expected %d\n", what, seen, want);
    ++failures;
  }
  seen = -1;
}

// The smallest shape: no nesting, nothing generic.
template <class T> int pre_plain(T a) {
  auto l = [](int b) pre(probe(b)) { return b; };
  return l((int)a);
}

// A generic lambda, whose call operator is substituted from its own
// instantiation rather than with the enclosing template.
template <class T> int pre_generic(T a) {
  auto l = [](auto b) pre(probe(b)) { return b; };
  return l((int)a);
}

// A postcondition with a result name.  Instantiated more than once below:
// that is what reached a release build on the GCC side.
template <class T> int post_result(T a) {
  auto l = [](int b) post(r : probe(r)) { return b * 10; };
  return l((int)a);
}

// The same defect reached through a class template rather than a function
// template.
template <class T> struct S {
  int m(T a) {
    auto l = [](int b) pre(probe(b)) { return b; };
    return l((int)a);
  }
};

// The shape the bug was originally (mis)reported as: a lambda nested inside a
// generic lambda.  Kept so it stays covered.
static int nested_in_generic(int a) {
  auto outer = [](auto x) {
    auto inner = [](int b) pre(probe(b)) { return b; };
    return inner((int)x);
  };
  return outer(a);
}

// The predicate must still be able to fail.
template <class T> int violates(T a) {
  auto l = [](int b) pre(b > 100) { return b; };
  return l((int)a);
}

// Controls: each drops one ingredient the bug needed.
static int g_v = 7;

template <class T> int ctl_nothing_local(T a) {
  auto l = [](int b) pre(probe(99)) { return b; };
  return l((int)a);
}
template <class T> int ctl_global(T a) {
  auto l = [](int b) pre(probe(g_v)) { return b; };
  return l((int)a);
}
// A contract_assert is part of the body and substitutes with it.
template <class T> int ctl_assert(T a) {
  auto l = [](int b) {
    contract_assert(probe(b));
    return b;
  };
  return l((int)a);
}
static int ctl_non_template() {
  auto l = [](int b) pre(probe(b)) { return b; };
  return l(33);
}
template <class T> int ctl_on_the_template(T a) pre(probe((int)a)) {
  return (int)a;
}

int main() {
  if (pre_plain(7) != 7) __builtin_abort();
  check_seen("pre, plain lambda in a function template", 7);
  if (pre_plain(9) != 9) __builtin_abort();
  check_seen("pre, same instantiation called again", 9);
  if (pre_plain(4.0) != 4) __builtin_abort();
  check_seen("pre, second instantiation", 4);

  if (pre_generic(8) != 8) __builtin_abort();
  check_seen("pre, generic lambda", 8);
  if (pre_generic(2.0) != 2) __builtin_abort();
  check_seen("pre, generic lambda, second instantiation", 2);

  if (post_result(7) != 70) __builtin_abort();
  check_seen("post result binding", 70);
  if (post_result(3.0) != 30) __builtin_abort();
  check_seen("post result binding, second instantiation", 30);
  if (post_result(2L) != 20) __builtin_abort();
  check_seen("post result binding, third instantiation", 20);

  {
    S<int> s;
    if (s.m(11) != 11) __builtin_abort();
  }
  check_seen("pre, lambda in a class-template member", 11);
  {
    S<long> s;
    if (s.m(12) != 12) __builtin_abort();
  }
  check_seen("pre, class template, second instantiation", 12);

  if (nested_in_generic(6) != 6) __builtin_abort();
  check_seen("pre, lambda nested in a generic lambda", 6);

  if (viol != 0) {
    std::printf("FAIL: unexpected violations: %d\n", viol);
    ++failures;
  }
  viol = 0;

  if (violates(1) != 1) __builtin_abort();
  if (viol != 1) {
    std::printf("FAIL: a failing predicate did not report\n");
    ++failures;
  }
  viol = 0;

  if (ctl_nothing_local(1) != 1) __builtin_abort();
  check_seen("control: predicate names nothing local", 99);
  if (ctl_global(1) != 1) __builtin_abort();
  check_seen("control: predicate names a global", 7);
  if (ctl_assert(5) != 5) __builtin_abort();
  check_seen("control: contract_assert in the body", 5);
  if (ctl_non_template() != 33) __builtin_abort();
  check_seen("control: the same lambda in a non-template", 33);
  if (ctl_on_the_template(44) != 44) __builtin_abort();
  check_seen("control: contract on the template itself", 44);

  if (failures)
    __builtin_abort();
  return 0;
}
