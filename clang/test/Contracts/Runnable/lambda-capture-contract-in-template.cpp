// Mirror of g++.dg/contracts/cpp26/lambda-capture-contract-in-template.C.
//
// A contract predicate naming a CAPTURE, on a lambda inside a template,
// segfaulted GCC (16.2.0 and trunk).  Two defects there: nothing
// mapped the pattern lambda's capture proxy to the instantiation's, and even
// once it resolved, the outer-variable check rejected it because its
// carve-out covered a contract condition naming a parameter but not one
// naming a capture.  Clang accepts every shape; this is a regression pin.
//
// The row that earns the test is `copy_not_alias': a by-value capture is a
// copy taken when the closure is built, so a predicate that read the
// enclosing variable instead would compile, fire no violation, and be wrong.
// Every check reports the value the predicate saw.
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

template <class T> int by_value(T a) {
  int lo = 5;
  auto l = [lo](int b) pre(probe(b - lo)) { return b; };
  return l((int)a);
}

template <class T> int by_ref(T a) {
  int lo = 100;
  auto l = [&lo](int b) pre(probe(b + lo)) { return b; };
  return l((int)a);
}

template <class T> int two_captures(T a) {
  int p = 3, q = 40;
  auto l = [p, q](int b) pre(probe(b + p + q)) { return b; };
  return l((int)a);
}

// A capture-default.  The body odr-uses `lo' as well, deliberately: a lambda
// may not acquire a capture solely because a contract assertion names it,
// since that would change the closure type, and both compilers reject the
// predicate-only spelling.
template <class T> int capture_default(T a) {
  int lo = 7;
  auto l = [=](int b) pre(probe(b * lo)) { return b + lo; };
  return l((int)a);
}

template <class T> int post_capture(T a) {
  int lo = 2;
  auto l = [lo](int b) post(r : probe(r + lo)) { return b * 10; };
  return l((int)a);
}

// The predicate must see the captured COPY, not the later value.
template <class T> int copy_not_alias(T a) {
  int lo = 1;
  auto l = [lo](int b) pre(probe(lo)) { return b; };
  lo = 999;
  return l((int)a);
}

template <class T> struct S {
  int m(T a) {
    int lo = 6;
    auto l = [lo](int b) pre(probe(b - lo)) { return b; };
    return l((int)a);
  }
};

// Control: the same lambda outside a template.
static int non_template() {
  int lo = 4;
  auto l = [lo](int b) pre(probe(b - lo)) { return b; };
  return l(10);
}

template <class T> int violates(T a) {
  int hi = 100;
  auto l = [hi](int b) pre(b > hi) { return b; };
  return l((int)a);
}

int main() {
  if (by_value(15) != 15) __builtin_abort();
  check_seen("by-value capture in a function template", 10);
  if (by_value(25.0) != 25) __builtin_abort();
  check_seen("by-value capture, second instantiation", 20);

  if (by_ref(1) != 1) __builtin_abort();
  check_seen("by-reference capture", 101);

  if (two_captures(1) != 1) __builtin_abort();
  check_seen("two captures", 44);

  if (capture_default(3) != 10) __builtin_abort();
  check_seen("capture default", 21);

  if (post_capture(5) != 50) __builtin_abort();
  check_seen("postcondition naming a capture and the result", 52);

  if (copy_not_alias(0) != 0) __builtin_abort();
  check_seen("the capture is a copy, not an alias", 1);

  {
    S<int> s;
    if (s.m(16) != 16) __builtin_abort();
  }
  check_seen("capture in a class-template member", 10);
  {
    S<long> s;
    if (s.m(26) != 26) __builtin_abort();
  }
  check_seen("class-template member, second instantiation", 20);

  if (non_template() != 10) __builtin_abort();
  check_seen("control: the same lambda outside a template", 6);

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

  if (failures)
    __builtin_abort();
  return 0;
}
