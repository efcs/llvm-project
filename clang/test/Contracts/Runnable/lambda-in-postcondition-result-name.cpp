// Mirror of g++.dg/contracts/cpp26/lambda-in-postcondition-result-name.C.
//
// On the GCC side a lambda-expression in the predicate of a postcondition on
// a NON-MEMBER function did not parse when the postcondition had a
// result-name-introducer:
//
//   int f (int x) post (r : [] { return ok (); } ()) { return x; }
//   error: expected ')' before '{' token
//
// The contract path there raises the "we are in a template" flag whenever a
// result name is present, because the result variable's type is unknown while
// the predicate is parsed off the declarator, and lambda parsing did not cope.
// Clang parses all four forms; this is a regression pin.
//
// Every check reports the value the predicate saw, so a lambda that parsed
// but was wired up wrongly would fail rather than pass.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t && %t

#include <contracts>
#include <cstdio>

void handle_contract_violation(const std::contracts::contract_violation &) {}

static int seen = -1;
static int failures = 0;

static bool record(int v) {
  seen = v;
  return true;
}

static void check_seen(const char *what, int want) {
  if (seen != want) {
    std::printf("FAIL: %s: predicate saw %d, expected %d\n", what, seen, want);
    ++failures;
  }
  seen = -1;
}

// The shape that failed on GCC: non-member, postcondition, result name.
int first(int x) post(r : [] { return record(1); }()) { return x; }

// The lambda's position in the predicate is irrelevant -- the flag GCC raised
// was in effect for the whole parse -- so pin first, later and parenthesized.
int later(int x) post(r : r >= 0 && [] { return record(2); }()) { return x; }

int parenthesized(int x) post(r : ([] { return record(3); }())) { return x; }

// The result name used in the predicate alongside the lambda.
int uses_result(int x) post(r : r == 7 && [] { return record(4); }()) {
  return x;
}

// A capture, which reaches the capture machinery as well as the parser.
int with_capture(const int x) post(r : [x] { return record(x); }()) {
  return x;
}

// A lambda nested inside a lambda in the predicate.
int nested_lambda(int x)
    post(r : [] { return [] { return record(9); }(); }()) {
  return x;
}

// Controls: no result name, and a precondition.  Both always parsed.
int no_result_name(int x) post([] { return record(5); }()) { return x; }
int precondition(int x) pre([] { return record(6); }()) { return x; }

// A member function with a result name -- the path that already worked on GCC.
struct S {
  int mem(int x) post(r : [] { return record(7); }()) { return x; }
};

// A real template, where the flag genuinely is raised.  Instantiated three
// times: substitution really does run over this predicate.
template <class T> T templated(T x) post(r : [] { return record(8); }()) {
  return x;
}

int main() {
  first(0);
  check_seen("free function, result name", 1);
  later(0);
  check_seen("lambda later in the predicate", 2);
  parenthesized(0);
  check_seen("parenthesized lambda", 3);
  uses_result(7);
  check_seen("result name used alongside the lambda", 4);
  with_capture(9);
  check_seen("capturing lambda in the predicate", 9);
  nested_lambda(0);
  check_seen("lambda nested inside a lambda", 9);

  no_result_name(0);
  check_seen("control: no result name", 5);
  precondition(0);
  check_seen("control: precondition", 6);

  S s;
  s.mem(0);
  check_seen("control: member function", 7);

  templated(0);
  check_seen("control: function template", 8);
  templated(0.5);
  check_seen("control: template, second instantiation", 8);
  templated(0L);
  check_seen("control: template, third instantiation", 8);

  if (failures)
    __builtin_abort();
  return 0;
}
