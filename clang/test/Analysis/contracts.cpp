// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -analyzer-checker=core,debug.ExprInspection -verify=expected,enforce %s
// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=quick_enforce -analyzer-checker=core,debug.ExprInspection -verify=expected,enforce %s
// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -analyzer-checker=core,debug.ExprInspection -verify=expected,observe %s
// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -analyzer-checker=core,debug.ExprInspection -verify=expected,ignore %s

void clang_analyzer_eval(bool);
void clang_analyzer_warnIfReached();

// Analysis continues past a contract_assert.
void continues_past_assert(int *p) {
  contract_assert(p != nullptr);
  int *q = nullptr;
  *q = 1; // expected-warning {{Dereference of null pointer}}
}

// Under enforce/quick_enforce the predicate is assumed to hold afterwards.
// Otherwise neither outcome is excluded (both are reported because of eager
// assumption of the comparison).
void assert_is_assumed(int x) {
  contract_assert(x > 0);
  clang_analyzer_eval(x > 0); // enforce-warning {{TRUE}} \
                              // observe-warning {{TRUE}} observe-warning {{FALSE}} \
                              // ignore-warning {{TRUE}} ignore-warning {{FALSE}}
}

// A definitely violated contract terminates the program unless it is only
// observed or ignored.
void violated_assert() {
  int x = 0;
  contract_assert(x != 0);
  clang_analyzer_warnIfReached(); // observe-warning {{REACHABLE}} \
                                  // ignore-warning {{REACHABLE}}
}

// The predicate of an ignored contract is not evaluated.
int g;
bool touch() { g = 1; return true; }
void ignored_not_evaluated() {
  g = 0;
  contract_assert(touch());
  clang_analyzer_eval(g == 0); // enforce-warning {{FALSE}} \
                               // observe-warning {{FALSE}} \
                               // ignore-warning {{TRUE}}
}

// Preconditions are assumed on entry to the function.
int pre_assumed(int x) pre(x > 0) {
  clang_analyzer_eval(x > 0); // enforce-warning {{TRUE}} \
                              // observe-warning {{TRUE}} observe-warning {{FALSE}} \
                              // ignore-warning {{TRUE}} ignore-warning {{FALSE}}
  return x;
}

// ... including when the function is inlined into a caller.
int pre_inlined(int x) pre(x != 0) { return x; }
void call_violating_pre() {
  pre_inlined(0);
  clang_analyzer_warnIfReached(); // observe-warning {{REACHABLE}} \
                                  // ignore-warning {{REACHABLE}}
}

struct S {
  int m;
  S(int v) pre(v > 0) : m(v) {
    clang_analyzer_eval(m > 0); // enforce-warning {{TRUE}} \
                                // observe-warning {{TRUE}} observe-warning {{FALSE}} \
                                // ignore-warning {{TRUE}} ignore-warning {{FALSE}}
  }
};
