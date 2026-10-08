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

// Postconditions are evaluated when the function returns, after the return
// value is computed, and the result name refers to the returned value.
int post_result(int x) post(r : r > 0) {
  return x;
}
void call_post_result(int x) {
  int v = post_result(x);
  clang_analyzer_eval(v > 0); // enforce-warning {{TRUE}} \
                              // observe-warning {{TRUE}} observe-warning {{FALSE}} \
                              // ignore-warning {{TRUE}} ignore-warning {{FALSE}}
  clang_analyzer_eval(v == x); // expected-warning {{TRUE}}
}
int post_returns_arg(const int x) post(r : r == x) {
  return x;
}
void call_post_returns_arg(int y) {
  int v = post_returns_arg(y);
  clang_analyzer_eval(v == y); // expected-warning {{TRUE}}
}
void call_violating_post() {
  post_result(0);
  clang_analyzer_warnIfReached(); // observe-warning {{REACHABLE}} \
                                  // ignore-warning {{REACHABLE}}
}

// ... on every return statement, and when control flows off the end.
void post_on_global(bool b) post(g > 0) {
  if (b)
    return;
  g = 2;
}
void call_post_on_global() {
  g = 1;
  post_on_global(true);
  clang_analyzer_eval(g > 0); // enforce-warning {{TRUE}} \
                              // observe-warning {{TRUE}} \
                              // ignore-warning {{TRUE}}
  g = 0;
  post_on_global(true);
  clang_analyzer_warnIfReached(); // observe-warning {{REACHABLE}} \
                                  // ignore-warning {{REACHABLE}}
}
void call_post_on_global_falls_off() {
  g = 0;
  post_on_global(false);
  clang_analyzer_eval(g == 2); // expected-warning {{TRUE}}
}

// A result name of class type refers to the returned object.
struct P { int a; int b; };
P make_p(int v) post(r : r.a > 0 && r.b == 0) {
  return P{v, 0};
}
void call_make_p(int v) {
  P p = make_p(v);
  clang_analyzer_eval(p.a > 0); // enforce-warning {{TRUE}} \
                                // observe-warning {{TRUE}} observe-warning {{FALSE}} \
                                // ignore-warning {{TRUE}} ignore-warning {{FALSE}}
  clang_analyzer_eval(p.b == 0); // expected-warning {{TRUE}}
}

// A result name of reference type refers to the referenced object.
int &ref_result(int &x) post(r : r > 0) {
  return x;
}
void call_ref_result(int &y) {
  int &v = ref_result(y);
  clang_analyzer_eval(v > 0); // enforce-warning {{TRUE}} \
                              // observe-warning {{TRUE}} observe-warning {{FALSE}} \
                              // ignore-warning {{TRUE}} ignore-warning {{FALSE}}
  clang_analyzer_eval(y > 0); // enforce-warning {{TRUE}} \
                              // observe-warning {{TRUE}} observe-warning {{FALSE}} \
                              // ignore-warning {{TRUE}} ignore-warning {{FALSE}}
}

// The predicate of an ignored postcondition is not evaluated either.
int ignored_post(int x) post(touch()) {
  return x;
}
void call_ignored_post() {
  g = 0;
  ignored_post(1);
  clang_analyzer_eval(g == 0); // enforce-warning {{FALSE}} \
                               // observe-warning {{FALSE}} \
                               // ignore-warning {{TRUE}}
}

// Under observe, the eagerly assumed comparison of an observed predicate leaves
// a path on which it is false, which the analysis continues on (the function
// does continue after a violation under observe); see OPEN_QUESTIONS.md 1.11.
// The report is issued once, on the first dereference.
int deref_after_pre(int *p) pre(p != nullptr) {
  return *p; // observe-warning {{Dereference of null pointer (loaded from variable 'p')}}
}
int deref_after_pre_twice(int *p) pre(p != nullptr) {
  int v = *p; // observe-warning {{Dereference of null pointer (loaded from variable 'p')}}
  return v + *p;
}
int deref_after_assert(int *p) {
  contract_assert(p != nullptr);
  return *p; // observe-warning {{Dereference of null pointer (loaded from variable 'p')}}
}
// The result name of a postcondition refers to the result object, so the
// assumption on it does not affect the caller's pointer either way.
int *post_not_null(int *p) post(r : r != nullptr) {
  return p;
}
int deref_post_result(int *p) {
  return *post_not_null(p);
}

// The postconditions of a coroutine are evaluated (in the ramp), and the CFG
// reaches them through the co_return statements. The analyzer does not model
// coroutines yet (it stops at the co_return statement), so it does not reach
// them under any semantic; it does not crash on them either.
namespace std {
template <class R, class...> struct coroutine_traits {
  using promise_type = typename R::promise_type;
};
template <class P = void> struct coroutine_handle {
  coroutine_handle() = default;
  template <class Q> coroutine_handle(coroutine_handle<Q>);
  static coroutine_handle from_address(void *);
};
struct suspend_never {
  bool await_ready() noexcept;
  void await_suspend(coroutine_handle<>) noexcept;
  void await_resume() noexcept;
};
} // namespace std
struct Task {
  struct promise_type {
    Task get_return_object();
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void return_value(int);
    void unhandled_exception();
  };
  bool valid() const;
};
Task co_post(const int x) post(r : (clang_analyzer_warnIfReached(), true)) {
  co_return x;
}
Task co_post_result(const int x) post(r : r.valid()) {
  co_return x;
}
