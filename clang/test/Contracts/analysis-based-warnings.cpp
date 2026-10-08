// The analysis-based warnings (which work on the CFG) do not depend on the
// evaluation semantic selected for the contract assertions: the predicate of
// a contract assertion is always part of the CFG, whether or not it is
// evaluated at run time.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -Wall -Wthread-safety -Wunreachable-code %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -Wall -Wthread-safety -Wunreachable-code -fcontract-evaluation-semantic=ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -Wall -Wthread-safety -Wunreachable-code -fcontract-evaluation-semantic=observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -Wall -Wthread-safety -Wunreachable-code -fcontract-evaluation-semantic=quick_enforce %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -Wall -Wthread-safety -Wunreachable-code -fcontract-group-evaluation-semantic=grp=ignore %s

[[noreturn]] bool fail();
bool check(int);

// -Wreturn-type: a predicate which calls a [[noreturn]] function ends the
// path, like any other call. (-Wunreachable-code then reports the contract
// check itself, which follows the call.)
int noreturn_in_assert(int x) {
  if (x)
    return 1;
  contract_assert(fail()); // expected-warning {{code will never be executed}}
}
int noreturn_in_pre(int x) pre(fail()) { // expected-warning {{code will never be executed}}
  if (x)
    return 1;
}
int noreturn_in_group(int x) {
  if (x)
    return 1;
  contract_assert [[clang::contract_group("grp")]] (fail()); // expected-warning {{code will never be executed}}
}
int falls_off(int x) {
  contract_assert(check(x));
  if (x)
    return 1;
} // expected-warning {{non-void function does not return a value in all control paths}}
int falls_off_with_post(int x) post(r : r > 0) {
  if (x)
    return 1;
} // expected-warning {{non-void function does not return a value in all control paths}}
int returns_with_post(int x) post(r : r > 0) {
  if (x)
    return 1;
  return 2;
}

// -Wunreachable-code: the code after a predicate which does not return is
// unreachable.
void unreachable_after_pre(int x) pre(fail()) { // expected-warning {{code will never be executed}}
  check(x);
}
// The postconditions of a function which never returns normally are not
// reported as unreachable code (they are not written at that point of the
// function), and a contract_assert after a statement which never completes
// is reported like any other statement.
int post_never_returns_loop(int x) post(r : r > 0) {
  for (;;) {
  }
}
int post_never_returns_loop2(int x) post(r : r > 0) {
  while (true) {
    if (x)
      continue;
  }
}
int post_never_returns_noreturn(int x) post(r : r > 0) {
  fail();
}
int post_never_returns_pre(int x) pre(fail()) post(r : r > 0) { // expected-warning {{code will never be executed}}
  return x;
}
int assert_after_return(int x) {
  return x;
  contract_assert(check(x)); // expected-warning {{code will never be executed}}
}
int assert_after_loop(int x) post(r : r > 0) {
  for (;;) {
  }
  contract_assert(check(x)); // expected-warning {{code will never be executed}}
}

// The postconditions of a coroutine are reachable through its co_return
// statements (they are evaluated when the ramp returns), so they are not
// reported as unreachable code either.
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
struct VoidTask {
  struct promise_type {
    VoidTask get_return_object();
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void return_void();
    void unhandled_exception();
  };
};
Task co_post(const int x) pre(x > 0) post(r : r.valid()) {
  co_return x;
}
int gl;
VoidTask co_post_void(int x) post(check(gl)) {
  if (x)
    co_return;
}
VoidTask co_post_never_returns(int x) post(check(gl)) {
  for (;;) {
  }
}

// -Wuninitialized: the predicate uses the variables like any other expression.
int uninit_in_assert() {
  int v; // expected-note {{initialize the variable 'v' to silence this warning}}
  contract_assert(v > 0); // expected-warning {{variable 'v' is uninitialized when used here}}
  return 0;
}
int uninit_in_post() post(r : r > 0) {
  int v; // expected-note {{initialize the variable 'v' to silence this warning}}
  return v; // expected-warning {{variable 'v' is uninitialized when used here}}
}

// -Wthread-safety: capabilities asserted or used in a predicate are checked
// and tracked like in any other expression.
struct __attribute__((capability("mutex"))) Mutex {
  void lock() __attribute__((acquire_capability()));
  void unlock() __attribute__((release_capability()));
  void assert_held() const __attribute__((assert_capability(this)));
};
Mutex mu;
int guarded __attribute__((guarded_by(mu)));

void reads_guarded_in_assert() {
  contract_assert(guarded > 0); // expected-warning {{reading variable 'guarded' requires holding mutex 'mu'}}
}
void reads_guarded_in_pre() pre(guarded > 0) { // expected-warning {{reading variable 'guarded' requires holding mutex 'mu'}}
}
void asserts_held_in_assert() {
  contract_assert((mu.assert_held(), true));
  (void)guarded;
}

// (An error stops the analysis-based warnings for the rest of the file.)
int init_in_assert() {
  int v;
  contract_assert((v = 1, true)); // expected-error {{cannot assign to variable 'v' because it is considered 'const' inside of a contract}}
  return v;
}
