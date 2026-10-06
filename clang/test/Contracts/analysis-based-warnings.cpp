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
