// Once the returned object has been initialized, unwinding must destroy it --
// exactly once.  This test covers the contracts route to that; see the note at
// the bottom for the core-language route, which Clang still gets wrong.
//
// A contract-violation handler that throws out of a POSTcondition check.
// [stmt.return]/5 sequences postcondition evaluation after the destruction of
// local variables, so the returned object exists by then.  No wording requires
// its destruction -- [except.ctor]/2 names only temporaries and local
// variables, and [basic.contract.eval] says the behaviour is "as if the
// function body exits via that same exception", describing a state in which
// the result object was never initialized.  We destroy it anyway; leaking an
// object the program can no longer reach is not a defensible reading.
//
// A core issue has been filed for this at
// <https://github.com/cplusplus/cwg/issues/988> (it has no CWG issue number
// yet), proposing that [except.ctor]/2 be extended to cover an exception
// escaping the evaluation of a postcondition assertion.  Until that resolves,
// this test deliberately runs ahead of the wording: do NOT "fix" it back to
// expecting a leak on the strength of the wording as it stands.
//
// GCC does the same, via g++.dg/contracts/cpp26/contract-retval-destroyed-on-
// unwind.C, so the two compilers agree here.
//
// Every case counts constructions against destructions, so a leak and a double
// destroy both fail.
//
// STILL BROKEN, DELIBERATELY NOT COVERED HERE: the core-language case, where
// an ordinary local variable's destructor throws after the returned object has
// been initialized and no contract is involved at all.  [except.ctor]/2
// requires the returned object to be destroyed then -- "If an exception is
// thrown during the destruction of temporaries or local variables for a return
// statement, the destructor for the returned object (if any) is also invoked"
// -- and Clang leaks it, in plain C++17.  GCC handles it, via
// current_retval_sentinel.  Fixing it needs a cleanup live across the whole
// body, which today would make requiresLandingPad() true from function entry
// and turn every potentially-throwing call into an invoke; that wants a gate
// on the body containing a potentially-throwing destructor first.  Tracked as
// a separate core-language bug with its own reproducer.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts \
// RUN:   -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

#include <contracts>

int live = 0;
int destroyed = 0;

struct Counted {
  int v;
  Counted(int x) : v(x) { ++live; }
  Counted(const Counted &o) : v(o.v) { ++live; }
  ~Counted() { --live; ++destroyed; }
};

struct Trivial { int v; };

struct E {};

bool handler_should_throw = false;

void handle_contract_violation(const std::contracts::contract_violation &) {
  if (handler_should_throw)
    throw E{};
}

// --- a postcondition handler throws ----------------------------------------

Counted post_throws_nrvo(int n) post(r : r.v > 100) {
  Counted result(n);
  return result;
}

Counted post_throws_no_nrvo(int n) post(r : r.v > 100) {
  Counted a(n), b(n + 1);
  return n > 0 ? a : b;
}

// A failing PREcondition must destroy nothing: the object does not exist yet.
Counted pre_throws(int n) pre(n > 100) {
  Counted result(n);
  return result;
}

// --- controls ---------------------------------------------------------------

Counted post_passes(const int n) post(r : r.v == n) {
  Counted result(n);
  return result;
}

Trivial trivial_return(int n) post(r : r.v > 100) {
  Trivial t{n};
  return t;
}

void void_return(const int n) post(n > 100) {}

// An exception BEFORE the return statement: the local still owns the object,
// so it must be destroyed exactly once by its own cleanup and the return-value
// cleanup must stay disarmed.
Counted throws_before_return(bool arm) {
  Counted result(1);
  if (arm)
    throw E{};
  return result;
}

static void check(int got, int want) {
  if (got != want)
    __builtin_abort();
}

template <class F> static void expect_throw_balanced(F f) {
  live = 0;
  destroyed = 0;
  bool threw = false;
  try { f(); } catch (E &) { threw = true; }
  if (!threw)
    __builtin_abort();
  check(live, 0);                       // no leak, and no double destroy
  if (destroyed == 0)
    __builtin_abort();
}

int main() {
  // An exception raised before the return statement: the local still owns the
  // object and destroys it; the return-value cleanup must stay disarmed.
  handler_should_throw = false;
  expect_throw_balanced([] { throws_before_return(true); });

  // A throwing violation handler on a postcondition.
  handler_should_throw = true;
  expect_throw_balanced([] { post_throws_nrvo(1); });
  expect_throw_balanced([] { post_throws_no_nrvo(1); });

  // A failing precondition: nothing exists yet, so nothing may be destroyed.
  live = 0;
  destroyed = 0;
  bool threw = false;
  try { pre_throws(1); } catch (E &) { threw = true; }
  if (!threw)
    __builtin_abort();
  check(live, 0);
  check(destroyed, 0);

  // Normal completion destroys exactly once, in the caller -- not early, and
  // not twice.  This is what pins the cleanup as EH-only.
  handler_should_throw = false;
  live = 0;
  destroyed = 0;
  {
    Counted got = post_passes(7);
    check(got.v, 7);
    check(live, 1);
    check(destroyed, 0);
  }
  check(live, 0);
  check(destroyed, 1);

  live = 0;
  destroyed = 0;
  check(throws_before_return(false).v, 1);
  check(live, 0);
  check(destroyed, 1);

  // A trivially-destructible return type and a void return must not grow a
  // cleanup; both take the same throwing-handler path.
  handler_should_throw = true;
  threw = false;
  try { trivial_return(1); } catch (E &) { threw = true; }
  if (!threw)
    __builtin_abort();
  threw = false;
  try { void_return(1); } catch (E &) { threw = true; }
  if (!threw)
    __builtin_abort();

  return 0;
}
