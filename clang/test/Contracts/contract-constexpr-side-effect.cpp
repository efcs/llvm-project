// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce -fsyntax-only -verify %s
// expected-no-diagnostics
//
// (Only the constant-evaluation half of the original test is kept here; the
// run-time half belongs with the Runnable tests.)

// A contract predicate that modifies an object of the enclosing constant
// evaluation is still a core constant expression.
//
// [basic.contract.eval] says an evaluation using a checking semantic
// "determines the value of the predicate", that "it is unspecified whether the
// predicate is evaluated", and that an alternative evaluation "that produces
// the same value as the predicate but has no side effects can occur" -- can,
// not must.  A contract violation on constant-evaluation grounds occurs only
// when the predicate "is not a core constant expression".  Modifying an object
// whose lifetime began within the enclosing evaluation is fine in one, so this
// is all well-formed.
//
// Mirror of gnu_gcc's contract-constexpr-side-effect.C.  GCC evaluated the
// predicate under a tracker that refuses such a modification and then read the
// refusal as "not a core constant expression", rejecting these programs with
// "contract condition is not constant"; it now falls back to a real evaluation.
// Clang always accepted them, and this test pins the two compilers to the same
// answers -- including that the modification is visible afterwards, which is
// unspecified but is what both now do.

constexpr bool bump(unsigned *p) {
  *p += 1;
  return true;
}

// The predicate modifies an object belonging to the caller's evaluation.
constexpr unsigned one_side_effect(unsigned *p) {
  contract_assert(bump(p));
  return *p;
}

constexpr unsigned run_one() {
  unsigned x = 1;
  return one_side_effect(&x);
}

static_assert(run_one() == 2);

// A precondition doing the same, reached twice, so this is not a one-shot.
constexpr unsigned pre_side_effect(unsigned *p) pre(bump(p)) { return *p; }

constexpr unsigned run_pre() {
  unsigned x = 0;
  unsigned a = pre_side_effect(&x);
  unsigned b = pre_side_effect(&x);
  return a * 10 + b;
}

static_assert(run_pre() == 12);

// Modification and a genuine read of enclosing state together.
struct counter {
  unsigned hits;
};

constexpr bool touch(counter *c, unsigned limit) {
  c->hits += 1;
  return c->hits <= limit;
}

constexpr unsigned guarded(counter *c) {
  contract_assert(touch(c, 10));
  return c->hits;
}

constexpr unsigned run_guarded() {
  counter c{0};
  guarded(&c);
  guarded(&c);
  return guarded(&c);
}

static_assert(run_guarded() == 3);
