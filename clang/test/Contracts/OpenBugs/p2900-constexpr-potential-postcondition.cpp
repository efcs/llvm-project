// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce \
// RUN:   -Winvalid-constexpr -fsyntax-only -verify=common,checked %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -Winvalid-constexpr -fsyntax-only -verify=common,checked %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore \
// RUN:   -Winvalid-constexpr -fsyntax-only -verify=common %s
// XFAIL: *
//
// OPEN BUG (constant evaluation, diagnostic quality -- not a conformance
// requirement): -Winvalid-constexpr does not see a contract assertion whose
// predicate can never be constant-evaluated.
//   Expected: "constexpr function never produces a constant expression" for
//     bad_pre, bad_post and mixed under the enforce/observe RUN lines.
//   Actual: only bad_body is diagnosed.
//
// Round three (deliberately left open): Expr::isPotentialConstantExpr does
// not evaluate contract assertions at all (State::shouldCheckContract), since
// evaluating a contract assertion never makes an expression ineligible to be
// a core constant expression (P2900R14 3.5.12). Diagnosing a predicate that
// can never be constant would need to distinguish it from one that merely
// reads a parameter or the result name with an unknown value in that mode,
// in both evaluators. It would also only be right for a terminating
// semantic: with observe, such a predicate is diagnosed at each call but the
// call still produces a constant expression, so the `observe` RUN lines
// below expect more than [basic.contract.eval]p8 supports.

// Regression pin for CLANG-22: a contract predicate that can never be
// constant-evaluated makes the call unusable in a constant expression, and
// Expr::isPotentialConstantExpr should see that for a POSTcondition as it
// already did for a precondition.  EvaluatePostContracts used to return false
// for the whole sequence in this mode, so the identical predicate was
// diagnosed as a `pre` and silent as a `post`.
//
// Scope, so this test is not read as more than it is:
//
//   * -Winvalid-constexpr is required.  Since P2448 the underlying rule is
//     gone in C++23 and later, so Clang skips the check entirely at those
//     dialects unless the warning is asked for.  This is a lint, not a
//     conformance gate.
//   * The semantic is pinned on every RUN line, and the split is at `ignore`,
//     not at `enforce`.  What matters is whether the predicate is EVALUATED
//     during constant evaluation, not whether a failure stops the program: a
//     predicate that cannot be constant-evaluated makes the call non-constant
//     under `observe` exactly as under `enforce`, because you have to evaluate
//     it either way to know whether to report.  Only `ignore` does not
//     evaluate it, and only `ignore` is therefore clean.  `common` is what
//     holds at every semantic; `checked` is what the predicates add wherever
//     they are evaluated.
//   * **A postcondition is only reached when the body evaluates.**  See the
//     "still not reached" group below -- that residue is what keeps CLANG-22
//     open, and it is why the pins here take functions with no parameters.
//
// Deliberately NOT mirrored on the GCC side: GCC does not run contract
// predicates through its potential-constant-expression check at all, so this
// widens an existing divergence rather than closing one.  Both are conforming
// -- since P2448 there is no rule left to violate.

int global = 1;
// common-note@-1 1 {{declared here}}
// checked-note@-2 3 {{declared here}}

// -- Control: a non-constant body, no contracts.  Fires at every semantic. --

constexpr int bad_body(int x) { return global + x; }
// common-error@-1 {{constexpr function never produces a constant expression}}
// common-note@-2 {{read of non-const variable 'global' is not allowed in a constant expression}}

// -- Control: the same predicate as a precondition. ------------------------
// A precondition is evaluated before the body, so it is reached whether or not
// the body can be evaluated with unknown arguments.

constexpr int bad_pre(int x) pre(global > 0) { return x; }
// checked-error@-1 {{constexpr function never produces a constant expression}}
// checked-note@-2 {{read of non-const variable 'global' is not allowed in a constant expression}}

// -- The pin: a postcondition on a body that does evaluate. ----------------

constexpr int bad_post() post(global > 0) { return 1; }
// checked-error@-1 {{constexpr function never produces a constant expression}}
// checked-note@-2 {{read of non-const variable 'global' is not allowed in a constant expression}}

// The skip below is per-contract, not per-sequence: the result-name
// postcondition is skipped and the hopeless one is still checked.
constexpr int mixed() post(r: r > 0) post(global > 0) { return 1; }
// checked-error@-1 {{constexpr function never produces a constant expression}}
// checked-note@-2 {{read of non-const variable 'global' is not allowed in a constant expression}}

// -- A postcondition that binds the result name is skipped. ----------------
//
// In this mode the body ran with unknown arguments, so there is no return
// value for the result name to bind to; evaluating the predicate would read
// an unset value.  No diagnostic, even though `r > 0` cannot be decided.

constexpr int result_name() post(r: r > 0) { return 1; }

// -- Still not reached: the residue that keeps CLANG-22 open. --------------
//
// isPotentialConstantExpr calls the function with no arguments, so a body that
// reads a parameter fails to evaluate, and HandleFunctionCall only runs the
// postconditions on ESR_Succeeded / ESR_Returned.  The predicate below is the
// same hopeless `global > 0` as bad_post above and is NOT diagnosed, purely
// because the postcondition is never reached.  Closing this needs the
// predicates checked outside the call flow, which is a design change and not
// a local fix.

constexpr int bad_post_unreached(int x) post(global > 0) { return x; }

// -- Contracts that are perfectly constant: never diagnosed. ---------------
// The parameter must be const to be named in a postcondition
// ([dcl.contract.func]).

constexpr int fine_pre(int x) pre(x > 0) { return x; }
constexpr int fine_post(const int x) post(r: r == x) { return x; }
constexpr int fine_both(const int x) pre(x > 0) post(r: r == x) { return x; }
