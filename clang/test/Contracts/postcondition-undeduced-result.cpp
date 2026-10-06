// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// Watch test for a bug that is OPEN ON UPSTREAM GCC and correct here: a
// postcondition with a result name, on a function whose return type deduction
// never completes, aborts g++ instead of reporting the error and stopping.
//
//   internal compiler error: in check_noexcept_r, at cp/except.cc:1063
//
// There the predicate of a result-named postcondition is parsed as a template
// tree, because the result variable's type is `auto` while it is parsed, and
// is substituted later from the deduced return type.  When deduction fails
// that substitution never happens, and genericizing the still-unresolved call
// trips an assertion.  Clang reaches the same situation and diagnoses it, so
// this file is NOT xfailed: it pins that it keeps doing so.
//
// The MIRROR is
// gcc/testsuite/g++.dg/contracts/cpp26/postcondition-undeduced-result.C,
// tracked as GCC-49 in that fork's bug-reports/ and upstream as PR127450.
// That fork has now FIXED it -- its version of this file is an ordinary
// regression test rather than an xfail, and carries six further shapes that
// could not be written while the abort was still ending the translation unit
// at the first one.  Upstream GCC still aborts, which is why the row survives
// in its bug-reports/ and why this comment still describes a live defect.

bool check(bool b) { return b; }

// Shape 1, the one PR127450 reports: deduction fails because the returned
// expression is ill-formed.
//
// The second diagnostic is error recovery -- with nothing deduced the return
// type falls back to void, and a result name on a void return is separately
// ill-formed.  It is recorded because -verify demands every diagnostic be
// accounted for, not because the cascade is required behaviour.
class S {
  // expected-error@+2 {{use of undeclared identifier 'e'}}
  // expected-error@+1 {{result name 'r' cannot be used with a void return type}}
  auto f() post(r: check(r)) { return e; }
};

// Shape 2: deduction fails with no undeclared name anywhere, which is what
// shows the trigger is "deduction never completed" rather than "the body
// mentioned something undeclared".
bool g();
// expected-error@+2 {{function 'h' with deduced return type cannot be used before it is defined}}
// expected-note@+1 {{'h' declared here}}
auto h() post(r: g()) { return h(); }
