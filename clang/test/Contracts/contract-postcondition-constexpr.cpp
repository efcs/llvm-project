// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// Postcondition result binding during constant evaluation: a named-result
// postcondition (post(r: ... r ...)) evaluated at compile time binds the result
// variable to the function's return value.  (GCC mirror:
// g++.dg/contracts/cpp26/contract-postcondition-constexpr.C.)  Clang handles
// this correctly; the failing-predicate case is a proper CE violation.

constexpr int inc (const int x) post (r: r == x + 1) { return x + 1; }
static_assert (inc (4) == 5);

constexpr int identity (int x) post (r: r >= 0) { return x; }
static_assert (identity (7) == 7);

// A false postcondition is a proper constant-evaluation violation, reported at
// the contract (not "condition is not constant").
constexpr int bad (int x)
  post (r: r > 100) // expected-error {{contract failed during execution of constexpr function}}
{ return x; }
constexpr int z = bad (5); // expected-error {{must be initialized by a constant expression}} expected-note {{in call to 'bad(5)'}}
