// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A result-name introducer is only meaningful on a postcondition.  On a
// precondition it is parsed and rejected with a dedicated diagnostic; in a
// contract_assert the identifier falls through to the predicate and is
// rejected by the expression parser instead.

int f(int x) pre(r: r > 0) { return x; }
// expected-error@-1 {{result name 'r' not allowed outside of post condition specifier}}

void g(int x) {
  contract_assert(r: r > 0);
  // expected-error@-1 {{use of undeclared identifier 'r'}}
  // expected-error@-2 {{expected ')'}}
  // expected-note@-3 {{to match this '('}}
}

// A result name on a void-returning function is separately diagnosed.
void h() post(r: r > 0) {}
// expected-error@-1 {{result name 'r' cannot be used with a void return type}}

// The valid case still works.
int ok(int x) post(r: r > 0) { return x; }
