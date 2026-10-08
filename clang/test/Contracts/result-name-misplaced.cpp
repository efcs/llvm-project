// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A result-name introducer is only meaningful on a postcondition.  On a
// precondition or an assertion-statement it is parsed and rejected with a
// dedicated diagnostic, and the predicate (which may use the name) is parsed
// without further errors.

int f(int x) pre(r: r > 0) { return x; }
// expected-error@-1 {{result name 'r' is not allowed in a precondition specifier; only a postcondition specifier can introduce a result name}}

void g(int x) {
  contract_assert(r: r > 0);
  // expected-error@-1 {{result name 'r' is not allowed in a contract_assert statement; only a postcondition specifier can introduce a result name}}
  contract_assert(r [[maybe_unused]]: true);
  // expected-error@-1 {{result name 'r' is not allowed in a contract_assert statement; only a postcondition specifier can introduce a result name}}
  contract_assert(x: x > 0);
  // expected-error@-1 {{result name 'x' is not allowed in a contract_assert statement; only a postcondition specifier can introduce a result name}}
}

// A result name on a void-returning function is separately diagnosed.
void h() post(r: r > 0) {}
// expected-error@-1 {{result name 'r' cannot be used with a void return type}}

// The valid case still works.
int ok(int x) post(r: r > 0) { return x; }
