// RUN: %clang_cc1 -std=c++26 -fcontracts  -fsyntax-only -verify %s

// Smoke test: a contract_assert inside a generic lambda, naming a local and
// the lambda's own parameter.  Small on purpose -- it is the first thing to
// check when contract parsing inside a lambda regresses.
extern int yy;
auto tmpl_lambda = [](auto p) {
  int local = 202 + p;

  return [&](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert(p2 && p && local); // expected-note 2 {{contract context}} expected-note 2 {{required here}}
    return p2;
  }(p);

}(yy); // expected-note {{here}}
