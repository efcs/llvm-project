// RUN: %clang_cc1 -std=c++26 -fcontracts  -fcolor-diagnostics -fsyntax-only -verify %s
extern int yy;
auto tmpl_lambda = [](auto p) {
  int local = 202 + p;

  return [&](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert(p2 && p && local); // expected-note 2 {{contract context}} expected-note 2 {{required here}}
    return p2;
  }(p);

}(yy); // expected-note {{here}}
