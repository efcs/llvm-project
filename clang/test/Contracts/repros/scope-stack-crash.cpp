// RUN: %contracts_verify_test

extern int yy;
void foo(auto p) {
  int local = 202 + p;

  [&](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert(p2 && p && local); // expected-note 2 {{contract context}} expected-note 2 {{required here}}
    return p2;
  }(p);

}

int x = (foo(42), 1); // expected-note {{here}}