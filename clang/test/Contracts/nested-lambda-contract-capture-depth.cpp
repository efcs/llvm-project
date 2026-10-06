// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts


namespace DoubleDeep {

void foo(int a);
void foo(int a) {
  int z1 = 0;
  [&](int y) {
    ++y;
    int z2 = z1;
    [&] (int x) { // expected-error {{implicit capture of local entity 'a' is not allowed when used exclusively in contract assertions}}
      ++y;
      ++x;
      ++z1;
      ++z2;
      contract_assert( // expected-note {{contract}}
          [&]
              (int v) {
            ++y; // expected-error {{considered 'const' inside of a contract}}
            ++x; // expected-error {{considered 'const' inside of a contract}}
            ++a; // expected-error {{considered 'const' inside of a contract}} expected-note {{required here}}
            ++v; // OK

            return [&](int w) {
              ++a; // expected-error {{considered 'const' inside of a contract}}
              ++x; // expected-error {{considered 'const' inside of a contract}}
              ++y; // expected-error {{considered 'const' inside of a contract}}
              ++v; // OK
              ++w; // OK
              return true;
            }(42);
          }(12));
    }(y);
  }(a);
}
} // namespace DoubleDeep
