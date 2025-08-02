// RUN: %clang_cc1 -std=c++26 -fsyntax-only -fcolor-diagnostics -verify %s -fcontracts ||  %clang_cc1 -std=c++26 -fsyntax-only -fcolor-diagnostics  %s -fcontracts ||  %clang_cc1 -std=c++26 -fsyntax-only -verify -fcolor-diagnostics  %s -fcontracts


namespace DoubleDeep {

void foo(int a);
void foo(int a) {
  int z1 = 0;
  [&](int y) {
    ++y;
    int z2 = z1;
    [&] (int x) { // expected-error {{'a' is not allowed}}
      ++y;
      ++x;
      ++z1;
      ++z2;
      contract_assert( // expected-note {{contract}}
          [&]
              (int v) {
            ++y; // expected-error {{inside a contract}}
            ++x; // expected-error {{inside a contract}}
            ++a; // expected-error {{inside a contract}} expected-note {{required here}}
            ++v; // OK

            return [&](int w) {
              ++a; // expected-error {{inside a contract}}
              ++x; // expected-error {{inside a contract}}
              ++y; // expected-error {{inside a contract}}
              ++v; // OK
              ++w; // OK
              return true;
            }(42);
          }(12));
    }(y);
  }(a);
}
} // namespace DoubleDeep
