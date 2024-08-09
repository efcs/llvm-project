// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts


namespace test_one {
constexpr void f() {
  constexpr int z = 42;
  constexpr int zz = 101;
  constexpr auto y = [=, yyy=z](int x)
    -> int
            pre(x == 1)
            post(r : r == 2) // expected-error {{contract failed during execution of constexpr function}}
            pre(z == 42 && yyy == 42)
            { return 1; }; 
  static_assert(y(1)); // expected-error {{static assertion expression is not an integral constant expression}}
  // expected-note@-1 {{in call to 'y.operator()(1)'}}
}

}

namespace test_two {
  void f(int x) {
    (void)[&]() pre(x) { }; // expected-error {{}}
    (void)[&]() pre(x) {((void)x); };
  }
}