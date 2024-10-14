// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts

struct Foo {
  int x = 1;
  int y = 2;
  int z = 3;

};
constexpr Foo f(Foo x) post(r : r.x != 0) { // expected-error {{contract failed during execution of constexpr function}}
  return x;
}

constexpr int do_test() {
  f(Foo{1, 2, 3}); // OK
  f(Foo{0, 2, 3});  // expected-note {{in call to 'f({0, 2, 3})'}}
  return 42;
}

constexpr int test = do_test(); // expected-error {{constexpr variable 'test' must be initialized by a constant expression}}
  // expected-note@-1 {{in call to 'do_test()'}}

constexpr int f2(int x) post(r : r != 0) { // expected-error {{contract failed during execution of constexpr function}}
  return x;
}
constexpr int test3 = f2(1); // OK
constexpr int test2 = f2(0); // expected-error {{constexpr variable 'test2' must be initialized by a constant expression}}
  // expected-note@-1 {{in call to 'f2(0)'}}