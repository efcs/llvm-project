// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts

struct Foo {
  int x = 1;
  int y = 2;
  int z = 3;

};
constexpr Foo f(Foo x) post(r : r.x != 0) {
  return x;
}

constexpr int do_test() {
  f(Foo{1, 2, 3}); // OK
  f(Foo{0, 2, 3}); // Not OK
  return 42;
}

constexpr int test = do_test();

constexpr int f2(int x) post(r : r != 0) {
  return x;
}

constexpr int test2 = f2(1);
