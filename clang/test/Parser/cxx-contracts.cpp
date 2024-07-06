// RUN: %clang_cc1 -std=c++26 -fcontracts %s -verify
// expected-no-diagnostics

int f(int x) pre(x != 0)
             post(r : x > 0) {
  return x;
}

struct Foo {
    int x;
    Foo(int x) pre(x != 0) : x(x) {}
    int get() post(this->x > 0) {
      return x;
    }
};

auto lam = [](int x) pre(x != 0) post(r : x > 0) { return x; };
