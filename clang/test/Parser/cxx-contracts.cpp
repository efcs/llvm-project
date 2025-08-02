// RUN: %clang_cc1 -std=c++26 -fcontracts %s -verify

// expected-no-diagnostics

int f(const int x) pre(x != 0)
             post(r : x > 0) {
  return x;
}

struct Foo {
    int x;
    Foo(const int x) pre(x != 0 && this->x != 0) : x(x) {}
    int get() post(this->x > 0) {
      return x;
    }
};

auto lam = [](const int x) pre(x != 0) post(r : x > 0) { return x; };