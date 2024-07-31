// RUN: %clang_cc1 -std=c++26 -fcontracts %s -verify


int f(int x) pre(x != 0)
             post(r : x > 0) {
  return x;
}

struct Foo {
    int x;
    Foo(int x) pre(x != 0 && this->x != 0) : x(x) {}
    int get() post(this->x > 0) {
      return x;
    }
};

auto lam = [](int x) pre(x != 0) post(r : x > 0) { return x; }; // expected-error {{contract feature Undeduced Auto Result Name is not yet implemented}}
