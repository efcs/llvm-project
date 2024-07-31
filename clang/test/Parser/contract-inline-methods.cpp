// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts

struct Foo {
  int f(int x) pre(xy != 0) { // expected-error {{use of undeclared identifier 'xy'}}
    return x;
  }
  int g(int x) pre(x != 0);
};

int Foo::g(int x) pre(x != 0) {
  return x;
}