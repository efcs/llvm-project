// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts

struct Foo {
  int f(int xyy) pre(xy != 0) { // expected-error {{use of undeclared identifier 'xy'}}
    return xyy;
  }
  int g(int x, int y = 42) pre(x != y);
};

int Foo::g(int x, int y)  pre(x != y) {
  return x;
}

