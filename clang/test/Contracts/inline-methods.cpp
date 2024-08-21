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

namespace refer_to_later_decl {
struct X {
  void f(int x) pre(this->y);
  bool y;
};

void f() {
  X x;
  x.f(0); // OK
}
} // namespace refer_to_later_decl

namespace param_and_member_with_same_name {
struct Y {
  static bool value;
};

struct X {
  void f(Y x) pre(x.value);
  bool x;
};

void f() {
  X x;
  x.f(Y{}); // OK
}

} // namespace param_and_member_with_same_name
