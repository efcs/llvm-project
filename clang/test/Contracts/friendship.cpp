// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcolor-diagnostics -verify  %s

namespace ContractIsFriend {

class A {
  friend void f(A &a) pre(a.g()) post(a.g());
  bool g() const; // expected-note 1 {{private here}}
public:
  A();
};
void f(A &a) pre(a.g()) post(a.g()) {
  a.g(); // OK
}
void u(A &a) pre(a.g()) { // expected-error {{'g' is a private member}}
}

} // end namespace ContractIsFriend

namespace FriendClass {

class B;

class A {
  friend class B;
  bool g() const;

public:
  A();
};

class B {
  static void f(A &a) pre(a.g()) post(a.g()) {
    a.g(); // OK
    contract_assert(a.g());
  }

  void u(A &a) pre(a.g()) post(a.g());
};

void B::u(A &a) pre(a.g()) post(a.g()) {} // Also OK

} // end namespace FriendClass

namespace TemplateFriend {

template <class T, class U = T> struct B {
  void f(T &a) pre(a.g()) // expected-error {{'g' is a private member}}
      post(a.g())         // expected-error {{'g' is a private member}}
  {}
};
class A {
  friend struct B<A, int>;
  bool g() const; // expected-note 2 {{private here}}
};

template struct B<A, int>;  // OK
template struct B<A, long>; // expected-note {{here}}

} // end namespace TemplateFriend
