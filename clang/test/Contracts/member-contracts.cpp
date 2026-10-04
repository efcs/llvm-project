// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// Function contract specifiers of member functions are a complete-class
// context, in every grammar position.
template <class T> struct A {
  void f() requires true pre(m > 0) post(m > 0);
  void g() const & noexcept pre(m > 0);
  auto h() -> int pre(m > 0) post(r: r == m);
  T m;
};
template struct A<int>;

struct B { virtual void v(); };
struct C : B {
  void v() override pre(m > 0); // expected-error {{virtual function 'v' cannot have function contract specifiers}}
  int m;
};

// An attribute-specifier-seq may follow the contract keyword.
struct D {
  void f() pre [[clang::contract_group("x")]] (m > 0);
  int m;
};

// Out-of-line member definitions are parsed in the scope of the class.
struct S {
  int m;
  using T = int;
  static int s;
  int f(T x); // expected-note {{previously declared without contracts here}}
  int g(T x) pre(m > 0 && x > 0 && s > 0) post(r: r == m);
  S() pre(m >= 0);
  ~S() post(m >= 0);
};
int S::g(T x) pre(m > 0 && x > 0 && s > 0) post(r: r == m) { return m; }
S::S() pre(m >= 0) {}
S::~S() post(m >= 0) {}

// Contracts on a non-first declaration must match (none here).
int S::f(T x) pre(m > 0) { return m; } // expected-error {{method out-of-line definition differs in contract specifier sequence}}

namespace N {
int n;
void f() pre(n > 0);
}
void N::f() pre(n > 0) {}

// 'this' is not available in the contracts of a static member function.
struct St {
  int m;
  static void f() pre(m > 0); // expected-error {{invalid use of member 'm' in static member function}}
  static void g() pre(this != nullptr); // expected-error {{invalid use of 'this' outside of a non-static member function}}
};

// Access checking happens in the context of the function.
class X {
  int m; // expected-note {{declared private here}}
public:
  void f() pre(m > 0);
  friend void g(X x) pre(x.m > 0);
};
void g(X x) pre(x.m > 0) {}
void h(X x) pre(x.m > 0); // expected-error {{'m' is a private member of 'X'}}
