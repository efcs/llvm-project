// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [dcl.contract.func]p4: A declaration that is not a first declaration shall
// have either no function-contract-specifier-seq or the same one as the first
// declaration.

namespace Same {
int f() post(r: r > 0);
int f() post(r: r > 0);
int f() post(r: r > 0) { return 1; }

// Renaming of the parameters and of the result binding is allowed.
int g(const int x) pre(x > 0) post(r: r > x);
int g(const int y) pre(y > 0) post(s: s > y) { return y + 1; }

// Omitting the contracts is allowed.
int h(int x) pre(x > 0);
int h(int x);
int h(int x) { return x; }

// Multiple declarators in one declaration each have their own contracts.
int a() pre(true), b() pre(false);
int a() pre(true);
int b() pre(false);
} // namespace Same

namespace Members {
struct S {
  int m;
  S() post(m == 0);
  int g() post(r: r > 0);
  int h(int x) pre(x > 0);
  int k() pre(m > 0);
  template <class T> int t(T x) pre(x > m);
};
S::S() post(m == 0) : m(0) {}
int S::g() post(r: r > 0) { return 1; }
int S::h(int y) pre(y > 0) { return y; }
int S::k() pre(m > 0) { return m; }
template <class T> int S::t(T y) pre(y > m) { return m; }

template <class T> struct TS {
  T m;
  int f() post(r: r > 0);
  int g(T x) pre(x > m);
};
template <class T> int TS<T>::f() post(r: r > 0) { return 1; }
template <class T> int TS<T>::g(T y) pre(y > m) { return 1; }
int use = TS<int>().f() + TS<int>().g(1);
} // namespace Members

namespace Friends {
class A {
  friend void f(A &a) pre(a.g()) post(a.g());
  bool g() const;
};
void f(A &a) pre(a.g()) post(a.g()) {}
} // namespace Friends

namespace Mismatch {
int f() pre(true); // expected-note {{contract previously specified with a non-equivalent condition}}
int f() pre(false); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{contract specified here}}

int g(); // expected-note {{previously declared without contracts here}}
int g() pre(true); // expected-error {{function redeclaration differs in contract specifier sequence}}

int h() pre(true); // expected-note {{previously declared with fewer contracts here (1 vs 2)}}
int h() pre(true) pre(true); // expected-error {{function redeclaration differs in contract specifier sequence}}

constexpr int k(int x) pre(x); // expected-note {{previously declared with fewer contracts here (1 vs 2)}}
constexpr int k(int y) pre(y);
constexpr int k(int z) pre(z);
constexpr int k(int x) pre(0) post(1) // expected-error {{function redeclaration differs in contract specifier sequence}}
{ return 0; }

template <typename T>
constexpr int t() pre(T{}) { return 1; };
// expected-note@-1 {{contract previously specified with a non-equivalent condition}}
template <typename U>
constexpr int t() pre(U{} + 1); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{contract specified here}}

struct S {
  int f(); // expected-note {{previously declared without contracts here}}
};
int S::f() pre(true) { return 0; } // expected-error {{method out-of-line definition differs in contract specifier sequence}}
} // namespace Mismatch

namespace ConstParams {
// [dcl.contract.func]p7: A non-reference parameter odr-used in a
// postcondition shall be const on all declarations.
int f(const int v) post(v > 0); // expected-note {{parameter referenced in postcondition of first declaration here}}
int f(int w); // expected-error {{parameter 'w' of type 'int' must be declared const because it is referenced in a postcondition of the function}}
int f(const int x);

int g(const int v, int &r) post(r > v);
int g(const int, int &);
} // namespace ConstParams

namespace ExplicitInstantiation {
using size_t = decltype(sizeof(int));
template <size_t x> struct UT { constexpr int size() const { return x; } };

template <size_t bits> class t {
  explicit t(const UT<bits> f) pre(f.size() == bits / 8) {}
};

template t<128>::t(UT<128> f);

template <> t<127>::t(const UT<127> f) {}
} // namespace ExplicitInstantiation
