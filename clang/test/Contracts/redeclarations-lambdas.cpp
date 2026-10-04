// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify=expected,pedantic -pedantic %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -emit-pch -DPCH -o %t.pch %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -include-pch %t.pch -DPCH -fsyntax-only -verify=pch %s

// [dcl.contract.func]p5: Two function contract specifiers are the same if
// their predicates would satisfy the one-definition rule if placed in function
// definitions on the respective declarations, except for renaming of
// parameters, template parameters and the result binding. If that is not the
// case solely due to the comparison of two lambda-expressions contained in the
// predicates, no diagnostic is required.
//
// We treat lambda-expressions consisting of the same tokens as being the same
// (as they would be in function definitions in different translation units),
// and diagnose the remaining cases.

#ifdef PCH
// A declaration from a PCH can be redeclared.
#ifndef HEADER
#define HEADER
int pch1(int x) pre([](int v) { return v > 0; }(x));
int pch2(int x) pre([](int v) { return v > 0; }(x));
// The closure types of the lambda-expressions of these declarations are
// merged when the declarations are loaded.
int pch3(int x) pre([x] { return x > 0; }());
int pch3(int y) pre([y] { return y > 0; }());
#else
int pch1(int y) pre([](int v) { return v > 0; }(y)) { return y; }
int pch2(int x) pre([](int v) { return v > 1; }(x)); // pch-error {{function redeclaration differs in contract specifier sequence}}
// pch-note@-1 {{in contract specified here}}
// pch-note@-9 {{contract previously specified with a non-equivalent condition}}
int pch3(int z) pre([z] { return z > 0; }()) { return z; }
#endif
#else

template <class It, class Pred> bool all_of(It b, It e, Pred p) {
  for (; b != e; ++b)
    if (!p(*b))
      return false;
  return true;
}
struct Vec {
  const int *b, *e;
  const int *begin() const { return b; }
  const int *end() const { return e; }
};
bool call(bool (*fp)(int), int v);

namespace FreeFunctions {
int f(int x) pre([](int v) { return v > 0; }(x));
int f(int x) pre([](int v) { return v > 0; }(x)) { return x; }
// pedantic-warning@-1 {{redeclaration repeats a function contract specifier that contains a lambda expression; the closure types are distinct, so the program is ill-formed, no diagnostic required}}

// Renaming of the parameters and of the result binding is allowed.
int g(int x) pre([](int v) { return v > 0; }(x)) post(r : [](int v) { return v > 0; }(r));
int g(int y) pre([](int v) { return v > 0; }(y)) post(s : [](int v) { return v > 0; }(s)) { return y; } // pedantic-warning {{lambda expression}}

// The contracts can still be omitted.
int o(int x) pre([](int v) { return v > 0; }(x));
int o(int x) { return x; }

// A different body.
int h1(int x) pre([](int v) { return v > 0; }(x)); // expected-note {{contract previously specified with a non-equivalent condition}}
int h1(int x) pre([](int v) { return v > 1; }(x)); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// Entities declared within the predicate cannot be renamed.
int h2(int x) pre([](int v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
int h2(int x) pre([](int w) { return w > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// A different parameter type.
int h3(int x) pre([](int v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
int h3(int x) pre([](long v) { return v > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// Different lambda-declarators.
int h4(int x) pre([](int v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
int h4(int x) pre([](int v) noexcept { return v > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
int h5(int x) pre([](int v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
int h5(int x) pre([](int v) -> bool { return v > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
int h6(int x) pre([](int v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
int h6(int x) pre([](int v) mutable { return v > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
int h7(int x) pre([](int v = 1) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
int h7(int x) pre([](int v = 2) { return v > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// A lambda-expression is not the same as another kind of expression.
int h8(int x) pre([] { return true; }()); // expected-note {{non-equivalent condition}}
int h8(int x) pre(true); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
} // namespace FreeFunctions

namespace Captures {
int f(int x) pre([x] { return x > 0; }());
int f(int x) pre([x] { return x > 0; }()); // pedantic-warning {{lambda expression}}
int f(int y) pre([y] { return y > 0; }()) { return y; } // pedantic-warning {{lambda expression}}

int g(int x) pre([=] { return x > 0; }()) pre([&] { return x > 0; }()) pre([&x] { return x > 0; }());
int g(int y) pre([=] { return y > 0; }()) pre([&] { return y > 0; }()) pre([&y] { return y > 0; }()); // pedantic-warning {{lambda expression}}

// A different capture kind.
int h1(int x) pre([x] { return x > 0; }()); // expected-note {{non-equivalent condition}}
int h1(int x) pre([&x] { return x > 0; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// A different capture-default.
int h2(int x) pre([=] { return x > 0; }()); // expected-note {{non-equivalent condition}}
int h2(int x) pre([&] { return x > 0; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// An explicit capture is not the same as an implicit capture.
int h3(int x) pre([=] { return x > 0; }()); // expected-note {{non-equivalent condition}}
int h3(int x) pre([x] { return x > 0; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// A different parameter is captured.
int h4(int x, int y) pre([x] { return x > 0; }()); // expected-note {{non-equivalent condition}}
int h4(int x, int y) pre([y] { return y > 0; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// Init-captures.
int i1(int x) pre([y = x + 1] { return y > 0; }());
int i1(int z) pre([y = z + 1] { return y > 0; }()); // pedantic-warning {{lambda expression}}
int i2(int x) pre([y = x + 1] { return y > 0; }()); // expected-note {{non-equivalent condition}}
int i2(int x) pre([y = x + 2] { return y > 0; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
int i3(int x) pre([y = x + 1] { return y > 0; }()); // expected-note {{non-equivalent condition}}
int i3(int x) pre([z = x + 1] { return z > 0; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
} // namespace Captures

namespace Nested {
int f(int x) pre([](int v) { return [](int w) { return w > 0; }(v); }(x));
int f(int x) pre([](int v) { return [](int w) { return w > 0; }(v); }(x)) { return x; } // pedantic-warning {{lambda expression}}

int g(int x) pre([](int v) { return [](int w) { return w > 0; }(v); }(x)); // expected-note {{non-equivalent condition}}
int g(int x) pre([](int v) { return [](int w) { return w > 1; }(v); }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// A lambda-expression which returns a closure object.
int h(int x) pre([] { return [](int w) { return w > 0; }; }()(x));
int h(int x) pre([] { return [](int w) { return w > 0; }; }()(x)); // pedantic-warning {{lambda expression}}

// The same lambda-expressions, in a different order.
int k(int x) pre([](int v) { return v > 0; }(x) && [](int v) { return v > 1; }(x)); // expected-note {{non-equivalent condition}}
int k(int x) pre([](int v) { return v > 1; }(x) && [](int v) { return v > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
} // namespace Nested

namespace Locals {
int f(const Vec &c) pre([&] { for (int e : c) if (e <= 0) return false; return true; }());
int f(const Vec &d) pre([&] { for (int e : d) if (e <= 0) return false; return true; }()) { return 0; } // pedantic-warning {{lambda expression}}

int g(int x) pre([&] { int y = x; int z = y + 1; return z > y; }());
int g(int x) pre([&] { int y = x; int z = y + 1; return z > y; }()); // pedantic-warning {{lambda expression}}

int h1(int x) pre([&] { int y = x; int z = y + 1; return z > y; }()); // expected-note {{non-equivalent condition}}
int h1(int x) pre([&] { int y = x; int z = y + 1; return y > z; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

int h2(int x) pre([&] { int y = x; return y > 0; }()); // expected-note {{non-equivalent condition}}
int h2(int x) pre([&] { int q = x; return q > 0; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// A closure object stored in a local variable.
int k(int x) pre([&] { auto l = [](int v) { return v > 0; }; return l(x); }());
int k(int x) pre([&] { auto l = [](int v) { return v > 0; }; return l(x); }()); // pedantic-warning {{lambda expression}}

// Local classes are not compared structurally.
int l(int x) pre([&] { struct L { int v; }; return L{x}.v > 0; }()); // expected-note {{non-equivalent condition}}
int l(int x) pre([&] { struct L { int v; }; return L{x}.v > 0; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
} // namespace Locals

namespace Generic {
int f(int x) pre([](auto v) { return v > 0; }(x));
int f(int x) pre([](auto v) { return v > 0; }(x)); // pedantic-warning {{lambda expression}}

int g(int x) pre([]<class T>(T v) { return v > 0; }(x));
int g(int x) pre([]<class T>(T v) { return v > 0; }(x)); // pedantic-warning {{lambda expression}}

int h1(int x) pre([](auto v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
int h1(int x) pre([](auto v) { return v > 1; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

int h2(int x) pre([]<class T>(T v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
int h2(int x) pre([]<class T>(const T &v) { return v > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
} // namespace Generic

namespace Arguments {
// A closure object passed to a function template.
int f(const Vec &c) pre(all_of(c.begin(), c.end(), [](int e) { return e > 0; }));
int f(const Vec &c) pre(all_of(c.begin(), c.end(), [](int e) { return e > 0; })); // pedantic-warning {{lambda expression}}

int g(const Vec &c) pre(all_of(c.begin(), c.end(), [](int e) { return e > 0; })); // expected-note {{non-equivalent condition}}
int g(const Vec &c) pre(all_of(c.begin(), c.end(), [](int e) { return e >= 0; })); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

// A closure object converted to a pointer to function.
int h(int x) pre(call([](int v) { return v > 0; }, x)) pre(call(+[](int v) { return v > 0; }, x));
int h(int x) pre(call([](int v) { return v > 0; }, x)) pre(call(+[](int v) { return v > 0; }, x)); // pedantic-warning {{lambda expression}}

// A closure type named in an unevaluated operand is not compared.
int k(int x) pre(sizeof(decltype([] { return 1; })) > 0); // expected-note {{non-equivalent condition}}
int k(int x) pre(sizeof(decltype([] { return 1; })) > 0); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
} // namespace Arguments

namespace Members {
struct S {
  int m;
  int f(int x) pre([](int v) { return v > 0; }(x));
  int g(int x) pre([this](int v) { return v > m; }(x));
  int h(int x) pre([&] { return x > m; }());
  int k(int x) pre([*this](int v) { return v > m; }(x));
  int bad(int x) pre([this](int v) { return v > m; }(x)); // expected-note {{non-equivalent condition}}
  static int st(int x) pre([](int v) { return v > 0; }(x));
  friend int fr(S &s) pre([&] { return s.m > 0; }());
  friend int fr2(S &s) pre([&] { return s.m > 0; }()); // expected-note {{non-equivalent condition}}
};
int S::f(int x) pre([](int v) { return v > 0; }(x)) { return x; } // pedantic-warning {{lambda expression}}
int S::g(int y) pre([this](int v) { return v > m; }(y)) { return y; } // pedantic-warning {{lambda expression}}
int S::h(int x) pre([&] { return x > m; }()) { return x; } // pedantic-warning {{lambda expression}}
int S::k(int x) pre([*this](int v) { return v > m; }(x)) { return x; } // pedantic-warning {{lambda expression}}
int S::bad(int x) pre([this](int v) { return v >= m; }(x)) { return x; } // expected-error {{method out-of-line definition differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
int S::st(int x) pre([](int v) { return v > 0; }(x)) { return x; } // pedantic-warning {{lambda expression}}
int fr(S &t) pre([&] { return t.m > 0; }()) { return 0; } // pedantic-warning {{lambda expression}}
int fr2(S &s) pre([&] { return s.m > 1; }()); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
} // namespace Members

namespace Templates {
// Renaming of the template parameters is allowed.
template <class T> T f(T x) pre([](T v) { return v > 0; }(x));
template <class U> U f(U y) pre([](U v) { return v > 0; }(y)) { return y; } // pedantic-warning {{lambda expression}}
int use1 = f(1);

template <class T> T g(T x) pre([](T v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
template <class U> U g(U y) pre([](U v) { return v > 1; }(y)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

template <class T, class U> T h(T x, U y) pre([](T v) { return v > 0; }(x)); // expected-note {{non-equivalent condition}}
template <class T, class U> T h(T x, U y) pre([](U v) { return v > 0; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}

template <class T> struct TS {
  T m;
  int f(T x) pre([this](T v) { return v > m; }(x));
  template <class U> int g(U x) pre([&](T v) { return v > m; }(x));
  int bad(T x) pre([this](T v) { return v > m; }(x)); // expected-note {{non-equivalent condition}}
};
template <class T> int TS<T>::f(T y) pre([this](T v) { return v > m; }(y)) { return 1; } // pedantic-warning {{lambda expression}}
template <class T> template <class V>
int TS<T>::g(V y) pre([&](T v) { return v > m; }(y)) { return 1; } // pedantic-warning {{lambda expression}}
template <class T> int TS<T>::bad(T y) pre([this](T v) { return v < m; }(y)) { return 1; } // expected-error {{method out-of-line definition differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
int use2 = TS<int>().f(1) + TS<int>().g(1L);

// Explicit specializations have their own contracts.
template <class T> T s(T x) pre([](T v) { return v > 0; }(x));
template <> int s<int>(int x) pre([](int v) { return v > 1; }(x));
template <> int s<int>(int y) pre([](int v) { return v > 1; }(y)) { return y; } // pedantic-warning {{lambda expression}}
template <> long s<long>(long x) pre([](long v) { return v > 1; }(x)); // expected-note {{non-equivalent condition}}
template <> long s<long>(long x) pre([](long v) { return v > 2; }(x)); // expected-error {{differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
} // namespace Templates

#endif // PCH
