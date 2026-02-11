// RUN: %clang_cc1 -fcontracts -std=c++26 -fcolor-diagnostics -Wno-division-by-zero -fcontract-evaluation-semantic=ignore -verify=expected %s
// RUN: %clang_cc1 -fcontracts -std=c++26 -fcolor-diagnostics -Wno-division-by-zero -fcontract-evaluation-semantic=enforce -verify=expected,enforce %s


consteval int id(int i) { return i; }
constexpr char id(char c) { return c; }

template <class T> constexpr int f(T t) {   // expected-note {{here}}
  return t + id(t); // expected-note {{consteval function 'id'}}
}
auto a = &f<char>; // OK, f<char> is not an immediate function
auto b = &f<int>;  // expected-error {{outside of an immediate invocation}}

static_assert(f(3) == 6); // OK

template <class T> constexpr int g(T t) { // g<int> is not an immediate function
  return t + id(42); // because id(42) is already a constant
}

template <class T, class F> constexpr bool is_not(T t, F f) { return not f(t); }

consteval bool is_even(int i) { return i % 2 == 0; }

static_assert(is_not(5, is_even)); // OK

int x = 0; // expected-note {{declared here}}

template <class T>
constexpr T h(T t = id(x)) { // expected-note {{consteval function 'id'}}
  // expected-note@-1 {{read of non-const variable 'x' is not allowed in a constant expression}}
  // id(x) is not evaluated when parsing the default argument
  // ([dcl.fct.default], [temp.inst])
  return t;
}

template <class T>
constexpr T hh() { // hh<int> is an immediate function because of the invocation
  return h<T>();   // of the immediate function id in the default argument of
                   // h<int>
}

int i = hh<int>(); // expected-error {{call to immediate function 'hh<int>' is not a constant expression}} expected-note {{in call to 'hh<int>()'}}
// outside of an immediate-escalating function

struct A {
  int x;
  int y = id(x);
};

template <class T>
constexpr int k(int) { // k<int> is not an immediate function because A(42) is a
  return A(42).y;      // constant expression and thus not immediate-escalating
}

constexpr int l(int c) pre(c >= 2) { // enforce-error {{contract failed}}
   return (c % 2 == 0) ? c / 0 : c;
}
const int i0 =
    l(0); // dynamic initialization is contract violation or undefined behavior
const int i1 = // enforce-error {{initialization of constant-initialized variable failed}}
// enforce-note@-1 {{initializer is constant when evaluated with 'ignore' contract evaluation semantic}}
    l(1); // enforce-note {{in call to 'l(1)'}}

const int i2 = l(2); // dynamic initialization is undefined behavior
const int i3 = l(3); //