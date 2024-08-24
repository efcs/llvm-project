// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts -fcolor-diagnostics ||  %clang_cc1 -std=c++26 -fsyntax-only  %s -fcontracts -fcolor-diagnostics



struct ImpBC {
  operator bool() const;
};

struct ExpBC {
  explicit operator bool() const;
};

struct ImpBC2 {
  operator int() const;
};

struct NoBool {
};


template <class T>
void foo() pre(T{}) // expected-error {{'NoBool' is not contextually convertible to 'bool'}}
{
  contract_assert(T{}); // expected-error {{'NoBool' is not contextually convertible to 'bool'}}
}

template void foo<int>();
template void foo<ImpBC>();
template void foo<NoBool>(); // expected-note {{requested here}}

template <class T>
bool b(T&&);

template <class T>
constexpr bool is_const(T&&) {
  return __is_const(T);
}

namespace TestConversion {

template <class T>
struct Foo {
  template <class U = T>
  void foo( U&& u = {})
  pre(u) // expected-error {{value of type 'const NoBool' is not contextually convertible to 'bool'}}
  post(u) // expected-error {{value of type 'const NoBool' is not contextually convertible to 'bool'}}
  {}

  template <class U = T>
  void bar(U u = {})
    // expected-note@-1 {{parameter of type 'int' is declared here}}
    // expected-note@-2 {{'double'}} expected-note@-2 {{'long'}}
    pre(b(u))
    post(b(u)) // expected-error 1+ {{parameter 'u' referenced in contract postcondition must be declared const}}
  {}

};

void test_it() {
  {
    Foo<ImpBC> f;
    f.foo();
  }
  {
    Foo<NoBool> f;
    f.foo(); // expected-note {{requested here}}
  }
  {
    Foo<long> f;
    f.bar(35l); // expected-note {{requested here}}
  }
  {
    Foo<double> f;
    f.bar(35.0); // expected-note {{requested here}}
  }
  {
    Foo<int> f;
    f.bar<const int>();
    f.bar<int>(); // expected-note {{requested here}}
  }
}
} // namespace TestConversion


namespace TestConstification {

struct NonConst {
  operator bool(); // expected-note 1+ {{not viable}}
};

void f(NonConst &x)
pre(x) // expected-error {{'const NonConst'}}
post(x) // expected-error {{'const NonConst'}}
    {}


template <class U>
struct A : public U {
  template<class T>
  void f(T &&x = {})
    pre(this->bar()) // expected-error {{not marked const}}
  {}
  void bar(); // expected-note {{declared here}}

  template <class T>
  void g(T &&x = {})
    post(x) // expected-error {{const TestConstification::NonConst}}
  {}
};

struct B { void bar() const; };
struct C {};

void c1() {
    A<C>{}.f(42); // expected-note {{requested here}}
    A<B>{}.g(NonConst{}); // expected-note {{requested here}}
}

template <class T> void g(T&& x) post(x) {} // expected-error {{TestConstification::NonConst}}

template <class T> void w(T x) post(x) {} // expected-error {{must be declared const}}
// expected-note@-1 {{'int' is declared here}}

template <class T>
void v(T x) pre(x) { } // OK

void c2() {

    NonConst x;
    g(x); // expected-note {{requested here}}
    w(42); // expected-note {{requested here}}
    v(42);
}

} // namespace TestConstification

namespace DoubleInstant {
template<class T>
struct A {
  template <class U>
  void foo(T x) post((x, true)) {}
};
template struct A<int>; // OK? Isn't instantiated yet?


} // namespace DoubleInstant