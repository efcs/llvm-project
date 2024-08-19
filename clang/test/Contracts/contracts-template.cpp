// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts



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

namespace BasicInstantiation {

template <class T>
struct Foo {
  template <class U = T>
  void foo( U&& u = {})
  pre(u) // expected-error {{value of type 'const NoBool' is not contextually convertible to 'bool'}}
  post(u) // expected-error {{value of type 'const NoBool' is not contextually convertible to 'bool'}}
  {}

  template <class U = T>
  void bar(U u = {})
    pre(b(u))
    post(b(u))
  {}

};

void test_it() {
  Foo<ImpBC> f;
  f.foo();

  Foo<NoBool> f2;
  f2.foo(); // expected-note {{requested here}}

  Foo<int> f3;
  f3.foo();
  f3.bar<const int>();
  f3.bar<int>();

  Foo<double> f4;
  f4.bar(35);
}
}


namespace DoubleInstant {
template<class T>
struct A {
  template <class U>
  void foo(T x) post((x, true)) {}
};
template struct A<int>;


} // namespace DoubleInstant