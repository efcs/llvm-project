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
struct Foo {
  template <class U = T>
  void foo(U u = {}) pre(u) {}
};

void test_it() {
  Foo<ImpBC> f;
  f.foo();

  Foo<NoBool> f2;
  f2.foo();
}
