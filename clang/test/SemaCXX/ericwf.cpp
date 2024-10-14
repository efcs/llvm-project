// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts


void foo(int x) {
  int y = x;
  contract_assert(++y); // expected-error {{read-only variable is not assignable}}
  contract_assert(y++); // expected-error {{read-only variable is not assignable}}
  ++y;
}


template <class T>
void bar(T v) {
  contract_assert(v);
  contract_assert(v++); // expected-error {{read-only variable is not assignable}}
  contract_assert(++v); // expected-error {{read-only variable is not assignable}}
}

template void bar<int>(int); // expected-note {{requested here}}

int g = 42;

struct A {
  void a(int x) {
    contract_assert(b()); // expected-error {{this' argument to member function 'b' has type 'const A', but function is not marked const}}
    contract_assert(z++); // expected-error {{read-only variable is not assignable}}
    contract_assert(++g); // OK
    contract_assert(const_cast<decltype(this)>(this)->b()); // OK
  }

  int b() {} // expected-note {{declared here}}
  int z;
};
