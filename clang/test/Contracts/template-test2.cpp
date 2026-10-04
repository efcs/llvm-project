// RUN: %clang_cc1 -std=c++26 -fcontracts -verify %s


namespace BasicTest {
template<class T>
T f(T x) {
  T local = x;
  contract_assert(++x); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert(++local); // expected-error {{cannot assign to variable 'local' because it is considered 'const' inside of a contract}}
  return x;
}

void basic() {
  f(1); // expected-note {{in instantiation of function template specialization 'BasicTest::f<int>' requested here}}
}
} // namespace BasicTest

namespace LambdaTest {
template<class T>
void f(T x) {
  T local = x;
  contract_assert(++x); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert(++local); // expected-error {{cannot assign to variable 'local' because it is considered 'const' inside of a contract}}
  struct X { // expected-note {{in instantiation of member function 'LambdaTest::f(int)::X::foo' requested here}}
    auto foo(T z) {
      // The copy of 'z' is declared outside of the contract assertion, so it
      // is const within it.
      return [=]() mutable { // expected-note {{while substituting into a lambda expression here}}
        contract_assert(++z); // expected-error {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}

      };
    }
  };
}

void instant() {
  f(42); // expected-note 2 {{in instantiation of function template specialization 'LambdaTest::f<int>' requested here}}
}
}