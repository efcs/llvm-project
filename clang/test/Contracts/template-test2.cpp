// RUN: %clang_cc1 -std=c++26 -fcontracts -verify %s || %clang_cc1 -std=c++26 -fcontracts -fcolor-diagnostics -fsyntax-only %s || %clang_cc1 -std=c++26 -fcontracts -fcolor-diagnostics -verify %s


namespace BasicTest {
template<class T>
T f(T x) {
  T local = x;
  contract_assert(++x);
  contract_assert(++local);
  return x;
}

void basic() {
  f(1);
}
} // namespace BasicTest

namespace LambdaTest {
template<class T>
void f(T x) {
  T local = x;
  contract_assert(++x);
  contract_assert(++local);
  struct X {
    auto foo(T z) {
      return [=]() mutable {

        contract_assert(++z);

      };
    }
  };
}

void instant() {
  f(42);
}
}