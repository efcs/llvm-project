// RUN: %clang_cc1 -std=c++23  %s -fcontracts -fcontract-evaluation-semantic=enforce

// Initially this caused a crash because we were failing to take into account
// the cleanups that are required for the contract evaluation.

__attribute__((weak)) void unknown();
struct S {
  S();
  bool foo() const;
  ~S();
};

namespace ONS {
  struct Tag {};
  struct O {};

  Tag operator==(O, O);
  Tag operator!=(O, O);
}

template <class T>
struct X {
  auto foo() const {
    [&]() { contract_assert(S{}.foo() && __is_same(decltype(T{} == T{}), ONS::Tag)); }();
    return S().foo();
  }
};


void f() {
  X<ONS::O> x;
  x.foo();

}

