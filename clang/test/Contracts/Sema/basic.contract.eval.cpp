// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -fcontract-evaluation-semantic=ignore -DSIZE=1 -verify %s
// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -fcontract-evaluation-semantic=observe -DSIZE=2 -verify %s
// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -fcontract-evaluation-semantic=enforce -DSIZE=2 -verify %s
// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -fcontract-evaluation-semantic=quick_enforce -DSIZE=2 -verify %s

// [basic.contract.eval] - Example #2
constexpr int f(int i) {
  contract_assert((++const_cast<int &>(i), true));
  return i;
}
inline void g() {
  int a[f(1)]; // size dependent on the evaluation semantic of contract_assert above
  static_assert(sizeof(a) / sizeof(int) == SIZE);
}
// expected-no-diagnostics
