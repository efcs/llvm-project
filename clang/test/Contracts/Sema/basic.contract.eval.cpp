// RUN: %clang_cc1 -fcontracts -std=c++26 -fcolor-diagnostics %s

// [basic.contract.eval] - Example #1
constexpr int f(int i) {
  contract_assert((++const_cast<int &>(i), true));
  return i;
}
inline void g() {
  int a[f(1)]; // size dependent on the evaluation semantic of contract_assert above
}