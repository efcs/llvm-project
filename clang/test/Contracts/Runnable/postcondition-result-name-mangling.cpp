// A postcondition with a result name does not create a mangling conflict with an
// identically named overload that has an extra parameter. (GCC mirror:
// g++.dg/contracts/cpp26/name_mangling.C.)
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

#include <contracts>

int f() post(r : r > 1) { return 2; }
void f(int) post(true) {}

int main() {
  f();
  f(2);
  return 0;
}
