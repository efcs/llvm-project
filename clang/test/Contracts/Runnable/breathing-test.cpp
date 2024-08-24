// RUN: %clang -fcontracts -std=c++20 -fcontract-evaluation-semantic=observe %s -o %t
// RUN:  %t 1>&2

#include "contracts.h"
#include "contracts-runtime.h"
#include "my_assert.h"

using namespace std::contracts;



int count = 0;
bool counter(bool value) {
  ++count;
  return value;
}

void test(const int x) pre(counter(x)) post(counter(x)) { contract_assert(counter(x)); }

int main() {
  test(1);
  assert(count == 3);
}
