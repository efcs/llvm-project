// RUN: %clang -fcontracts -std=c++20 -fcontract-evaluation-semantic=observe %s -o %t
// RUN:  %t 1>&2

#include "contracts.h"
#include "contracts-runtime.h"
#include "my_assert.h"

using namespace std::contracts;



void test(int x) pre(x) post(x) { contract_assert(x); }

int main() {
  test(0);
  assert(count == 3);
}