// Contract evaluation order: preconditions (left-to-right) before the function
// body, postconditions after it. (No dedicated base test existed in either
// compiler; also owed to GCC.)
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=enforce %libcxx_flags -o %t
// RUN: %t

#include <contracts>
#include "my_assert.h"

char order[8];
int n = 0;
bool rec(char c) { order[n++] = c; return true; }

int f()
  pre(rec('a'))  // first precondition
  pre(rec('b'))  // second precondition (evaluated after the first)
  post(rec('d')) // postcondition (evaluated after the body)
{
  rec('c'); // body
  return 0;
}

int main() {
  f();
  order[n] = '\0';
  assert(__builtin_strcmp(order, "abcd") == 0);
  return 0;
}
