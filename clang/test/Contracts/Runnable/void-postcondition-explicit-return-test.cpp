// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=enforce %libcxx_flags -o %t
// RUN: %t

#include "my_assert.h"

int post_count = 0;

bool count_post() {
  ++post_count;
  return true;
}

void explicit_return() post(count_post()) {
  return;
}

int main() {
  post_count = 0;
  explicit_return();
  assert(post_count == 1);
}
