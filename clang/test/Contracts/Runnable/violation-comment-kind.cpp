// The contract_violation kind() reports pre/post/assert for the three assertion
// forms, and comment() returns the predicate's source text. (GCC mirror:
// g++.dg/contracts/cpp26/basic.contract.eval.p11-observe.C, which checks
// assertion_kind and the comment for each form.) Note: Clang's postcondition
// comment() retains the "r :" result-name prefix (a logged divergence from GCC).
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

#include <contracts>
#include "my_assert.h"

using namespace std::contracts;

assertion_kind exp_kind;
const char* exp_comment;
unsigned calls = 0;

void handle_contract_violation(const contract_violation& v) {
  ++calls;
  assert(v.kind() == exp_kind);
  assert(__builtin_strcmp(v.comment(), exp_comment) == 0);
}

int g(const int x)
  pre(x > 100)
  post(r : r > x)
{
  exp_kind = assertion_kind::assert;
  exp_comment = "x == 42";
  contract_assert(x == 42);
  exp_kind = assertion_kind::post;
  exp_comment = "r : r > x";
  return x; // postcondition fires here
}

int main() {
  exp_kind = assertion_kind::pre;
  exp_comment = "x > 100";
  g(7); // pre, then assert, then post each violate under observe
  assert(calls == 3);
  return 0;
}
