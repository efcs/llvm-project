// REQUIRES: contracts-libcxx
// RUN: %clangxx -fcontracts -std=c++26 -fcontract-evaluation-semantic=enforce %s %libcxx_flags -o %t
// RUN: %t | FileCheck %s

// The destructor of a variable with static storage duration evaluates a
// contract assertion that is violated. The variable is constant-initialized,
// and its destruction would be constant if the contract assertion were
// ignored, but it is not: the destructor has to run at exit and check the
// contract assertion, which invokes the contract-violation handler.

#include <contracts>
#include <cstdio>
#include <cstdlib>

struct D {
  int v;
  constexpr D(int v) : v(v) {}
  constexpr ~D() { contract_assert(v > 0); }
};

static D s(0);
D s2(0);
D fine(1);

void handle_contract_violation(const std::contracts::contract_violation &v) {
  std::printf("violation: %s\n", v.comment());
  std::fflush(stdout);
  // The enforce semantic terminates the program if the handler returns.
  std::_Exit(0);
}

int main() {
  std::printf("main done\n");
  // CHECK: main done
  // 'fine' is destroyed first, without a violation.
  // CHECK-NEXT: violation: v > 0
  // CHECK-NOT: violation
}
