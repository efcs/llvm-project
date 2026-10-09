// A replaced violation handler that throws: under a non-terminating (observe)
// semantic on a function that is not noexcept, the exception propagates out of
// the contracted function as if thrown at the point of the assertion, so it is
// catchable by the caller.
// (GCC mirror: g++.dg/contracts/cpp26/throwing-violation-handler.cc driver.)
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

#include <contracts>
#include <cstdlib>

void handle_contract_violation(const std::contracts::contract_violation&) {
  throw 666;
}

int f(int x) pre(x >= 0) { return x; } // not noexcept

int main() {
  try {
    f(-1); // precondition violated -> handler throws -> propagates
  } catch (int e) {
    if (e == 666)
      std::exit(0);
  }
  __builtin_trap(); // unreachable if the exception propagated
}
