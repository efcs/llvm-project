// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=enforce %libcxx_flags -o %t && not --crash %t

// The other half of [basic.contract.eval]/17's note: "If the function has a
// non-throwing exception specification, the function std::terminate is
// invoked ([except.terminate])."
//
// The handler throws out of a postcondition on a noexcept function.  Since
// the behavior is as if the function body exits via that exception, and the
// function is noexcept, the program terminates -- the try/catch in the body
// does not run, and neither does the one in main.  Both would abort with a
// different signature than terminate does, so `not --crash` alone would not
// distinguish them; the aborts are what make the run meaningful.
//
// GCC mirror: g++.dg/contracts/cpp26/postcondition-throw-noexcept-terminate.C

#include <contracts>

struct Bail {};

void handle_contract_violation(const std::contracts::contract_violation &) {
  throw Bail{};
}

static int f() noexcept post(r : r > 0) {
  try {
    return -1;
  } catch (...) {
    __builtin_abort(); // must not run
  }
}

int main() {
  try {
    f();
  } catch (const Bail &) {
    __builtin_abort(); // must not run either
  }
  return 0;
}
