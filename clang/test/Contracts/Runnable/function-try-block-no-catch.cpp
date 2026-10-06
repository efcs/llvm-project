// A throwing violation handler from a precondition behaves as if the function
// body exited via that exception; a function-try-block is the function body and
// therefore does NOT catch it. In a potentially-throwing function the exception
// instead propagates to the caller. ([basic.contract.eval] Note 12; GCC mirror:
// g++.dg/contracts/cpp26/basic.contract.eval.p17.C.)
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

#include <contracts>
#include <cstdlib>

struct E {};

void handle_contract_violation(const std::contracts::contract_violation&) {
  throw E{};
}

bool ftb_caught = false;

void f(int x) pre(x > 0) try {
} catch (...) {
  ftb_caught = true; // must NOT run: the ftb cannot catch a pre handler throw
}

int main() {
  try {
    f(-1); // pre violated -> handler throws -> propagates past the ftb
  } catch (E&) {
    if (!ftb_caught)
      std::exit(0);
  }
  __builtin_trap(); // unreachable
}
