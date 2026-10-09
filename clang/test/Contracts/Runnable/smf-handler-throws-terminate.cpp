// A throwing violation handler invoked from a special member function with a
// non-throwing exception specification terminates: the function-try-block is the
// function body and does not catch the exception ([basic.contract.eval] Note 12).
// (GCC mirror: g++.dg/contracts/cpp26/basic.contract.eval.p17-SMF-pre.C.)
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

#include <contracts>
#include <exception>
#include <cstdlib>

struct MyException {};

void handle_contract_violation(const std::contracts::contract_violation&) {
  throw MyException{};
}

struct X {
  X(int x) noexcept pre(x > 1) try {
  } catch (...) {
    // A function-try-block on a ctor cannot catch a contract-handler exception.
  }
};

int main() {
  std::set_terminate([] { std::exit(0); });
  try {
    X x(-42); // pre violated -> handler throws -> noexcept ctor -> terminate
  } catch (...) {
  }
  __builtin_trap(); // unreachable
}
