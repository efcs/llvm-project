// A noexcept function terminates if a replaced violation handler throws: the
// handler's exception cannot escape a noexcept function, so std::terminate runs.
// (GCC mirror: g++.dg/contracts/cpp26/contract-violation-noexcept2.C.)
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

#include <contracts>
#include <exception>
#include <cstdlib>

void handle_contract_violation(const std::contracts::contract_violation&) {
  throw 666;
}

void f(int x) noexcept pre(x >= 0) {}

int main() {
  std::set_terminate([] { std::exit(0); });
  f(-1);            // noexcept + throwing handler -> terminate
  __builtin_trap(); // unreachable
}
