// General contracts/noexcept correctness (surfaced during D4298 review):
// a contract-violation handler that throws inside a noexcept function must
// terminate the program, regardless of the contract's own evaluation
// semantic -- here plain "enforce", not one of D4298's noexcept_enforce/
// noexcept_observe semantics -- because the enclosing function is itself
// noexcept and cannot let the exception escape.  Inline (non-wrapper,
// non-capture) check codegen.
// (GCC mirror: g++.dg/contracts/cpp26/p4298-noexcept-fn-inline.C)
//
// Discrimination follows basic.contract.eval.p17-4: main() wraps the
// triggering call in try/catch(...).  If the noexcept boundary correctly
// terminates, the installed terminate handler exits(0) before the exception
// ever reaches main's catch.  If the boundary were broken, the exception
// would propagate up, be caught by main's catch(...), and fall through to
// the "should not get here" __builtin_trap().
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=enforce %libcxx_flags -o %t
// RUN: %t

#include <contracts>
#include <exception>
#include <cstdlib>

struct MyException {};

// Confirm there is an active exception of the expected type at terminate.
void my_term()
{
  try { throw; }
  catch (MyException) { std::exit(0); }
}

void handle_contract_violation(const std::contracts::contract_violation&)
{
  throw MyException{};
}

int f(int x) noexcept pre(x > 0) { return x; }

int main()
{
  std::set_terminate(my_term);
  try {
    f(-1);
  } catch (...) {
  }
  __builtin_trap();  // We should not get here.
}
