// A precondition failing under the 'enforce' semantic must report its real
// assertion kind (pre), NOT "assert".  The enforced violation for pre/post/
// assert contracts is emitted through a per-function shared enforce block; if
// that block hardcodes the kind, a failing 'pre' would be reported as 'assert'.
//
// -fno-exceptions forces the shared-enforce-block codegen path, which is the
// one that can hardcode assertion_kind::assert.  The handler validates the kind
// and exits 0 on success (before the noreturn terminate); any mismatch exits
// with a distinct non-zero status.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fno-exceptions -fcontract-evaluation-semantic=enforce %libcxx_flags -o %t && %t

#include <contracts>
#include <cstdlib>

using namespace std::contracts;

void handle_contract_violation(const contract_violation &v) {
  if (v.kind() != assertion_kind::pre)
    std::_Exit(2);
  if (v.semantic() != evaluation_semantic::enforce)
    std::_Exit(3);
  // Real kind observed; exit before the (noreturn) enforce terminate.
  std::exit(0);
}

void f(const int x) pre(x > 0) {}

int main() {
  f(-1);      // fails 'pre'; enforce handler must see kind == pre
  std::abort(); // unreachable: handler exits 0 first
}
