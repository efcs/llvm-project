// Regression test for the contract allowed-semantics mask.
//
// A contract with no assertion-control label has an "all semantics allowed"
// mask, and that mask must cover QuickEnforce.  Bit N corresponds to
// evaluation-semantic value N, so a mask of 0xF covers Ignore(1)/Observe(2)/
// Enforce(3) but NOT QuickEnforce(4) -- under which a contract resolved to
// quick_enforce is silently downgraded through the fallback chain to observe
// and a violation does not terminate.  Here quick_enforce is honored and a
// failing precondition traps.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=quick_enforce %libcxx_flags -o %t
// RUN: not --crash %t

int f(int i) pre(i > 0) { return i; }

int main() {
  return f(-1); // quick_enforce: must trap, not fall back to observe
}
