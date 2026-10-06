// Mirror of g++.dg/contracts/cpp26/contract-result-binding-identity.C.
//
// [dcl.contract.res]/1 binds a postcondition's result name to one object.  On
// the GCC side a scalar result was spilled afresh every time a predicate took
// its address, so one predicate saw two objects and could compute a wrong
// value (PR112794).  Clang has never had that; this is a regression pin.
//
// Example 2 in [dcl.contract.res] permits a temporary for a register-returned
// result, so WHICH object the binding names is unspecified there -- but not
// that there may be two -- and requires `&r == ptr' to hold for a class with
// a non-trivial copy constructor, which is checked below.
//
// NOTE: whether a mutation performed through the result binding is observable
// is deliberately NOT checked here.  Clang loses it, GCC (as of 2026-09-02)
// does not; see contract-result-binding-mutation.cpp, which pins that gap.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts %libcxx_flags -o %t && %t

const int *a1 = nullptr;
const int *a2 = nullptr;

const int *addr_via_ref(const int &i) { return &i; }

bool rec1(const int *p) { a1 = p; return true; }
bool rec2(const int *p) { a2 = p; return true; }

// Same binding, two ways of taking its address, one predicate evaluation.
int two_addresses() post(r : rec1(&r) && rec2(addr_via_ref(r))) { return 7; }

// The same disagreement expressed as the predicate's value: a faithful
// evaluation is plainly true, so no violation may occur.
bool same(const int &a, const int *b) { return &a == b; }

int value_form() post(r : same(r, &r)) { return 7; }

// A class returned in memory: Example 2 requires &r to be the returned object
// itself, so no stand-in may be introduced for this one.
struct Big {
  Big() {}
  Big(const Big &) {}
  int x = 0;
};

Big memory_returned(Big *const ptr) post(r : &r == ptr) { return {}; }

int main() {
  two_addresses();
  if (a1 != a2)
    __builtin_abort();

  value_form();

  Big b = memory_returned(&b);
  (void)b;

  return 0;
}
