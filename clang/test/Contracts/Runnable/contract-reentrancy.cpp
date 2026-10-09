// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts \
// RUN:   -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

// Re-entrant contract instantiation, at run time.
//
// A contract predicate may odr-use a contracted function without the nested
// instantiation tripping over the outer one.  A compile-only test cannot tell
// whether the contract that was instantiated re-entrantly is any good: a
// predicate that silently never runs, or one substituted against the wrong
// function's parameters, compiles just as quietly as a correct one.
//
// So this checks that the re-entrantly instantiated contracts actually
// evaluate, in the right order, against the right objects.  Everything runs
// under the observe semantic so that a violation is counted rather than
// terminating.
//
// GCC mirror: g++.dg/contracts/cpp26/contract-reentrancy-run.C.

#include <contracts>

static int viol = 0;
void handle_contract_violation(const std::contracts::contract_violation &) {
  ++viol;
}

static int order[8];
static int seq_len = 0;
static bool rec(int id, bool result) {
  order[seq_len++] = id;
  return result;
}

//===----------------------------------------------------------------------===//
// The callee's precondition is evaluated from inside the caller's, and it is
// evaluated FIRST -- the caller's predicate cannot finish until the call it
// contains returns.
//===----------------------------------------------------------------------===//

template <class T> struct S {
  bool ok() const pre(rec(1, n >= 0)) { return n >= 0; }
  int n;
};

template <class T> int f_plain(S<T> s) pre(rec(2, s.ok())) { return s.n; }

// The same shape through a nested template function rather than a member.
template <class T> bool positive(T v) pre(rec(3, v != T{})) { return v > T{}; }

template <class T> T f_nested(T v) pre(rec(4, positive(v))) { return v; }

int main() {
  // Neither contract fails: the callee's precondition holds and it returns
  // true, so the caller's holds too.
  if (f_plain(S<int>{1}) != 1)
    return 1;
  if (viol != 0)
    return 2;
  if (seq_len != 2 || order[0] != 1 || order[1] != 2)
    return 3;

  // Both fail, callee first: S::ok's `n >= 0` is violated, ok() returns false,
  // and that makes f_plain's own `s.ok()` false as well.
  seq_len = 0;
  if (f_plain(S<int>{-1}) != -1)
    return 4;
  if (viol != 2)
    return 5;
  if (seq_len != 2 || order[0] != 1 || order[1] != 2)
    return 6;

  viol = 0;
  seq_len = 0;
  if (f_nested(5) != 5)
    return 7;
  if (viol != 0)
    return 8;
  if (seq_len != 2 || order[0] != 3 || order[1] != 4)
    return 9;

  seq_len = 0;
  if (f_nested(0) != 0)
    return 10;
  if (viol != 2)
    return 11;

  return 0;
}
