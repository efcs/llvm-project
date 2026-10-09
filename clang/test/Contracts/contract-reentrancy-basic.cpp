// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// expected-no-diagnostics

// Re-entrant contract instantiation: a contract predicate odr-uses a function
// that itself has contracts, so instantiating one function's contracts starts
// instantiating another's while the first is still mid-transform.
//
// InstantiateFunctionContractsOnUse runs from MarkFunctionReferenced for every
// odr-use, and a predicate performs odr-uses, so this nesting is reachable
// from ordinary code.  Sema::PushContractScope records its state on
// getCurFunction(), so a nested pass that borrows the enclosing function's
// scope finds it already marked as being in a contract and asserts.  The two
// functions are unrelated, and each substitutes in a scope of its own.
//
// This file covers the shapes that need only plain -fcontracts.  The paper
// extensions get their own files, since each needs its own flag:
//   contract-reentrancy-p3097.cpp        virtual functions
//   contract-reentrancy-p3098.cpp        postcondition captures
//   contract-reentrancy-p3097-p3098.cpp  both at once
//   contract-reentrancy-p3850.cpp        labels, requires-clauses, messages
//   Runnable/contract-reentrancy.cpp     the contracts actually fire
//
// GCC mirror: g++.dg/contracts/cpp26/contract-reentrancy-basic.C.

//===----------------------------------------------------------------------===//
// The precondition of one function calls a contracted member of another.
//===----------------------------------------------------------------------===//

template <class T> struct S {
  bool ok() const pre(n >= 0) { return n >= 0; }
  int n = 0;
};

template <class T> int f_pre(S<T> s) pre(s.ok()) { return s.n; }
int use_pre() { return f_pre(S<int>{}); }

//===----------------------------------------------------------------------===//
// The outer contract is a postcondition rather than a precondition.  The value
// parameter is const because [dcl.contract.func]/7 requires that of one
// odr-used by a postcondition -- which is itself a rule the on-use
// instantiation exists to apply.
//===----------------------------------------------------------------------===//

template <class T> int f_post(const S<T> s) post(r: s.ok() && r >= 0) {
  return s.n;
}
int use_post() { return f_post(S<int>{}); }

//===----------------------------------------------------------------------===//
// Both ends are members of the same class template, so the nested pass runs
// with the same instantiation in flight.
//===----------------------------------------------------------------------===//

template <class T> struct Siblings {
  bool ok() const pre(n >= 0) { return n >= 0; }
  int get() const pre(ok()) { return n; }
  int n = 0;
};

int use_siblings() {
  Siblings<int> s;
  return s.get();
}

//===----------------------------------------------------------------------===//
// Control: the outer contract is on a NON-template function, so it is never
// instantiated and no nesting occurs.  This is what places the defect in the
// re-entrancy rather than in contracts that call contracted functions.
//===----------------------------------------------------------------------===//

template <class T> bool free_ok(T v) pre(v == v) { return v >= 0; }
int f_nontemplate(int x) pre(free_ok(x)) { return x; }
int use_nontemplate() { return f_nontemplate(1); }

//===----------------------------------------------------------------------===//
// Three levels deep: each predicate reaches one more contracted function.
//===----------------------------------------------------------------------===//

template <class T> struct Chain {
  bool c3() const pre(n >= 0) { return true; }
  bool c2() const pre(c3()) { return true; }
  bool c1() const pre(c2()) { return true; }
  int n = 0;
};

template <class T> int f_deep(Chain<T> c) pre(c.c1()) { return c.n; }
int use_deep() { return f_deep(Chain<int>{}); }

//===----------------------------------------------------------------------===//
// Mutual re-entrancy: each predicate odr-uses the other function.  Members, so
// that each is visible to the other's predicate on its first declaration --
// contracts may only appear on the first declaration, so two free functions
// cannot be written this way.
//===----------------------------------------------------------------------===//

template <class T> struct Mutual {
  bool f(T v) const pre(g(v)) { return true; }
  bool g(T v) const pre(f(v)) { return true; }
};

int use_mutual() {
  Mutual<int> m;
  return m.f(1);
}

//===----------------------------------------------------------------------===//
// Self re-entrancy: the predicate odr-uses the very function it belongs to.
// Unbounded at run time, but it must not run away at compile time -- the
// InstantiatingTemplate guard stops the recursion.
//===----------------------------------------------------------------------===//

template <class T> struct Selfish {
  bool f(T v) const pre(f(v)) { return true; }
};

int use_self() {
  Selfish<int> s;
  return s.f(1);
}

//===----------------------------------------------------------------------===//
// The odr-use is inside a lambda nested in the predicate, so the nested pass
// starts from within a lambda's function scope rather than the enclosing
// function's.
//===----------------------------------------------------------------------===//

template <class T> int f_lambda(S<T> s) pre([&] { return s.ok(); }()) {
  return s.n;
}
int use_lambda() { return f_lambda(S<int>{}); }

//===----------------------------------------------------------------------===//
// The re-entrant predicate is reached during constant evaluation.
//===----------------------------------------------------------------------===//

template <class T> struct CS {
  constexpr bool ok() const pre(n >= 0) { return n >= 0; }
  int n = 0;
};

template <class T> constexpr int f_constexpr(CS<T> s) pre(s.ok()) {
  return s.n;
}
static_assert(f_constexpr(CS<int>{}) == 0);

//===----------------------------------------------------------------------===//
// The callee reached from the predicate is declared but never defined.  This
// is the [dcl.contract.func]/9 case the on-use hook exists for: without it the
// callee's predicate stays dependent and is never checked at all.
//===----------------------------------------------------------------------===//

template <class T> bool declared_only(T v) pre(v == v);
template <class T> int f_declonly(T v) pre(declared_only(v)) { return 0; }
int use_declonly() { return f_declonly(1); }
