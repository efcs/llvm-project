// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// GENERATED from a contract-feature cross-product specification.
// Do not edit by hand -- regeneration discards any change made here.
//
// Who needs a function's contracts, crossed with what state they are in and what the predicate contains.
//
// Three axes.  READER: the function's own definition, a P3097
// interface wrapper, a P3595 caller-side check, constant evaluation, or another
// contract's predicate.  PRODUCER: defined, declared-only, pure virtual,
// out-of-line, explicitly instantiated.  PREDICATE KIND: a constant, a
// non-dependent call, a type-dependent expression, or a reference to a member of
// the dependent class.
//
// The third axis is the one worth defending.  Contracts that are never
// substituted at all show up with a predicate containing anything substitution
// would rewrite, and a plain call suffices; a predicate carried into CodeGen
// still dependent needs one that is value-*dependent* when parsed.  `pre (true)`
// reproduces neither, because a predicate that is already a constant is usable
// without substitution at all.  Two failure modes one axis apart -- and that
// axis is why hand-picked shapes keep catching one and missing the other.
//
// Every shape is measured at both the syntax and the codegen phase, because a
// predicate that reaches CodeGen still dependent is invisible to
// -fsyntax-only.
//
// Mirror: g++.dg/contracts/cpp26/matrix-readers.C

// expected-no-diagnostics

int mxhelper ();
constexpr int mxhelper_ce () { return 0; }

// --------------------------------------------------------------------------
// Constant evaluation as the reader: the contract is needed during
// compilation of a constant expression rather than at run time, which reaches the
// constexpr evaluator's own copy of the machinery.
//
// Predicate kind: constant -- `true`.  Phase: syntax.
// --------------------------------------------------------------------------
namespace constant_evaluation__constant__syntax {

template <class T> struct A {
  constexpr int get () const pre (true) { return n; }
  int n;
};
constexpr A<int> a{0};
static_assert (a.get () == 0);
}  // namespace constant_evaluation__constant__syntax

// --------------------------------------------------------------------------
// Constant evaluation as the reader: the contract is needed during
// compilation of a constant expression rather than at run time, which reaches the
// constexpr evaluator's own copy of the machinery.
//
// Predicate kind: call -- `mxhelper_ce () == 0`.  Phase: syntax.
// --------------------------------------------------------------------------
namespace constant_evaluation__call__syntax {

template <class T> struct A {
  constexpr int get () const pre (mxhelper_ce () == 0) { return n; }
  int n;
};
constexpr A<int> a{0};
static_assert (a.get () == 0);
}  // namespace constant_evaluation__call__syntax

// --------------------------------------------------------------------------
// Constant evaluation as the reader: the contract is needed during
// compilation of a constant expression rather than at run time, which reaches the
// constexpr evaluator's own copy of the machinery.
//
// Predicate kind: dependent -- `sizeof (T) > 0`.  Phase: syntax.
// --------------------------------------------------------------------------
namespace constant_evaluation__dependent__syntax {

template <class T> struct A {
  constexpr int get () const pre (sizeof (T) > 0) { return n; }
  int n;
};
constexpr A<int> a{0};
static_assert (a.get () == 0);
}  // namespace constant_evaluation__dependent__syntax

// --------------------------------------------------------------------------
// Constant evaluation as the reader: the contract is needed during
// compilation of a constant expression rather than at run time, which reaches the
// constexpr evaluator's own copy of the machinery.
//
// Predicate kind: member -- `n >= 0`.  Phase: syntax.
// --------------------------------------------------------------------------
namespace constant_evaluation__member__syntax {

template <class T> struct A {
  constexpr int get () const pre (n >= 0) { return n; }
  int n;
};
constexpr A<int> a{0};
static_assert (a.get () == 0);
}  // namespace constant_evaluation__member__syntax

// --------------------------------------------------------------------------
// Another contract's predicate as the reader -- the re-entrancy
// case, where instantiating one function's contracts starts instantiating
// another's mid-transform.  Kept here so the cross-product is complete rather
// than because the dedicated suite is inadequate.
//
// Predicate kind: constant -- `true`.  Phase: syntax.
// --------------------------------------------------------------------------
namespace predicate_odr_use__constant__syntax {

template <class T> struct A {
  bool ok () const pre (true) { return true; }
  int n;
};
template <class T> int f (A<T> a) pre (a.ok ()) { return a.n; }
int use () { return f (A<int>{}); }
}  // namespace predicate_odr_use__constant__syntax

// --------------------------------------------------------------------------
// Another contract's predicate as the reader -- the re-entrancy
// case, where instantiating one function's contracts starts instantiating
// another's mid-transform.  Kept here so the cross-product is complete rather
// than because the dedicated suite is inadequate.
//
// Predicate kind: call -- `mxhelper () == 0`.  Phase: syntax.
// --------------------------------------------------------------------------
namespace predicate_odr_use__call__syntax {

template <class T> struct A {
  bool ok () const pre (mxhelper () == 0) { return true; }
  int n;
};
template <class T> int f (A<T> a) pre (a.ok ()) { return a.n; }
int use () { return f (A<int>{}); }
}  // namespace predicate_odr_use__call__syntax

// --------------------------------------------------------------------------
// Another contract's predicate as the reader -- the re-entrancy
// case, where instantiating one function's contracts starts instantiating
// another's mid-transform.  Kept here so the cross-product is complete rather
// than because the dedicated suite is inadequate.
//
// Predicate kind: dependent -- `sizeof (T) > 0`.  Phase: syntax.
// --------------------------------------------------------------------------
namespace predicate_odr_use__dependent__syntax {

template <class T> struct A {
  bool ok () const pre (sizeof (T) > 0) { return true; }
  int n;
};
template <class T> int f (A<T> a) pre (a.ok ()) { return a.n; }
int use () { return f (A<int>{}); }
}  // namespace predicate_odr_use__dependent__syntax

// --------------------------------------------------------------------------
// Another contract's predicate as the reader -- the re-entrancy
// case, where instantiating one function's contracts starts instantiating
// another's mid-transform.  Kept here so the cross-product is complete rather
// than because the dedicated suite is inadequate.
//
// Predicate kind: member -- `n >= 0`.  Phase: syntax.
// --------------------------------------------------------------------------
namespace predicate_odr_use__member__syntax {

template <class T> struct A {
  bool ok () const pre (n >= 0) { return true; }
  int n;
};
template <class T> int f (A<T> a) pre (a.ok ()) { return a.n; }
int use () { return f (A<int>{}); }
}  // namespace predicate_odr_use__member__syntax
