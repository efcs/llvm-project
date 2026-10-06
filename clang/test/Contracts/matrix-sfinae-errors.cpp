// RUN: %clang_cc1 -std=c++26 -fcontracts -verify-ignore-unexpected=note -fsyntax-only -verify %s
// GENERATED from a contract-feature cross-product specification.
// Do not edit by hand -- regeneration discards any change made here.
//
// Contracts and the immediate context: deduction, overload resolution, concepts and unevaluated operands.
//
// Contracts are not part of a function's type or signature, so
// they must be invisible to deduction, overload resolution, partial ordering and
// concept satisfaction.  What they are NOT is SFINAE-friendly: once a function is
// odr-used its contracts are needed ([dcl.contract.func]/9), and a predicate that
// cannot be substituted is a hard error -- never a quiet substitution failure that
// selects a different overload.
//
// The unevaluated-operand cells are the ones worth understanding before editing
// this file.  Contract instantiation runs from the odr-use path in both compilers
// (Clang's InstantiateFunctionContractsOnUse and GCC's
// maybe_instantiate_contracts), and an unevaluated operand does not odr-use.
// If that ever stops
// being true, an ill-formed predicate escapes the one specialization it belongs to
// and starts breaking concept checks in code that never mentioned contracts.
//
// Mirror: g++.dg/contracts/cpp26/matrix-sfinae-errors.C


struct HasIt { bool nonexistent () const { return true; } };

// --------------------------------------------------------------------------
// Calling the specialization needs its contracts
// ([dcl.contract.func]/9), so a predicate that cannot be substituted is a hard
// error.  The diagnostic is the whole point of the cell: silence here would mean
// the contract had been quietly dropped.
//
// The function here is DECLARED ONLY, which is what separates this from the
// cells around it -- there is no definition whose instantiation would reach the
// predicate, so the odr-use is the only thing that can.  And the odr-use may not
// survive: if it is optimized away there is no undefined reference either, so
// without this diagnostic the ill-formed predicate would go unreported in every
// translation unit.
//
// Diagnosing it requires instantiating a declared-only function's contracts
// from mark_used, on the odr-use itself; a compiler that instantiates contracts
// only when it instantiates a definition accepts this program silently.
// --------------------------------------------------------------------------
// (Moved to OpenBugs/contracts-needed-on-odr-use.cpp: this Clang does not
// instantiate the contracts of a declared-only template on odr-use.)

// --------------------------------------------------------------------------
// The one that matters most.  If an ill-formed contract predicate
// were treated as a substitution failure, the template would drop out of the
// overload set and the ellipsis overload would be selected -- silently calling a
// different function than the one written.  A hard error is correct; picking
// `fallback` is not.  Written so that the two outcomes are distinguishable:
// success here would compile cleanly and return 2.
// --------------------------------------------------------------------------
namespace illformed_predicate_does_not_sfinae_away {

template <class T> int chosen (T t) pre (t.nonexistent ()) { return 1; }  // expected-error {{}}
int chosen (...) { return 2; }

int use () { return chosen (0); }
}  // namespace illformed_predicate_does_not_sfinae_away

// --------------------------------------------------------------------------
// The same proposition for a member of a class template, whose
// contracts are handed to the instantiation as a placeholder and substituted
// later.  A different code path to the free-function case, so it gets its own
// cell.
// --------------------------------------------------------------------------
namespace illformed_predicate_in_class_template_member {

template <class T> struct S {
  int f (T t) const pre (t.nonexistent ()) { return 1; }  // expected-error {{}}
};

int use () { S<int> s; return s.f (0); }
}  // namespace illformed_predicate_in_class_template_member

// --------------------------------------------------------------------------
// An invalid contract must not swallow the rest of the
// translation unit.  A second, unrelated error after the bad predicate has to be
// reported too -- otherwise a contract mistake hides real bugs behind it.
// --------------------------------------------------------------------------
namespace recovery_other_diagnostics_still_reported {

template <class T> int f (T t) pre (t.nonexistent ()) { return 1; }  // expected-error {{}}

int use () { return f (0); }
int unrelated () { return undeclared_identifier; }  // expected-error {{}}
}  // namespace recovery_other_diagnostics_still_reported
