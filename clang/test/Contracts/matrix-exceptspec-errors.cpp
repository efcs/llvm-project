// RUN: %clang_cc1 -std=c++26 -fcontracts -verify-ignore-unexpected=note -fsyntax-only -verify %s
// GENERATED from a contract-feature cross-product specification.
// Do not edit by hand -- regeneration discards any change made here.
//
// Contracts must never change a function's exception specification.
//
// A contract assertion is not part of a function's interface,
// so nothing about it -- the predicate, whether the predicate can throw, a P3400
// label, a P3098 capture initializer, the chosen evaluation semantic, or the
// P3097 wrapper interposed on a virtual call -- may change what the exception
// specification says.
//
// Every cell compares a contracted construct against an uncontracted twin rather
// than against a literal answer.  `static_assert (noexcept (f ()) == false)` on
// its own would still pass if the compiler had lost the specification entirely;
// comparing against the twin says the contract changed nothing, which is the
// proposition actually at issue.
//
// The separate question -- what happens at run time when a contract does throw --
// is the `throwpaths` group.
//
// Mirror: g++.dg/contracts/cpp26/matrix-exceptspec-errors.C


// --------------------------------------------------------------------------
// The negative control for the cell above.  A non-noexcept
// function still must NOT convert to a `noexcept` pointer just because it
// carries a contract -- if a contract could launder that conversion, the type
// system would have been perturbed after all.
// --------------------------------------------------------------------------
namespace noexcept_pointer_conversion_still_rejected {

void guarded_throwing () pre (true);

void (*p) () noexcept = &guarded_throwing;  // expected-error {{}}
}  // namespace noexcept_pointer_conversion_still_rejected
