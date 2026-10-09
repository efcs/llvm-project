// RUN: %clang_cc1 -std=c++26 -fcontracts -verify-ignore-unexpected=note -emit-llvm -o /dev/null -verify %s
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
// Mirror: g++.dg/contracts/cpp26/matrix-sfinae-codegen-errors.C


struct HasIt { bool nonexistent () const { return true; } };

// --------------------------------------------------------------------------
// After a predicate fails to substitute, the compiler must
// diagnose and stop cleanly rather than carrying an invalid contract into code
// generation.  Run at the codegen phase on purpose: the failure mode this
// guards against is a dependent predicate surviving all the way to CodeGen, so
// a syntax-only version of this cell would not be testing what it claims to.
// --------------------------------------------------------------------------
namespace recovery_after_illformed_predicate_at_codegen {

template <class T> int f (T t) pre (t.nonexistent ()) { return 1; }  // expected-error {{}}

int use () { return f (0); }
}  // namespace recovery_after_illformed_predicate_at_codegen
