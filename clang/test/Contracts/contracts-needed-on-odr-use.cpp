// RUN: %clang_cc1 -std=c++26 -fcontracts -verify-ignore-unexpected=note -fsyntax-only -verify %s
//
// A declared-only function template is odr-used, so its contracts are needed
// ([dcl.contract.func]/9) and the predicate must be substituted -- here it
// cannot be, which is a hard error. (Cell moved here from
// matrix-sfinae-errors.cpp.)

struct HasIt { bool nonexistent () const { return true; } };

// --------------------------------------------------------------------------
// Calling the specialization needs its contracts
// ([dcl.contract.func]/9), so a predicate that cannot be substituted is a hard
// error.  The diagnostic is the whole point of the cell: silence here would mean
// the contract had been quietly dropped.
//
// The function here is DECLARED ONLY, which is what separates this from the
// cells in matrix-sfinae-errors.cpp -- there is no definition whose
// instantiation would reach the predicate, so the odr-use is the only thing
// that can.  And the odr-use may not survive: if it is optimized away there is
// no undefined reference either, so without this diagnostic the ill-formed
// predicate would go unreported in every translation unit.
//
// Diagnosing it requires instantiating a declared-only function's contracts
// from MarkFunctionReferenced, on the odr-use itself; a compiler that
// instantiates contracts only when it instantiates a definition accepts this
// program silently.
// --------------------------------------------------------------------------
namespace odr_used_illformed_predicate_is_an_error {

template <class T> int f (T t) pre (t.nonexistent ());  // expected-error {{}}

int use () { return f (0); }
}  // namespace odr_used_illformed_predicate_is_an_error
