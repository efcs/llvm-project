// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
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
// Mirror: g++.dg/contracts/cpp26/matrix-sfinae.C

// expected-no-diagnostics

struct HasIt { bool nonexistent () const { return true; } };

template <class T, class U> struct same { static constexpr bool value = false; };
template <class T> struct same<T, T> { static constexpr bool value = true; };

// --------------------------------------------------------------------------
// decltype does not odr-use its operand, so a specialization
// whose contract predicate could not be substituted must still be usable as a
// type.  If contract instantiation leaks into unevaluated operands this becomes
// a hard error, and every concept that mentions the function breaks with it.
// --------------------------------------------------------------------------
namespace unevaluated_decltype {

template <class T> int f (T t) pre (t.nonexistent ());

using D = decltype (f (0));
static_assert (__is_same (D, int));
}  // namespace unevaluated_decltype

// --------------------------------------------------------------------------
// sizeof, like decltype, does not odr-use.  Same proposition
// reached by a different operator, because the two take different paths to
// MarkFunctionReferenced.
// --------------------------------------------------------------------------
namespace unevaluated_sizeof {

template <class T> int f (T t) pre (t.nonexistent ());

static_assert (sizeof (f (0)) == sizeof (int));
}  // namespace unevaluated_sizeof

// --------------------------------------------------------------------------
// The noexcept operator is an unevaluated operand too.  This cell
// doubles as the first half of the exception-specification proposition: asking
// whether a call can throw must not drag the callee's contracts into
// existence.
// --------------------------------------------------------------------------
namespace unevaluated_noexcept_operator {

template <class T> int f (T t) pre (t.nonexistent ());

static_assert (noexcept (f (0)) == false);
}  // namespace unevaluated_noexcept_operator

// --------------------------------------------------------------------------
// A requires-expression names the call without odr-using it, so
// the concept must be satisfiable even though the predicate could never be
// substituted for this argument type.  The failure mode if contracts leak here
// is the worst of the set: a concept quietly reports false, or the program stops
// compiling somewhere that never mentioned contracts.
// --------------------------------------------------------------------------
namespace unevaluated_requires_expression {

template <class T> int f (T t) pre (t.nonexistent ());

template <class T> concept callable = requires (T t) { f (t); };
static_assert (callable<int>);
}  // namespace unevaluated_requires_expression

// --------------------------------------------------------------------------
// The same question inside a template argument, where the
// unevaluated operand is evaluated during template-argument deduction rather
// than in a function body.
// --------------------------------------------------------------------------
namespace unevaluated_in_template_argument {

template <class T> int f (T t) pre (t.nonexistent ());
template <int N> struct Box { static constexpr int value = N; };

static_assert (Box<sizeof (decltype (f (0)))>::value == sizeof (int));
}  // namespace unevaluated_in_template_argument

// --------------------------------------------------------------------------
// A contract is not part of the function type, so it cannot
// change what the return type deduces to or which specialization is formed.
// --------------------------------------------------------------------------
namespace contract_does_not_affect_deduction {

template <class T> T identity (T t) pre (true) { return t; }

static_assert (same<decltype (identity (0)), int>::value);
static_assert (same<decltype (identity (0.0)), double>::value);
}  // namespace contract_does_not_affect_deduction

// --------------------------------------------------------------------------
// Overload resolution ranks conversion sequences; contracts take
// no part.  The int overload must win for an int argument whether or not either
// candidate carries a contract.
// --------------------------------------------------------------------------
namespace contract_does_not_affect_overload_resolution {

constexpr int pick (int) { return 1; }
constexpr int pick (double) pre (true) { return 2; }

static_assert (pick (0) == 1);
static_assert (pick (0.0) == 2);
}  // namespace contract_does_not_affect_overload_resolution

// --------------------------------------------------------------------------
// Partial ordering of function templates considers the signature;
// a contract on the more specialized candidate must not change which one is
// chosen.
// --------------------------------------------------------------------------
namespace contract_does_not_affect_partial_ordering {

template <class T> constexpr int rank (T) { return 1; }
template <class T> constexpr int rank (T *) pre (true) { return 2; }

static_assert (rank (0) == 1);
static_assert (rank ((int *) nullptr) == 2);
}  // namespace contract_does_not_affect_partial_ordering

// --------------------------------------------------------------------------
// A concept that asks whether a call is valid must answer the same
// way for a contracted and an uncontracted function.
// --------------------------------------------------------------------------
namespace contract_does_not_affect_concept_satisfaction {

int plain (int);
int guarded (int) pre (true);

template <class F, class A> concept invocable = requires (F f, A a) { f (a); };
static_assert (invocable<decltype (plain), int>);
static_assert (invocable<decltype (guarded), int>);
}  // namespace contract_does_not_affect_concept_satisfaction

// --------------------------------------------------------------------------
// Taking the address of a contracted function yields the same type
// as for an uncontracted one, and the two are assignable to the same pointer --
// the clearest statement that contracts are outside the type system.
// --------------------------------------------------------------------------
namespace contract_does_not_affect_function_pointer_type {

int plain (int);
int guarded (int) pre (true);

static_assert (same<decltype (&plain), decltype (&guarded)>::value);
using FP = int (*) (int);
FP a = &plain;
FP b = &guarded;
}  // namespace contract_does_not_affect_function_pointer_type

// --------------------------------------------------------------------------
// A constraint that is not satisfied removes the candidate before
// its contracts are ever needed, so an ill-formed predicate on a
// never-selected specialization must stay harmless.  The distinction matters:
// constraint failure IS a substitution failure, contract ill-formedness is
// not.
// --------------------------------------------------------------------------
namespace unsatisfied_constraint_never_needs_the_contract {

template <class T> concept is_class = __is_class (T);

template <class T> requires is_class<T>
int f (T t) pre (t.nonexistent ()) { return 1; }
int f (...) { return 2; }

int use () { return f (0); }
}  // namespace unsatisfied_constraint_never_needs_the_contract

// --------------------------------------------------------------------------
// The positive control for the cell above: when the constraint IS
// satisfied the constrained candidate is selected and its contract substitutes
// cleanly.  Without this, the previous cell would pass just as well if
// constrained templates were being dropped altogether.
// --------------------------------------------------------------------------
namespace satisfied_constraint_with_valid_contract {

template <class T> concept is_class = __is_class (T);

template <class T> requires is_class<T>
int f (T t) pre (t.nonexistent ()) { return 1; }
int f (...) { return 2; }

int use () { return f (HasIt{}); }
}  // namespace satisfied_constraint_with_valid_contract
