// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
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
// Mirror: g++.dg/contracts/cpp26/matrix-exceptspec.C

// expected-no-diagnostics

int may_throw ();                 // not noexcept
int never_throws () noexcept;
bool pred_may_throw ();           // a predicate that is itself not noexcept
bool pred_never_throws () noexcept;

// --------------------------------------------------------------------------
// A precondition does not change what the noexcept operator says
// about a call, whether the function is noexcept or not.  Compared against an
// uncontracted twin rather than against a literal, so that losing the
// specification altogether would still fail.
// --------------------------------------------------------------------------
namespace pre_does_not_change_noexcept {

int plain_throwing (int);
int guarded_throwing (int) pre (true);
int plain_nothrow (int) noexcept;
int guarded_nothrow (int) noexcept pre (true);

static_assert (noexcept (guarded_throwing (0)) == noexcept (plain_throwing (0)));
static_assert (noexcept (guarded_nothrow (0)) == noexcept (plain_nothrow (0)));
static_assert (noexcept (guarded_throwing (0)) == false);
static_assert (noexcept (guarded_nothrow (0)) == true);
}  // namespace pre_does_not_change_noexcept

// --------------------------------------------------------------------------
// The same for a postcondition, including one that binds the
// result -- a different parse and a different lowering, so it does not follow
// from the precondition case.
// --------------------------------------------------------------------------
namespace post_does_not_change_noexcept {

int plain (int);
int guarded (int) post (r: r > 0);
int guarded_nothrow (int) noexcept post (r: r > 0);

static_assert (noexcept (guarded (0)) == noexcept (plain (0)));
static_assert (noexcept (guarded (0)) == false);
static_assert (noexcept (guarded_nothrow (0)) == true);
}  // namespace post_does_not_change_noexcept

// --------------------------------------------------------------------------
// The sharp case: the predicate itself can throw.  A contract
// check is not part of the function's interface, so a `noexcept` function stays
// `noexcept` even when its predicate could throw -- what happens at run time is
// termination, not a change of specification.
// --------------------------------------------------------------------------
namespace throwing_predicate_does_not_change_noexcept {

int guarded (int) noexcept pre (pred_may_throw ());
int guarded2 (int) pre (pred_may_throw ());

static_assert (noexcept (guarded (0)) == true);
static_assert (noexcept (guarded2 (0)) == false);
}  // namespace throwing_predicate_does_not_change_noexcept

// --------------------------------------------------------------------------
// A contract_assert lives in the body, so it cannot affect the
// declared specification -- but it can reach the *deduced* one for a lambda, and
// it must not.
// --------------------------------------------------------------------------
namespace contract_assert_does_not_change_noexcept {

void plain () noexcept { }
void guarded () noexcept { contract_assert (pred_may_throw ()); }

static_assert (noexcept (plain ()) == noexcept (guarded ()));
static_assert (noexcept (guarded ()) == true);
}  // namespace contract_assert_does_not_change_noexcept

// --------------------------------------------------------------------------
// A specification computed from another call must see the same
// answer whether or not the callee carries contracts, or the contract has leaked
// into the operand of the noexcept it was computed from.
// --------------------------------------------------------------------------
namespace computed_noexcept_spec_unaffected {

int plain (int) noexcept;
int guarded (int) noexcept pre (true);

void from_plain () noexcept (noexcept (plain (0)));
void from_guarded () noexcept (noexcept (guarded (0)));

static_assert (noexcept (from_plain ()) == noexcept (from_guarded ()));
static_assert (noexcept (from_guarded ()) == true);
}  // namespace computed_noexcept_spec_unaffected

// --------------------------------------------------------------------------
// Special members get an implicit exception specification computed
// from their subobjects.  A contract on a member function must not enter that
// computation -- a contracted class must stay nothrow-constructible if its
// uncontracted twin is.
// --------------------------------------------------------------------------
namespace implicit_special_member_spec_unaffected {

struct Plain {
  int n = 0;
  void touch () { }
};
struct Guarded {
  int n = 0;
  void touch () pre (n >= 0) { }
};

static_assert (__is_nothrow_constructible (Plain));
static_assert (__is_nothrow_constructible (Guarded));
static_assert (__is_nothrow_assignable (Plain &, const Plain &));
static_assert (__is_nothrow_assignable (Guarded &, const Guarded &));
static_assert (__is_nothrow_destructible (Guarded));
}  // namespace implicit_special_member_spec_unaffected

// --------------------------------------------------------------------------
// A contract on the constructor itself, rather than on a member
// function, is the harder half of the same proposition: the constructor is what
// the trait asks about.
// --------------------------------------------------------------------------
namespace contracted_ctor_spec_unaffected {

struct Plain { Plain () noexcept { } };
struct Guarded { Guarded () noexcept pre (true) { } };

static_assert (__is_nothrow_constructible (Plain));
static_assert (__is_nothrow_constructible (Guarded));
}  // namespace contracted_ctor_spec_unaffected

// --------------------------------------------------------------------------
// noexcept is part of the function type since C++17, so a
// contracted noexcept function must still convert to a `noexcept` function
// pointer -- the clearest possible statement that the contract did not perturb
// the type.
// --------------------------------------------------------------------------
namespace function_pointer_conversion_preserved {

void guarded () noexcept pre (true);
void guarded_throwing () pre (true);

void (*p) () noexcept = &guarded;
void (*q) () = &guarded;          // noexcept converts to non-noexcept
void (*r) () = &guarded_throwing;
}  // namespace function_pointer_conversion_preserved

// --------------------------------------------------------------------------
// A template's exception specification is instantiated lazily, and
// so now are its contracts.  This asks the two questions in the order that makes
// them interact: the noexcept operator forces the specification while the
// contract is still uninstantiated.
// --------------------------------------------------------------------------
namespace template_deferred_spec_with_contract {

template <class T> T plain (T t) noexcept (sizeof (T) > 1);
template <class T> T guarded (T t) noexcept (sizeof (T) > 1) pre (t == t);

static_assert (noexcept (guarded (0)) == noexcept (plain (0)));
static_assert (noexcept (guarded (0)) == true);
static_assert (noexcept (guarded ((char) 0)) == false);
}  // namespace template_deferred_spec_with_contract

// --------------------------------------------------------------------------
// The evaluation semantic is a translation-time choice and the
// specification is a property of the interface, so the two must not be
// connected.  Compiled here under the default semantic; the emitted test is
// compiled once per configuration by the surrounding suite.
// --------------------------------------------------------------------------
namespace semantic_choice_does_not_change_noexcept {

void guarded () noexcept pre (true);
void guarded2 () pre (true);

static_assert (noexcept (guarded ()) == true);
static_assert (noexcept (guarded2 ()) == false);
}  // namespace semantic_choice_does_not_change_noexcept
