// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -fcontract-group-evaluation-semantic=lib=enforce,quiet=ignore -verify=expected,warn %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -fcontract-group-evaluation-semantic=lib=enforce,quiet=ignore -Wno-contract-violation -verify=expected %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -fcontract-group-evaluation-semantic=lib=enforce,quiet=ignore -Werror=contract-violation -verify=expected,werror %s

// It is implementation-defined which evaluation semantic is used for any
// given evaluation of a contract assertion ([basic.contract.eval]p2). Here
// contract assertions are observed by default, enforced in the group 'lib',
// and ignored in the group 'quiet'.

constexpr bool observed(int x) pre(x > 0) { return true; }                                   // #observed
constexpr bool enforced(int x) pre [[clang::contract_group("lib")]] (x > 0) { return true; } // #enforced
constexpr bool ignored(int x) pre [[clang::contract_group("quiet")]] (x > 0) { return true; }

constexpr bool a = observed(0);
// warn-warning@#observed {{contract violation with the 'observe' evaluation semantic: precondition 'x > 0' evaluated to false}}
// warn-note@-2 {{in call to 'observed(0)'}}
// werror-error@#observed {{contract violation with the 'observe' evaluation semantic: precondition 'x > 0' evaluated to false}}
// werror-note@-4 {{in call to 'observed(0)'}}

constexpr bool b = enforced(0);
// expected-error@-1 {{constexpr variable 'b' must be initialized by a constant expression}}
// expected-note@#enforced {{contract violation with the 'enforce' evaluation semantic: precondition 'x > 0' evaluated to false}}
// expected-note@-3 {{in call to 'enforced(0)'}}

constexpr bool c = ignored(0);

// One evaluation can violate several contract assertions with different
// semantics.
constexpr bool d = observed(0) && ignored(0) && enforced(0);
// warn-warning@#observed {{precondition 'x > 0' evaluated to false}}
// warn-note@-2 {{in call to 'observed(0)'}}
// werror-error@#observed {{precondition 'x > 0' evaluated to false}}
// werror-note@-4 {{in call to 'observed(0)'}}
// expected-error@-5 {{constexpr variable 'd' must be initialized by a constant expression}}
// expected-note@#enforced {{contract violation with the 'enforce' evaluation semantic: precondition 'x > 0' evaluated to false}}
// expected-note@-7 {{in call to 'enforced(0)'}}

namespace nested {
// A contract assertion that is violated while the predicate of another
// contract assertion is evaluated.

// The predicate of the outer contract assertion is not a constant expression
// if an enforced contract assertion is violated during its evaluation, but
// the program is ill-formed whatever the semantic of the outer contract
// assertion is.
constexpr int observes_enforced(int x) pre(enforced(x)) { return x; } // #oe
constexpr int a = observes_enforced(0);
// expected-error@-1 {{constexpr variable 'a' must be initialized by a constant expression}}
// expected-note@#enforced {{contract violation with the 'enforce' evaluation semantic: precondition 'x > 0' evaluated to false}}
// expected-note@#oe {{in call to 'enforced(0)'}}
// expected-note@-4 {{in call to 'observes_enforced(0)'}}

// The predicate of the outer contract assertion holds if the violation of an
// observed contract assertion doesn't prevent its evaluation.
constexpr int enforces_observed(int x) pre [[clang::contract_group("lib")]] (observed(x)) { return x; } // #eo
constexpr int b = enforces_observed(0);
// warn-warning@#observed {{precondition 'x > 0' evaluated to false}}
// warn-note@#eo {{in call to 'observed(0)'}}
// warn-note@-3 {{in call to 'enforces_observed(0)'}}
// werror-error@#observed {{precondition 'x > 0' evaluated to false}}
// werror-note@#eo {{in call to 'observed(0)'}}
// werror-note@-6 {{in call to 'enforces_observed(0)'}}

constexpr int observes_observed(int x) pre(observed(x)) post(r : !observed(r)) { return x; } // #oo
constexpr int c = observes_observed(0);
// warn-warning@#observed 2 {{precondition 'x > 0' evaluated to false}}
// warn-warning@#oo {{postcondition 'r : !observed(r)' evaluated to false}}
// warn-note@#oo 2 {{in call to 'observed(0)'}}
// warn-note@-4 3 {{in call to 'observes_observed(0)'}}
// werror-error@#observed 2 {{precondition 'x > 0' evaluated to false}}
// werror-error@#oo {{postcondition 'r : !observed(r)' evaluated to false}}
// werror-note@#oo 2 {{in call to 'observed(0)'}}
// werror-note@-8 3 {{in call to 'observes_observed(0)'}}

// An ignored contract assertion is not evaluated at all.
constexpr int ignores_enforced(int x) pre [[clang::contract_group("quiet")]] (enforced(x)) { return x; }
constexpr int d = ignores_enforced(0);
} // namespace nested
