// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s

// P2900R14 [3.5.12]: "evaluating a contract assertion in and of itself never
// makes an expression ineligible to be a core constant expression". A
// contract violation in a manifestly constant-evaluated context makes the
// program ill-formed ([basic.contract.eval]p8) instead. In particular it is
// not a substitution failure: which overload is selected, or whether a
// constraint is satisfied, must not depend on contract assertions.

// ignore-no-diagnostics

constexpr int f(int x) pre(x > 0) { return x; } // #f

struct Zero { static constexpr int value = 0; };
struct One { static constexpr int value = 1; };
struct None {};

namespace template_argument {
template <int N> struct X { using type = int; };
template <class T> typename X<f(T::value)>::type g(T) { return 1; } // #g
template <class T> long g(...) { return 2; }

// No contract violation: the first overload is viable.
int a = g<One>(One{});
// A real substitution failure: the second overload is selected.
long b = g<None>(None{});
static_assert(__is_same(decltype(g<None>(None{})), long));

// A contract violation is not a substitution failure. With the observe
// semantic the violation is diagnosed for each substitution into the
// declaration, which happens once for the explicitly-specified and once for
// the deduced template arguments.
auto c = sizeof(g<Zero>(Zero{}));
// observe-warning@#f 2 {{contract violation with the 'observe' evaluation semantic: precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{while substituting explicitly-specified template arguments into function template 'g'}}
// observe-note@-3 {{while substituting deduced template arguments into function template 'g' [with T = Zero]}}
// observe-note@#g 2 {{in call to 'f(0)'}}
// terminating-error@#g {{contract violation in a constant expression}}
// terminating-note@-6 {{while substituting explicitly-specified template arguments into function template 'g'}}
// terminating-note-re@#f {{contract violation with the '{{(quick_)?enforce}}' evaluation semantic: precondition 'x > 0' evaluated to false}}
// terminating-note@#g {{in call to 'f(0)'}}
} // namespace template_argument

namespace array_bound {
template <class T> char (*h(T))[f(T::value)]; // #h
template <class T> long h(...);
static_assert(sizeof(*h<One>(One{})) == 1);
static_assert(sizeof(h<None>(None{})) == sizeof(long));

auto d = sizeof(h<Zero>(Zero{}));
// observe-warning@#f {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{while substituting explicitly-specified template arguments into function template 'h'}}
// observe-note@#h {{in call to 'f(0)'}}
// terminating-error@#h {{contract violation in a constant expression}}
// terminating-note@-5 {{while substituting explicitly-specified template arguments into function template 'h'}}
// terminating-note@#f {{precondition 'x > 0' evaluated to false}}
// terminating-note@#h {{in call to 'f(0)'}}
} // namespace array_bound

namespace concepts {
// "Concepts do not see contracts."
template <int N> struct X {};
template <class T> concept C = requires { typename X<f(T::value)>; }; // #C
static_assert(C<One>);
static_assert(!C<None>);

bool e = C<Zero>;
// observe-warning@#f {{precondition 'x > 0' evaluated to false}}
// observe-note@#C {{in instantiation of requirement here}}
// observe-note@#C {{while substituting template arguments into constraint expression here}}
// observe-note@-4 {{while checking the satisfaction of concept 'C<Zero>' requested here}}
// observe-note@#C {{in call to 'f(0)'}}
// terminating-error@#C {{contract violation in a constant expression}}
// terminating-note@#C {{in instantiation of requirement here}}
// terminating-note@#C {{while substituting template arguments into constraint expression here}}
// terminating-note@-9 {{while checking the satisfaction of concept 'C<Zero>' requested here}}
// terminating-note@#f {{precondition 'x > 0' evaluated to false}}
// terminating-note@#C {{in call to 'f(0)'}}

// An atomic constraint is a constant expression of type bool.
template <class T> concept Positive = f(T::value) > 0; // #Positive
static_assert(Positive<One>);
bool g = Positive<Zero>;
// observe-warning@#f {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{while checking the satisfaction of concept 'Positive<Zero>' requested here}}
// observe-note@#Positive {{in call to 'f(0)'}}
// terminating-error@#Positive {{substitution into constraint expression resulted in a non-constant expression}}
// terminating-note@-5 {{while checking the satisfaction of concept 'Positive<Zero>' requested here}}
// terminating-note@#f {{precondition 'x > 0' evaluated to false}}
// terminating-note@#Positive {{in call to 'f(0)'}}
} // namespace concepts
