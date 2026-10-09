// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s -fexperimental-new-constant-interpreter

// C++26 [expr.const]: Whether an object has constant destruction is determined
// with all contract assertions using the ignore semantic, like whether a
// variable is constant-initialized ([expr.const.init]p4). If the destructor
// evaluates a contract assertion with a checking semantic, the object does
// not have constant destruction after all: the destructor is called at run
// time and checks the contract assertion then ([basic.stc.static]p2 doesn't
// allow eliminating a destructor with side effects) -- unless the variable is
// required to have constant destruction (a constexpr variable, a class
// template argument). Then the destruction is evaluated with the contract
// assertions checked: a violation with the observe semantic is diagnosed and
// the destruction stays constant; a violation with a terminating semantic
// makes the program ill-formed ([basic.contract.eval]p8).

// ignore-no-diagnostics

struct D {
  int v;
  constexpr D(int v) : v(v) {}
  constexpr ~D() { contract_assert(v > 0); } // #D
};

struct P {
  int v;
  constexpr P(int v) : v(v) {}
  constexpr ~P() pre(v > 0) post(true) {} // #P
};

namespace constexpr_variable {
constexpr D ok(1);
constexpr P p_ok(1);

constexpr D bad(0); // #bad
// observe-warning@#D {{contract violation with the 'observe' evaluation semantic: contract assertion 'v > 0' evaluated to false}}
// observe-note@#bad {{in call to 'bad.~D()'}}
// terminating-error@#bad {{constexpr variable 'bad' must have constant destruction}}
// terminating-note-re@#D {{contract violation with the '{{(quick_)?enforce}}' evaluation semantic: contract assertion 'v > 0' evaluated to false}}
// terminating-note@#bad {{in call to 'bad.~D()'}}

constexpr P p_bad(0); // #p-bad
// observe-warning@#P {{contract violation with the 'observe' evaluation semantic: precondition 'v > 0' evaluated to false}}
// observe-note@#p-bad {{in call to 'p_bad.~P()'}}
// terminating-error@#p-bad {{constexpr variable 'p_bad' must have constant destruction}}
// terminating-note-re@#P {{contract violation with the '{{(quick_)?enforce}}' evaluation semantic: precondition 'v > 0' evaluated to false}}
// terminating-note@#p-bad {{in call to 'p_bad.~P()'}}

constexpr D arr[2] = {1, 0}; // #arr
// observe-warning@#D {{contract assertion 'v > 0' evaluated to false}}
// observe-note@#arr {{in call to 'arr[1].~D()'}}
// terminating-error@#arr {{constexpr variable 'arr' must have constant destruction}}
// terminating-note@#D {{contract assertion 'v > 0' evaluated to false}}
// terminating-note@#arr {{in call to 'arr[1].~D()'}}
} // namespace constexpr_variable

namespace static_storage {
// These variables are constant-initialized, and their destruction would be
// constant if the contract assertion were ignored. It is not: the destructor
// has to be called at run time, where it checks the contract assertion with
// whatever semantic it has then. Nothing is checked at compile time, since
// the variables are not required to have constant destruction. (Reading a
// member of a variable that is not constexpr isn't a constant expression
// during its destruction anyway.)
static D s(0);
D g(0);
thread_local D t(0);
void f() {
  static D local(0);
}
D fine(1);

// The destructor of a variable that is not constant-initialized is called at
// run time in any case.
int runtime();
D dynamic(runtime());
} // namespace static_storage

namespace template_argument {
// A class template argument is required to have constant destruction.
template <D d> struct T {};
T<D(1)> ok;
T<D(0)> bad; // #t-bad
// observe-warning@#D {{contract assertion 'v > 0' evaluated to false}}
// observe-note@#t-bad {{in call to 'D(0).~D()'}}
// terminating-error@#t-bad {{non-type template argument is not a constant expression}}
// terminating-note@#D {{contract assertion 'v > 0' evaluated to false}}
// terminating-note@#t-bad {{in call to 'D(0).~D()'}}
} // namespace template_argument
