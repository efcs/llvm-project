// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,enforce %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,enforce %s -fexperimental-new-constant-interpreter

// An immediate invocation is manifestly constant-evaluated, so the contract
// assertions that it evaluates are checked during translation.

// ignore-no-diagnostics

consteval int cv(int x) pre(x > 0) post(r : r < 10) { return x; } // #cv

void runtime_function(int n) {
  int ok = cv(1);
  int a = cv(0);
  // observe-warning@#cv {{contract violation with the 'observe' evaluation semantic: precondition 'x > 0' evaluated to false}}
  // observe-note@-2 {{in call to 'cv(0)'}}
  // enforce-error@-3 {{call to consteval function 'cv' is not a constant expression}}
  // enforce-note@#cv {{contract violation with the 'enforce' evaluation semantic: precondition 'x > 0' evaluated to false}}
  // enforce-note@-5 {{in call to 'cv(0)'}}
  int b = cv(10);
  // observe-warning@#cv {{postcondition 'r : r < 10' evaluated to false}}
  // observe-note@-2 {{in call to 'cv(10)'}}
  // enforce-error@-3 {{call to consteval function 'cv' is not a constant expression}}
  // enforce-note@#cv {{postcondition 'r : r < 10' evaluated to false}}
  // enforce-note@-5 {{in call to 'cv(10)'}}
}

// The initializer of a constant-initialized variable is manifestly
// constant-evaluated as a whole, which includes the immediate invocation. A
// violation is diagnosed for the immediate invocation, which is ill-formed
// whether or not the variable is constant-initialized.
int global = cv(0); // #global
// observe-warning@#cv {{precondition 'x > 0' evaluated to false}}
// observe-note@#global {{in call to 'cv(0)'}}
// enforce-error@#global {{call to consteval function 'cv' is not a constant expression}}
// enforce-note@#cv {{precondition 'x > 0' evaluated to false}}
// enforce-note@#global {{in call to 'cv(0)'}}

namespace escalation {
// An immediate invocation that is not a constant expression is
// immediate-escalating. A contract violation doesn't make an expression
// non-constant, so it must not make the enclosing function an immediate
// function.
template <class T> constexpr int not_escalating(T) { return cv(0); } // #not-escalating
// The immediate invocation is evaluated for the template and again for its
// instantiation; the identical violation is diagnosed once.
// observe-warning@#cv {{precondition 'x > 0' evaluated to false}}
// observe-note@#not-escalating {{in call to 'cv(0)'}}
// enforce-error@#not-escalating 2 {{call to consteval function 'cv' is not a constant expression}}
// enforce-note@#cv 2 {{precondition 'x > 0' evaluated to false}}
// enforce-note@#not-escalating 2 {{in call to 'cv(0)'}}
int (*pointer)(int) = &not_escalating<int>;
// enforce-note@-1 {{in instantiation of function template specialization 'escalation::not_escalating<int>' requested here}}

// The argument is not a constant expression here, so this function is an
// immediate function, and the contract assertions are checked when it is
// called.
template <class T> constexpr int escalating(T t) { return cv(t); } // #escalating
int a = escalating(1);
int b = escalating(0); // #b
// observe-warning@#cv {{precondition 'x > 0' evaluated to false}}
// observe-note@#escalating {{in call to 'cv(0)'}}
// observe-note@#b {{in call to 'escalating<int>(0)'}}
// enforce-error@#b {{call to immediate function 'escalation::escalating<int>' is not a constant expression}}
// enforce-note@#cv {{precondition 'x > 0' evaluated to false}}
// enforce-note@#escalating {{in call to 'cv(0)'}}
// enforce-note@#b {{in call to 'escalating<int>(0)'}}
} // namespace escalation

namespace consteval_if {
constexpr int f(int x) {
  if consteval {
    contract_assert(x > 0); // #consteval-if
  }
  return x;
}
constexpr int a = f(0);
// observe-warning@#consteval-if {{contract assertion 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'f(0)'}}
// enforce-error@-3 {{constexpr variable 'a' must be initialized by a constant expression}}
// enforce-note@#consteval-if {{contract assertion 'x > 0' evaluated to false}}
// enforce-note@-5 {{in call to 'f(0)'}}
int runtime(int x) { return f(x) + f(0); }

// std::is_constant_evaluated() is true in the predicate of a contract
// assertion that is evaluated during constant evaluation.
constexpr int g(int x) pre(__builtin_is_constant_evaluated()) { return x; }
constexpr int b = g(1);
static_assert(g(1) == 1);
} // namespace consteval_if

namespace initializer {
// An immediate invocation in the initializer of a variable with static
// storage duration. The variable is constant-initialized (the initializer is
// a constant expression with the contract assertions ignored), which makes
// the initializer manifestly constant-evaluated, so the immediate invocation
// is evaluated as part of it. A contract violation with a terminating
// semantic is diagnosed for the immediate invocation, which is ill-formed by
// itself, not as a failed constant initialization.
consteval int ce(int x) post(r : r > 0) { return x; } // #ce
int u = ce(0); // #u
// observe-warning@#ce {{postcondition 'r : r > 0' evaluated to false}}
// observe-note@#u {{in call to 'ce(0)'}}
// enforce-error@#u {{call to consteval function 'initializer::ce' is not a constant expression}}
// enforce-note@#ce {{postcondition 'r : r > 0' evaluated to false}}
// enforce-note@#u {{in call to 'ce(0)'}}
int u2 = ce(0) + ce(1); // #u2
// observe-warning@#ce {{postcondition 'r : r > 0' evaluated to false}}
// observe-note@#u2 {{in call to 'ce(0)'}}
// enforce-error@#u2 {{call to consteval function 'initializer::ce' is not a constant expression}}
// enforce-note@#ce {{postcondition 'r : r > 0' evaluated to false}}
// enforce-note@#u2 {{in call to 'ce(0)'}}
int fine = ce(1);
} // namespace initializer
