// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,enforce %s

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
// constant-evaluated as a whole.
int global = cv(0);
// observe-warning@#cv {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'cv(0)'}}
// enforce-error@-3 {{constant initialization of 'global' failed when checking its contract assertions}}
// enforce-note@#cv {{precondition 'x > 0' evaluated to false}}
// enforce-note@-5 {{in call to 'cv(0)'}}
// enforce-note@-6 {{'global' is constant-initialized because its initializer is a constant expression when all contract assertions are ignored}}

namespace escalation {
// An immediate invocation that is not a constant expression is
// immediate-escalating. A contract violation doesn't make an expression
// non-constant, so it must not make the enclosing function an immediate
// function.
template <class T> constexpr int not_escalating(T) { return cv(0); } // #not-escalating
// observe-warning@#cv 2 {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 2 {{in call to 'cv(0)'}}
// enforce-error@-3 2 {{call to consteval function 'cv' is not a constant expression}}
// enforce-note@#cv 2 {{precondition 'x > 0' evaluated to false}}
// enforce-note@-5 2 {{in call to 'cv(0)'}}
int (*pointer)(int) = &not_escalating<int>;
// observe-note@-1 {{in instantiation of function template specialization 'escalation::not_escalating<int>' requested here}}
// enforce-note@-2 {{in instantiation of function template specialization 'escalation::not_escalating<int>' requested here}}

// The argument is not a constant expression here, so this function is an
// immediate function, and the contract assertions are checked when it is
// called.
template <class T> constexpr int escalating(T t) { return cv(t); } // #escalating
int a = escalating(1);
int b = escalating(0);
// observe-warning@#cv {{precondition 'x > 0' evaluated to false}}
// observe-note@#escalating {{in call to 'cv(0)'}}
// observe-note@-3 {{in call to 'escalating<int>(0)'}}
// enforce-error@-4 {{constant initialization of 'b' failed when checking its contract assertions}}
// enforce-note@#cv {{precondition 'x > 0' evaluated to false}}
// enforce-note@#escalating {{in call to 'cv(0)'}}
// enforce-note@-7 {{in call to 'escalating<int>(0)'}}
// enforce-note@-8 {{'b' is constant-initialized because}}
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
