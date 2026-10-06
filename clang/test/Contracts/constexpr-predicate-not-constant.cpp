// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s

// C++26 [basic.contract.eval]p7: A contract violation occurs when [...] the
// evaluation of the predicate is performed in a context that is manifestly
// constant-evaluated and the predicate is not a core constant expression.
//
// That the predicate is not a core constant expression does not make the
// enclosing expression non-constant by itself: with the observe semantic the
// evaluation continues after the contract assertion.

// ignore-no-diagnostics

int runtime(int); // #runtime

namespace non_constexpr_call {
constexpr int f(int x) pre(runtime(x) > 0) { return x; } // #ncc
constexpr int a = f(1);
// observe-warning@#ncc {{contract violation with the 'observe' evaluation semantic: precondition 'runtime(x) > 0' is not a constant expression}}
// observe-note@#ncc {{non-constexpr function 'runtime' cannot be used in a constant expression}}
// observe-note@-3 {{in call to 'f(1)'}}
// observe-note@#runtime {{declared here}}
// terminating-error@-5 {{constexpr variable 'a' must be initialized by a constant expression}}
// terminating-note-re@#ncc {{contract violation with the '{{(quick_)?enforce}}' evaluation semantic: precondition 'runtime(x) > 0' is not a constant expression}}
// terminating-note@#ncc {{non-constexpr function 'runtime' cannot be used in a constant expression}}
// terminating-note@-8 {{in call to 'f(1)'}}
// terminating-note@#runtime {{declared here}}

// The contract assertion is checked at run time here.
int at_runtime(int x) { return f(x) + f(1); }
} // namespace non_constexpr_call

namespace undefined_behavior {
constexpr int f(int x) post(r : 1 / (r - 1)) { return x; } // #ub
static_assert(f(2) == 2);
constexpr int a = f(1);
// observe-warning@#ub {{postcondition 'r : 1 / (r - 1)' is not a constant expression}}
// observe-note@#ub {{division by zero}}
// observe-note@-3 {{in call to 'f(1)'}}
// terminating-error@-4 {{constexpr variable 'a' must be initialized by a constant expression}}
// terminating-note@#ub {{postcondition 'r : 1 / (r - 1)' is not a constant expression}}
// terminating-note@#ub {{division by zero}}
// terminating-note@-7 {{in call to 'f(1)'}}

constexpr int g(const int *p) { contract_assert(*p == 0); return 1; } // #null
constexpr int b = g(nullptr);
// observe-warning@#null {{contract assertion '*p == 0' is not a constant expression}}
// observe-note-re@#null {{{{read of dereferenced null pointer|dereferencing a null pointer}} is not allowed in a constant expression}}
// observe-note@-3 {{in call to 'g(nullptr)'}}
// terminating-error@-4 {{constexpr variable 'b' must be initialized by a constant expression}}
// terminating-note@#null {{contract assertion '*p == 0' is not a constant expression}}
// terminating-note-re@#null {{{{read of dereferenced null pointer|dereferencing a null pointer}} is not allowed in a constant expression}}
// terminating-note@-7 {{in call to 'g(nullptr)'}}
} // namespace undefined_behavior

namespace side_effects_are_discarded {
// What a predicate that could not be evaluated leaves behind does not affect
// the enclosing evaluation.
struct S {
  int *p;
  constexpr S(int v) : p(new int(v)) {}
  constexpr ~S() { delete p; }
  constexpr bool ok() const { return *p > 0; }
};
// The temporary is not destroyed, and so its allocation is not freed, if
// the evaluation of the predicate is abandoned.
constexpr int f(int x) pre(S(x).ok() && runtime(x)) { return x; } // #leak1
constexpr int a = f(1);
// observe-warning@#leak1 {{precondition 'S(x).ok() && runtime(x)' is not a constant expression}}
// observe-note@#leak1 {{non-constexpr function 'runtime' cannot be used in a constant expression}}
// observe-note@-3 {{in call to 'f(1)'}}
// observe-note@#runtime {{declared here}}
// terminating-error@-5 {{constexpr variable 'a' must be initialized by a constant expression}}
// terminating-note@#leak1 {{precondition 'S(x).ok() && runtime(x)' is not a constant expression}}
// terminating-note@#leak1 {{non-constexpr function 'runtime' cannot be used in a constant expression}}
// terminating-note@-8 {{in call to 'f(1)'}}
// terminating-note@#runtime {{declared here}}

// A predicate that leaks memory is not a core constant expression.
constexpr int g(int x) pre(*new int(x) == x) { return x; } // #leak2
constexpr int b = g(1);
// observe-warning@#leak2 {{precondition '*new int(x) == x' is not a constant expression}}
// observe-note@#leak2 {{allocation performed here was not deallocated}}
// observe-note@-3 {{in call to 'g(1)'}}
// terminating-error@-4 {{constexpr variable 'b' must be initialized by a constant expression}}
// terminating-note@#leak2 {{precondition '*new int(x) == x' is not a constant expression}}
// terminating-note@#leak2 {{allocation performed here was not deallocated}}
// terminating-note@-7 {{in call to 'g(1)'}}

// The contract assertion is evaluated again in each iteration.
constexpr int loop(int n) {
  int sum = 0;
  for (int i = 0; i < n; ++i) {
    contract_assert(S(i + 1).ok() && (i < 1 || runtime(i))); // #loop
    sum += i;
  }
  return sum;
}
static_assert(loop(1) == 0);
constexpr int c = loop(3);
// observe-warning@#loop 2 {{contract assertion 'S(i + 1).ok() && (i < 1 || runtime(i))' is not a constant expression}}
// observe-note@#loop 2 {{non-constexpr function 'runtime' cannot be used in a constant expression}}
// observe-note@-3 2 {{in call to 'loop(3)'}}
// observe-note@#runtime 2 {{declared here}}
// terminating-error@-5 {{constexpr variable 'c' must be initialized by a constant expression}}
// terminating-note@#loop {{contract assertion 'S(i + 1).ok() && (i < 1 || runtime(i))' is not a constant expression}}
// terminating-note@#loop {{non-constexpr function 'runtime' cannot be used in a constant expression}}
// terminating-note@-8 {{in call to 'loop(3)'}}
// terminating-note@#runtime {{declared here}}
} // namespace side_effects_are_discarded

namespace speculative_evaluation {
// A predicate that is evaluated speculatively as part of another predicate.
constexpr bool check(int x) pre(x != 0) { return x > 0; } // #spec
constexpr int f(int x) pre(__builtin_constant_p(check(x)) ? check(x) : true) { return x; } // #spec-f
static_assert(f(1) == 1);
constexpr int a = f(-1);
// observe-warning@#spec-f {{precondition '__builtin_constant_p(check(x)) ? check(x) : true' evaluated to false}}
// observe-note@-2 {{in call to 'f(-1)'}}
// terminating-error@-3 {{constexpr variable 'a' must be initialized by a constant expression}}
// terminating-note@#spec-f {{precondition '__builtin_constant_p(check(x)) ? check(x) : true' evaluated to false}}
// terminating-note@-5 {{in call to 'f(-1)'}}

// A contract violation during speculative evaluation is not diagnosed and
// doesn't make the program ill-formed, whatever the evaluation semantic is.
// With a terminating semantic the operand can't be evaluated at compile
// time; with the observe semantic it can.
constexpr bool violated = __builtin_constant_p(check(0));
constexpr int g(int x) pre(__builtin_constant_p(check(0)) ? x > 0 : true) { return x; } // #spec-g
constexpr int b = g(1);
constexpr int c = g(0);
// observe-warning@#spec-g {{precondition '__builtin_constant_p(check(0)) ? x > 0 : true' evaluated to false}}
// observe-note@-2 {{in call to 'g(0)'}}
} // namespace speculative_evaluation
