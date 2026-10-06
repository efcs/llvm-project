// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s -fexperimental-new-constant-interpreter

// C++26 [basic.contract.eval]: A contract assertion that is evaluated in a
// manifestly constant-evaluated context
//  - has no effect with the ignore semantic,
//  - produces a diagnostic (a warning), but doesn't prevent the evaluation,
//    if it is violated with the observe semantic, and
//  - makes the program ill-formed if it is violated with the enforce or the
//    quick-enforce semantic.

// ignore-no-diagnostics

constexpr int pre_fn(int x) pre(x > 0) { return x; }                       // #pre
constexpr int post_fn(int x) post(r : r > 0) { return x; }                 // #post
constexpr int assert_fn(int x) { contract_assert(x > 0); return x; }       // #assert

// Contract assertions that hold are never diagnosed.
static_assert(pre_fn(1) == 1);
static_assert(post_fn(1) == 1);
static_assert(assert_fn(1) == 1);
constexpr int ok = pre_fn(1) + post_fn(2) + assert_fn(3);
static_assert(ok == 6);

namespace kinds {
// Each kind of contract assertion is checked and names the violated
// assertion.
constexpr int a = pre_fn(0);
// observe-warning@#pre {{contract violation with the 'observe' evaluation semantic: precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'pre_fn(0)'}}
// terminating-error@-3 {{constexpr variable 'a' must be initialized by a constant expression}}
// terminating-note-re@#pre {{contract violation with the '{{(quick_)?enforce}}' evaluation semantic: precondition 'x > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'pre_fn(0)'}}

constexpr int b = post_fn(0);
// observe-warning@#post {{contract violation with the 'observe' evaluation semantic: postcondition 'r : r > 0' evaluated to false}}
// observe-note@-2 {{in call to 'post_fn(0)'}}
// terminating-error@-3 {{constexpr variable 'b' must be initialized by a constant expression}}
// terminating-note-re@#post {{contract violation with the '{{(quick_)?enforce}}' evaluation semantic: postcondition 'r : r > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'post_fn(0)'}}

constexpr int c = assert_fn(0);
// observe-warning@#assert {{contract violation with the 'observe' evaluation semantic: contract assertion 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'assert_fn(0)'}}
// terminating-error@-3 {{constexpr variable 'c' must be initialized by a constant expression}}
// terminating-note-re@#assert {{contract violation with the '{{(quick_)?enforce}}' evaluation semantic: contract assertion 'x > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'assert_fn(0)'}}
} // namespace kinds

namespace order {
// The first violated contract assertion with a terminating semantic stops the
// evaluation. With the observe semantic the evaluation continues, and every
// violation is diagnosed.
constexpr int f(int x)
    pre(x > 0)        // #order-pre1
    pre(x > 1)        // #order-pre2
    post(r : r > 2) { // #order-post
  contract_assert(x > 3); // #order-assert
  return x;
}
constexpr int a = f(0);
// observe-warning@#order-pre1 {{precondition 'x > 0' evaluated to false}}
// observe-warning@#order-pre2 {{precondition 'x > 1' evaluated to false}}
// observe-warning@#order-assert {{contract assertion 'x > 3' evaluated to false}}
// observe-warning@#order-post {{postcondition 'r : r > 2' evaluated to false}}
// observe-note@-5 4 {{in call to 'f(0)'}}
// terminating-error@-6 {{constexpr variable 'a' must be initialized by a constant expression}}
// terminating-note@#order-pre1 {{precondition 'x > 0' evaluated to false}}
// terminating-note@-8 {{in call to 'f(0)'}}

constexpr int b = f(3);
// observe-warning@#order-assert {{contract assertion 'x > 3' evaluated to false}}
// observe-note@-2 {{in call to 'f(3)'}}
// terminating-error@-3 {{constexpr variable 'b' must be initialized by a constant expression}}
// terminating-note@#order-assert {{contract assertion 'x > 3' evaluated to false}}
// terminating-note@-5 {{in call to 'f(3)'}}

// The call stack leads to the violated contract assertion.
constexpr int g(int x) { return f(x) + 1; } // #order-g
constexpr int c = g(1);
// observe-warning@#order-pre2 {{precondition 'x > 1' evaluated to false}}
// observe-warning@#order-assert {{contract assertion 'x > 3' evaluated to false}}
// observe-warning@#order-post {{postcondition 'r : r > 2' evaluated to false}}
// observe-note@#order-g 3 {{in call to 'f(1)'}}
// observe-note@-5 3 {{in call to 'g(1)'}}
// terminating-error@-6 {{constexpr variable 'c' must be initialized by a constant expression}}
// terminating-note@#order-pre2 {{precondition 'x > 1' evaluated to false}}
// terminating-note@#order-g {{in call to 'f(1)'}}
// terminating-note@-9 {{in call to 'g(1)'}}
} // namespace order

namespace contexts {
// All manifestly constant-evaluated contexts check contract assertions.
constexpr int f(int x) pre(x > 0) { return x + 1; } // #ctx

static_assert(f(0) == 1);
// observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'f(0)'}}
// terminating-error@-3 {{static assertion expression is not an integral constant expression}}
// terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'f(0)'}}

template <int N> struct X {};
X<f(0)> x;
// observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'f(0)'}}
// terminating-error@-3 {{non-type template argument is not a constant expression}}
// terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'f(0)'}}

enum E {
  e1 = f(0)
  // observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
  // observe-note@-2 {{in call to 'f(0)'}}
  // terminating-error@-3 {{expression is not an integral constant expression}}
  // terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
  // terminating-note@-5 {{in call to 'f(0)'}}
};

struct BitField {
  int b : f(0);
  // observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
  // observe-note@-2 {{in call to 'f(0)'}}
  // terminating-error@-3 {{expression is not an integral constant expression}}
  // terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
  // terminating-note@-5 {{in call to 'f(0)'}}
};

int switch_case(int v) {
  switch (v) {
  case f(0):
    // observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
    // observe-note@-2 {{in call to 'f(0)'}}
    // terminating-error@-3 {{case value is not a constant expression}}
    // terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
    // terminating-note@-5 {{in call to 'f(0)'}}
    return 1;
  }
  return 0;
}

void noexcept_spec() noexcept(f(0) == 1);
// observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'f(0)'}}
// terminating-error@-3 {{noexcept specifier argument is not a constant expression}}
// terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'f(0)'}}

struct Explicit {
  explicit(f(0) == 1) Explicit(int);
  // observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
  // observe-note@-2 {{in call to 'f(0)'}}
  // terminating-error@-3 {{explicit specifier argument is not a constant expression}}
  // terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
  // terminating-note@-5 {{in call to 'f(0)'}}
};

alignas(f(0) * 8) int aligned;
// observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'f(0)'}}
// terminating-error@-3 {{'aligned' attribute requires integer constant}}
// terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'f(0)'}}

void constexpr_if() {
  if constexpr (f(0) == 1) {
    // observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
    // observe-note@-2 {{in call to 'f(0)'}}
    // terminating-error@-3 {{constexpr if condition is not a constant expression}}
    // terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
    // terminating-note@-5 {{in call to 'f(0)'}}
  }
}

void local_constexpr() {
  constexpr int local = f(0);
  // observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
  // observe-note@-2 {{in call to 'f(0)'}}
  // terminating-error@-3 {{constexpr variable 'local' must be initialized by a constant expression}}
  // terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
  // terminating-note@-5 {{in call to 'f(0)'}}
}

template <class T> constexpr int templated = f(sizeof(T) - 1);
// observe-warning@#ctx {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'f(0)'}}
// terminating-error@-3 {{constexpr variable 'templated<char>' must be initialized by a constant expression}}
// terminating-note@#ctx {{precondition 'x > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'f(0)'}}
int use_templated = templated<int> + templated<char>;
// observe-note@-1 {{in instantiation of variable template specialization 'contexts::templated<char>' requested here}}
// terminating-note@-2 {{in instantiation of variable template specialization 'contexts::templated<char>' requested here}}
} // namespace contexts
