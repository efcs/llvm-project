// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s -fexperimental-new-constant-interpreter

// C++26 [basic.contract.eval]p8: a contract violation with a terminating
// semantic in a manifestly constant-evaluated context makes the program
// ill-formed. That holds in the contexts where Clang would otherwise accept
// an expression that is not a constant expression, by folding it or by giving
// up on it: an array bound that is not a constant expression normally forms a
// variable length array (a Clang extension).

// ignore-no-diagnostics

constexpr int f(int x) pre(x > 0) { return x + 1; } // #f

int s = sizeof(int[f(0)]); // #sizeof
// observe-warning@#f {{precondition 'x > 0' evaluated to false}}
// observe-note@#sizeof {{in call to 'f(0)'}}
// terminating-error@#sizeof {{contract violation in a constant expression}}
// terminating-note@#f {{precondition 'x > 0' evaluated to false}}
// terminating-note@#sizeof {{in call to 'f(0)'}}

void local_array() {
  int a[f(0)]; // #local
  // observe-warning@#f {{precondition 'x > 0' evaluated to false}}
  // observe-note@#local {{in call to 'f(0)'}}
  // terminating-error@#local {{contract violation in a constant expression}}
  // terminating-note@#f {{precondition 'x > 0' evaluated to false}}
  // terminating-note@#local {{in call to 'f(0)'}}
  (void)a;
}

int global_array[f(0)]; // #global
// observe-warning@#f {{precondition 'x > 0' evaluated to false}}
// observe-note@#global {{in call to 'f(0)'}}
// terminating-error@#global {{contract violation in a constant expression}}
// terminating-note@#f {{precondition 'x > 0' evaluated to false}}
// terminating-note@#global {{in call to 'f(0)'}}

using array_type = int[f(0)]; // #alias
// observe-warning@#f {{precondition 'x > 0' evaluated to false}}
// observe-note@#alias {{in call to 'f(0)'}}
// terminating-error@#alias {{contract violation in a constant expression}}
// terminating-note@#f {{precondition 'x > 0' evaluated to false}}
// terminating-note@#alias {{in call to 'f(0)'}}

void parameter(int p[f(0)]); // #param
// observe-warning@#f {{precondition 'x > 0' evaluated to false}}
// observe-note@#param {{in call to 'f(0)'}}
// terminating-error@#param {{contract violation in a constant expression}}
// terminating-note@#f {{precondition 'x > 0' evaluated to false}}
// terminating-note@#param {{in call to 'f(0)'}}

struct Member {
  int m[f(0)]; // #member
  // observe-warning@#f {{precondition 'x > 0' evaluated to false}}
  // observe-note@#member {{in call to 'f(0)'}}
  // terminating-error@#member {{contract violation in a constant expression}}
  // terminating-note@#f {{precondition 'x > 0' evaluated to false}}
  // terminating-note@#member {{in call to 'f(0)'}}
};

// The bound of a new-expression is not manifestly constant-evaluated: the
// contract assertion is checked at run time.
int *dynamic_array() { return new int[f(0)]; }

// An array bound that is a constant expression is still fine, with any
// semantic, and the array is not a variable length array.
int fine[f(1)];
static_assert(sizeof(fine) == 2 * sizeof(int));
