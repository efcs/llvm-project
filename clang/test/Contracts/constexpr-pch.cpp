// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -emit-pch -o %t.pch %s -verify=pch
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -include-pch %t.pch -fsyntax-only %s -verify=use
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -emit-pch -o %t.pch %s -verify=pch -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -include-pch %t.pch -fsyntax-only %s -verify=use -fexperimental-new-constant-interpreter

// A contract violation in the initializer of a variable is diagnosed where
// the variable is defined. Evaluating the initializer again to get the value
// of the variable, for instance after loading it from an AST file, doesn't
// diagnose the violation again.

#ifndef HEADER
#define HEADER

constexpr int f(int x) pre(x > 0) { return x + 1; } // #f
constexpr int constexpr_var = f(0);
// pch-warning@#f {{contract violation with the 'observe' evaluation semantic: precondition 'x > 0' evaluated to false}}
// pch-note@-2 {{in call to 'f(0)'}}
const int const_var = f(0);
// pch-warning@#f {{contract violation with the 'observe' evaluation semantic: precondition 'x > 0' evaluated to false}}
// pch-note@-2 {{in call to 'f(0)'}}
inline int inline_var = f(0);
// pch-warning@#f {{contract violation with the 'observe' evaluation semantic: precondition 'x > 0' evaluated to false}}
// pch-note@-2 {{in call to 'f(0)'}}
template <int N> struct X { static constexpr int value = N; };

#else

// use-no-diagnostics
static_assert(constexpr_var == 1);
static_assert(const_var == 1);
int array[constexpr_var + const_var];
X<constexpr_var> x1;
X<const_var> x2;
int use = inline_var;

#endif
