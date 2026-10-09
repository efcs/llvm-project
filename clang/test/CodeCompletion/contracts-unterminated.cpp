// Code completion within a function contract specifier which is not
// terminated before the end of the file: the function is still created and
// its contracts parsed, so that the completion happens in the context of the
// function and its parameters are offered.
bool check(int);
int gl;
int f(const int xylo, const int yoyo) pre(
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -code-completion-at=%s:%(line-1):43 %s -o - | FileCheck %s
// CHECK: COMPLETION: check : [#bool#]check(<#int#>)
// CHECK: COMPLETION: gl : [#int#]gl
// CHECK: COMPLETION: xylo : [#const int#]xylo
// CHECK: COMPLETION: yoyo : [#const int#]yoyo
