// [expr.prim.lambda.capture]/3.3 lets a lambda inside a contract assertion have
// a capture-default or simple-capture, but it does not make an enclosing
// parameter usable without one: with no capture-default and no capture naming
// it, the parameter is not odr-usable ([basic.def.odr]), so referring to it is
// ill-formed.
//
// This must be a diagnostic.  Accepting the free-function case silently --
// -fsyntax-only exiting 0 with no diagnostic at all -- leaves Sema with no
// capture recorded for the DeclRefExpr, and CodeGen then hits "UNREACHABLE
// executed at CGExpr.cpp".  The member-function case below is diagnosed
// through a different path and is kept as the control.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

void free_fn(int x) pre([] { return x > 0; }()) {}
// expected-error@-1 {{variable 'x' cannot be implicitly captured in a lambda with no capture-default specified}}
// expected-note@-2 {{'x' declared here}}
// expected-note@-3 {{lambda expression begins here}}
// expected-note@-4 {{capture 'x' by value}}
// expected-note@-5 {{capture 'x' by reference}}
// expected-note@-6 {{default capture by value}}
// expected-note@-7 {{default capture by reference}}

struct S {
  void mem(int x) pre([] { return x > 0; }()) {}
  // expected-error@-1 {{variable 'x' cannot be implicitly captured in a lambda with no capture-default specified}}
  // expected-note@-2 {{'x' declared here}}
  // expected-note@-3 {{lambda expression begins here}}
  // expected-note@-4 {{capture 'x' by value}}
  // expected-note@-5 {{capture 'x' by reference}}
  // expected-note@-6 {{default capture by value}}
  // expected-note@-7 {{default capture by reference}}
};

void lambda_own_pre() {
  auto l = [](int x) pre([] { return x > 0; }()) {};
  // expected-error@-1 {{variable 'x' cannot be implicitly captured in a lambda with no capture-default specified}}
  // expected-note@-2 {{'x' declared here}}
  // expected-note@-3 {{lambda expression begins here}}
  // expected-note@-4 {{capture 'x' by value}}
  // expected-note@-5 {{capture 'x' by reference}}
  // expected-note@-6 {{default capture by value}}
  // expected-note@-7 {{default capture by reference}}
  (void)l;
}
