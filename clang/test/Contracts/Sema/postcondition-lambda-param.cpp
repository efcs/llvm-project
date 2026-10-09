// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts

// [dcl.contract.func]/7 constrains "a non-reference parameter of f".  A lambda
// appearing in the predicate has parameters of its own, and naming one is not
// a use of a parameter of f at all.
//
// Clang has always been right here, and the reason is worth pinning rather
// than assuming: classifyDiagnosableParmVar checks that the parameter belongs
// to FD, via getFunctionScopeIndex against FD's own parameter list.  GCC's
// check ran from finish_id_expression and had no such notion, so it rejected
// these; it was fixed on the GCC side, where moving the check onto a
// walk of the finished predicate forced the question "whose parameter is
// this?" to be answered explicitly.
//
// GCC mirror: g++.dg/contracts/cpp26/pr126897.C

// expected-no-diagnostics

int lambda_param(int x) post([](int y) { return y > 0; }(1)) { return x; }

int lambda_param_const(const int x) post([](int y) { return y > 0; }(1) && x >= 0) {
  return x;
}

// A generic lambda in the predicate, whose parameter is dependent until the
// call instantiates it.
int generic_lambda(int x) post([](auto y) { return y > 0; }(1)) { return x; }

// A lambda taking a non-const parameter BY VALUE, which is exactly the shape
// the rule would reject if it applied to the wrong function.
int by_value(const int x) post([](int y) { return y >= 0; }(x)) { return x; }

// Nested lambdas, so the parameter belongs to neither f nor the outer lambda.
int nested(int x) post([](int y) { return [](int z) { return z > 0; }(y); }(1)) {
  return x;
}
