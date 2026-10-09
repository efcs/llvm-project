// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -analyzer-checker=core,debug.ExprInspection -verify %s
//
// The contracts of a function are those of its first declaration, whose
// parameters the predicates refer to; they apply to a definition which omits
// them (and renames the parameters) as if they referred to its parameters.

void clang_analyzer_eval(bool);

int pre_omitted(int x) pre(x > 0);
int pre_omitted(int y) {
  clang_analyzer_eval(y > 0); // expected-warning {{TRUE}}
  return y;
}

int post_omitted(const int x) post(r : r > x);
int post_omitted(const int y) { return y + 1; }
void call_post_omitted(int v) {
  int r = post_omitted(v);
  clang_analyzer_eval(r > v); // expected-warning {{TRUE}}
}

// Inlined into a caller, the preconditions constrain the arguments.
int inlined(int x) pre(x != 0);
int inlined(int y) { return 10 / y; } // no division by zero
int call_inlined() { return inlined(1); }

struct S {
  int method(const int x) pre(x > 0) post(r : r > x);
};
int S::method(const int y) { return y + 1; }
void call_method(int v) {
  int r = S().method(v);
  clang_analyzer_eval(r > v); // expected-warning {{TRUE}}
}
