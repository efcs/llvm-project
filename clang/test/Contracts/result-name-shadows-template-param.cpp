// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [temp.local]p6: the name of a template-parameter shall not be bound to any
// following declaration whose locus is contained by the scope to which the
// template-parameter belongs. A result name that shadows a template parameter
// is diagnosed, matching what Clang already does for an ordinary parameter
// declaration (Sema::ActOnParamDeclarator); it used to be accepted silently
// (and crashed Clang at some point).
//
// Based on tests from Joshua Berne's contracts-p3850 branch.

template <typename r>
int shadows_type_param(int x) post(r: r > 0) { return x; }
// expected-error@-1 {{declaration of 'r' shadows template parameter}}
// expected-note@-3 {{template parameter is declared here}}

template <int r>
int shadows_non_type_param(int x) post(r: r > 0) { return x; }
// expected-error@-1 {{declaration of 'r' shadows template parameter}}
// expected-note@-3 {{template parameter is declared here}}

template <template <class> class r>
int shadows_template_template_param(int x) post(r: r > 0) { return x; }
// expected-error@-1 {{declaration of 'r' shadows template parameter}}
// expected-note@-3 {{template parameter is declared here}}

template <class T, int r>
struct S {
  int member(int x) post(r: r > 0) { return x; }
  // expected-error@-1 {{declaration of 'r' shadows template parameter}}
  // expected-note@-4 {{template parameter is declared here}}
};

// Shadowing a function parameter is diagnosed too (pre-existing behaviour).
int shadows_param(int r) post(r: r > 0) { return r; }
// expected-error@-1 {{declaration of result name 'r' shadows parameter}}
// expected-note@-2 {{previous declaration is here}}

// A result name that shadows nothing is fine.
template <typename T>
T fine(T x) post(r: r > T{}) { return x; }
