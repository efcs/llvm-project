// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// XFAIL: *
//
// OPEN BUG (parser/sema, crash): a result name that has the same name as a
// template parameter of the enclosing template crashes the compiler:
//   Casting.h: dyn_cast<ParmVarDecl>(NamedDecl*): Assertion
//   `detail::isPresent(Val)` failed in Sema::ActOnResultNameDeclarator
// (the shadow check assumes whatever lookup finds is a ParmVarDecl).
//   Expected ([temp.local]/6: a template-parameter shall not be redeclared
//     within its scope, including nested scopes): an error on
//     shadows_type_param and shadows_non_type_param.
//   Actual: assertion failure.

// A result name that shadows a template parameter is diagnosed, matching what
// Clang already does for an ordinary parameter declaration
// (Sema::ActOnParamDeclarator) and for a result name that shadows a function
// parameter.

template <typename r>
int shadows_type_param(int x) post(r: r > 0) { return x; }
// expected-error@-1 {{declaration of 'r' shadows template parameter}}
// expected-note@-3 {{template parameter is declared here}}

template <int r>
int shadows_non_type_param(int x) post(r: r > 0) { return x; }
// expected-error@-1 {{declaration of 'r' shadows template parameter}}
// expected-note@-3 {{template parameter is declared here}}

// Shadowing a function parameter is diagnosed too (pre-existing behaviour).
int shadows_param(int r) post(r: r > 0) { return r; }
// expected-error@-1 {{declaration of result name 'r' shadows parameter}}
// expected-note@-2 {{previous declaration is here}}

// A result name that shadows nothing is fine.
template <typename T>
T fine(T x) post(r: r > T{}) { return x; }
