// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A parameter named inside an UNEVALUATED operand of a postcondition
// predicate is not odr-used, so [dcl.contract.func] does not require it to be
// const.
//
// [dcl.contract.func]: "If the predicate of a postcondition assertion of a
// function f odr-uses a non-reference parameter of f, that parameter and the
// corresponding parameter on all declarations of f shall have const type."
// [basic.def.odr]/5 odr-uses a variable named by a POTENTIALLY-EVALUATED
// expression.  The operand of decltype, of sizeof, of noexcept, and the
// requirements of a requires-expression are unevaluated, so naming a
// parameter in one is not an odr-use.
//
// Mirror of gcc/testsuite/g++.dg/contracts/cpp26/postcondition-unevaluated-operand.C,
// where the same family was wrongly rejected.  The requires-expression cases
// are the delicate ones here: nothing pushes a FunctionScopeInfo for a free
// function's eagerly-parsed contracts, and only an unevaluated operand gets
// far enough to walk the scope stack, so an unguarded walk crashes in
// ScopeWalker::nextFuncScope.

template <typename T> bool g();

// ---------------------------------------------------------------- decltype

void dt(int p) post(g<decltype(p)>()) {}

// Parenthesized: still unevaluated, and decltype((p)) is a different type from
// decltype(p) -- which is why the operand's spelling must not decide whether
// the const rule fires.
void dt_paren(int p) post(g<decltype((p))>()) {}

template <typename T> void dt_dep(T p) post(g<decltype(p)>()) {}
template void dt_dep<int>(int);

template <typename T> void dt_dep_paren(T p) post(g<decltype((p))>()) {}
template void dt_dep_paren<int>(int);

// ------------------------------------------------------- requires-expression

void req(int p) post(requires { +p; }) {}

template <typename T> void req_dep(T p) post(requires { +p; }) {}
template void req_dep<int>(int);

// A requires-expression in a precondition took the same crashing path.
void req_pre(int p) pre(requires { +p; }) {}

// A member function's contracts are late-parsed, which pushes a function
// scope, so this shape reached the walk with a stack to walk and did not
// crash.  Kept so a fix that only repairs the free-function path is still
// exercised on both.
struct S {
  void req_member(int p) post(requires { +p; }) {}
};

// ------------------------------------- already-guarded operands, as controls

void sz(int p) post(sizeof(p) > 0) {}

template <typename T> void sz_dep(T p) post(sizeof(p) > 0) {}
template void sz_dep<int>(int);

void nx(int p) post(noexcept(+p) || true) {}

template <typename T> void nx_dep(T p) post(noexcept(+p) || true) {}
template void nx_dep<int>(int);

// ------------------------------------------------------------- redeclaration

// PR127196 comment 1.  The declarations differ in top-level const, which is
// ordinary C++ where the parameter is not odr-used, and only one of them
// carries the postcondition -- so there is no second predicate to mismatch
// against either.  Well-formed.
template <typename T> void redecl(T p) post(g<decltype(p)>());
template <typename T> void redecl(T const p) {}
template void redecl<int>(int);

// -------------------------------------------------- controls that must fail

// A real odr-use is still caught, dependent or not: loosening what counts as
// an odr-use must not lose the rule it exists to enforce.
void odr_use(int p) // expected-note {{parameter of type 'int' is declared here}}
    post(p > 0) {}  // expected-error {{parameter 'p' referenced in contract postcondition must be declared const}}

template <typename T>
void odr_use_dep(T p) // expected-note {{parameter of type 'int' is declared here}}
    post(p > 0) {}    // expected-error {{parameter 'p' referenced in contract postcondition must be declared const}}
template void odr_use_dep<int>(int);
// expected-note@-1 {{in instantiation of function template specialization 'odr_use_dep<int>' requested here}}

// An odr-use sitting alongside an unevaluated one is still an odr-use.
void mixed(int p) // expected-note {{parameter of type 'int' is declared here}}
    post(g<decltype(p)>() && p > 0) {} // expected-error {{parameter 'p' referenced in contract postcondition must be declared const}}

// Const parameters are accepted in every one of these positions.
void ok_const(const int p) post(g<decltype(p)>() && requires { +p; } && p > 0) {}
