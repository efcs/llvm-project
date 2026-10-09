// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify=expected,warn -Wunused -Wunused-parameter -Wshadow %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify=expected -Wno-unused -Wno-unused-parameter -Wno-shadow %s

// A result binding is declared by an attributed-identifier; [[maybe_unused]]
// may appertain to it ([dcl.contract.res]).

int used(const int x) post(r : r > x) { return x; }
int unused1(const int x) post(r : x > 0) { return x; } // warn-warning {{unused result name 'r'}}
int unused2(const int x) post(r [[maybe_unused]] : x > 0) { return x; }
int unused3(const int x) post(r [[maybe_unused]] [[]] : x > 0) { return x; }
int unused4(const int x) post(r : x > 0) post(s : s > 0) { return x; } // warn-warning {{unused result name 'r'}}
int unused5(const int x) post(r [[maybe_unused]] : x > 0) post(s : s > 0);

// The warning is issued once, when the postcondition is parsed, and not again
// for a definition which omits the contracts or for a template instantiation.
int unused5(const int x) { return x; }
template <class T> T tunused(const T x) post(r : x > T{}) { return x; } // warn-warning {{unused result name 'r'}}
int tuse = tunused(1);

// Only [[maybe_unused]] appertains to a result binding.
int attr1(const int x) post(r [[deprecated]] : r > x); // expected-error {{'deprecated' attribute cannot be applied to a result name}}
int attr2(const int x) post(r [[clang::contract_group("a")]] : r > x); // expected-error {{'clang::contract_group' attribute cannot be applied to a result name}}
int attr3(const int x) post(r [[maybe_unused("x")]] : r > x); // expected-error {{attribute 'maybe_unused' cannot have an argument list}}
int attr4(const int x) post(r [[foo::bar]] : r > x); // expected-warning {{unknown attribute 'foo::bar' ignored}}

// A parameter which is only used in a contract is used.
int param_in_contract(int x, int y) pre(x > 0) { return 0; } // warn-warning {{unused parameter 'y'}}
int param_in_first_decl(int x) pre(x > 0);
int param_in_first_decl(int x) { return 0; }
int param_in_lambda(int x) pre([x] { return x > 0; }()) { return 0; }

// -Wshadow: a result binding hides variables and fields like a local
// variable does.
int global; // warn-note {{previous declaration is here}}
int shadow_global() post(global : global > 0); // warn-warning {{declaration shadows a variable in the global namespace}}
struct S {
  int m; // warn-note {{previous declaration is here}}
  int shadow_field() post(m : m > 0); // warn-warning {{declaration shadows a field of 'S'}}
  static int no_shadow_in_static() post(m : m > 0);
};
int S::shadow_field() { return m; }
// A result binding is in scope in its predicate only, so it is neither
// shadowed by a local variable nor does it shadow a result binding of another
// postcondition.
int local(int x) post(r : r > 0) post(r : r > 1) { int r = x; return r; }
// Shadowing a parameter is an error.
int param(int x) post(x : x > 0); // expected-error {{declaration of result name 'x' shadows parameter}} \
                                  // expected-note {{previous declaration is here}}
