// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -fcontract-evaluation-semantic=ignore %s

// A naked function has no prologue or epilogue in which to evaluate its
// contract assertions, so it cannot have any (CodeGen used to silently skip
// them).

__attribute__((naked)) void f(int x) pre(x > 0) { __asm__(""); } // expected-error {{naked function 'f' cannot have function contract specifiers}} \
                                                                 // expected-note {{attribute is here}}
[[gnu::naked]] void g(int x) pre(x > 0); // expected-error {{naked function 'g' cannot have function contract specifiers}} \
                                         // expected-note {{attribute is here}}
void g(int x) { __asm__(""); }

// The attribute may be added by a later declaration.
void h(int x) pre(x > 0); // expected-error {{naked function 'h' cannot have function contract specifiers}}
__attribute__((naked)) void h(int x) { __asm__(""); } // expected-note {{attribute is here}}

int i(int x) post(r: true); // expected-error {{naked function 'i' cannot have function contract specifiers}}
__attribute__((naked)) int i(int x); // expected-note {{attribute is here}}
int i(int x) { __asm__(""); }

// Diagnosed once, on the declaration which introduces the attribute.
__attribute__((naked)) void j(int x) pre(x > 0); // expected-error {{naked function 'j' cannot have function contract specifiers}} \
                                                 // expected-note {{attribute is here}}
void j(int x) pre(x > 0);
void j(int x) { __asm__(""); }

struct S {
  __attribute__((naked)) void m(int x) pre(x > 0); // expected-error {{naked function 'm' cannot have function contract specifiers}} \
                                                   // expected-note {{attribute is here}}
};

// Without contracts there is nothing to diagnose.
__attribute__((naked)) void ok(int x) { __asm__(""); }
void ok2(int x) pre(x > 0) { }
