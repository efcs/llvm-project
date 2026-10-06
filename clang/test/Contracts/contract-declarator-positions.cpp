// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A contract specifier is part of a function DECLARATOR, so writing one on a
// declarator that merely has a function TYPE must be rejected.  The two are
// easy to conflate: `FAlias g_alias` declares something whose type is
// `int(int)`, and a parser that asks "is this type a function type?" rather
// than "did this declarator write a parameter list?" accepts it.
//
// contract-on-non-function.cpp covers the declarators with no function type
// anywhere -- data members, bit-fields, pointers to function.  These are the
// harder half: the function type is real, it just was not written here.
//
// The typedef-DEFINITION shape -- a contract inside the typedef declaration
// itself, `typedef int F(int) pre(true);` -- is a separate case and is
// currently accepted.  It is pinned in OpenBugs/contract-on-function-typedef.cpp.

using FAlias = int(int);
FAlias g_alias pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}

typedef int FTypedef(int);
FTypedef g_typedef pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}

// The same through a post specifier, so nothing depends on which keyword.
FAlias g_alias_post post(r : true); // expected-error {{function contract specifiers are only allowed on function declarations}}

// A parameter whose type is a function type.  It decays to a pointer to
// function, so there is no function declarator to carry a contract, and the
// contract must not be silently dropped.
void takes_fn(int bar() pre(true)); // expected-error {{expected ')'}} expected-note {{to match this '('}}

// A member declared through a function type alias, which reaches the
// late-parsing path rather than the eager one.
struct S {
  FAlias m_alias pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
};

// The well-formed counterpart of each, as controls: these declare functions
// by writing a parameter list, so the contract has a declarator to attach to.
int ok_free(int x) pre(x > 0);

struct T {
  int ok_member(int x) pre(x > 0);
};
