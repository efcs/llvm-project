// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A function-contract-specifier-seq may follow a declarator which declares a
// function ([dcl.decl.general]p4+a, [class.mem.general]p1+a): `FAlias g_alias`
// declares a function whose type is `int(int)`, so it may have one, like a
// declarator which writes a parameter list (see
// contract-on-typedef-function-type.cpp).
//
// contract-on-non-function.cpp covers the declarators with no function type
// anywhere -- data members, bit-fields, pointers to function.  These are the
// other half: a function type is involved, but no function is declared.
//
// The typedef-DEFINITION shape -- a contract inside the typedef declaration
// itself, `typedef int F(int) pre(true);` -- is a separate case, see
// contract-on-function-typedef.cpp.

using FAlias = int(int);
FAlias g_alias pre(true); // OK

typedef int FTypedef(int);
FTypedef g_typedef pre(true); // OK

// The same through a post specifier, so nothing depends on which keyword.
FAlias g_alias_post post(r : true); // OK

// A pointer or reference to the function type does not declare a function.
FAlias *g_alias_ptr pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
FAlias (*g_alias_ptr2) pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}

// A parameter whose type is a function type.  It decays to a pointer to
// function, so there is no function declared to carry a contract, and the
// contract must not be silently dropped.
void takes_fn(int bar() pre(true)); // expected-error {{function contract specifiers are not allowed on a parameter declaration}}
void takes_fn_alias(FAlias fn pre(true)); // expected-error {{function contract specifiers are not allowed on a parameter declaration}}

// A member declared through a function type alias, which reaches the
// late-parsing path rather than the eager one.
struct S {
  FAlias m_alias pre(true); // OK
  FAlias *m_alias_ptr pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
};

// The well-formed counterpart of each, as controls: these declare functions
// by writing a parameter list, so the contract has a declarator to attach to.
int ok_free(int x) pre(x > 0);

struct T {
  int ok_member(int x) pre(x > 0);
};
