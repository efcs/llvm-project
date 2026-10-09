// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A contract specifier written on something that is not a function
// declaration must be diagnosed rather than crash the compiler.  If
// ParseContractSpecifierSequence goes straight for the declarator's function
// chunk to bring the parameters into scope, it trips getFunctionTypeInfo()'s
// "Not a function declarator!" assertion when there is none -- a data member
// with a stray `pre`, one line and no other error, is enough.
//
// The sibling failure is a contract that WAS cached for late parsing and then
// has no declaration to be replayed against, because the declaration itself
// turned out to be ill-formed.  Those tokens reach Declarator::clear() and
// trip its "Late-parsed contracts unhandled" assertion.  Both are errors.

// ---------------------------------------------------------------------------
// Not a function declaration: diagnosed where it is written.
// ---------------------------------------------------------------------------

int gv pre(1); // expected-error {{function contract specifiers are only allowed on function declarations}}

int gv_post post(r : 1); // expected-error {{function contract specifiers are only allowed on function declarations}}

// A function chunk is present, but the declarator declares a pointer, not a
// function.
int (*gfp)() pre(1); // expected-error {{function contract specifiers are only allowed on function declarations}}

struct S {
  int m pre(1); // expected-error {{function contract specifiers are only allowed on function declarations}}
  static int sm pre(1); // expected-error {{function contract specifiers are only allowed on function declarations}}
  int bf : 3 pre(1); // expected-error {{function contract specifiers are only allowed on function declarations}}
  int (*mfp)() pre(1); // expected-error {{function contract specifiers are only allowed on function declarations}}
};

// The specifier is consumed whole rather than up to the first ')', so a
// predicate with nested parentheses and commas leaves nothing behind to
// cascade into unrelated errors -- the declaration after it still parses.
int nested(int, int);
int gv_nested pre(nested(1, (2 + 3)) > 0); // expected-error {{function contract specifiers are only allowed on function declarations}}
int after_nested;

// Second and later declarators in a member-declarator-list reach the same
// place with a recycled Declarator.
struct U {
  int a, b pre(1); // expected-error {{function contract specifiers are only allowed on function declarations}}
};

// ---------------------------------------------------------------------------
// Cached for late parsing, then no declaration to attach to.  The declaration
// is diagnosed on its own account; the contract is dropped without an
// assertion (Clang emits no second diagnostic for it).
// ---------------------------------------------------------------------------

struct V {
  V V() pre(1); // expected-error {{constructor cannot have a return type}}
};
