// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A contract on a declarator that is not the FIRST of its declarator-group.
//
// `int a, f() pre(true);` declares an object and a function, and a contract on
// the function is perfectly well-formed -- but only the first declarator of a
// group was ever offered a contract specifier.  A later one reached nothing
// that knew what `pre` was and died on a bare "expected ';'", which tells the
// user nothing about contracts at all.  ParseDeclGroup already handled a
// trailing requires-clause in both places; the contract was handled in only
// one.
//
// So the fix here shows up as code being ACCEPTED, not as a new diagnostic.

int a, f() pre(true);

// `y` is const because [dcl.contract.func] forbids a postcondition from
// odr-using a non-const value parameter -- nothing to do with declarator
// groups, but the rule applies here like anywhere else.
int b, g(int x) pre(x > 0), h(const int y) post(r : r > y);

// The first declarator may carry one too, and the two must not interfere.
int p(int x) pre(x > 0), q, r(int y) pre(y > 0);

// A later declarator that is NOT a function still gets the contract rule,
// which is the diagnostic this path previously could not produce.
int s, t pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}

// In a class, where a member-declarator-list takes a different path already
// covered by ParseCXXMemberDeclaratorBeforeInitializer -- here as a control
// that the two agree.
struct S {
  int m, mf(int x) pre(x > 0);
};
