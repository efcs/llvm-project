// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A predicate that fails to build leaves the contract sequence invalid, and
// the declaration then goes through contract matching like any other.  That
// used to abort:
//
//   Assertion `!NewDecl->getContracts()->isInvalidDecl()' failed
//       clang/lib/Sema/SemaContract.cpp, CheckEquivalentContractSequence
//
// The function handled an invalid OLD sequence twice over and an invalid NEW
// one not at all.  Nothing exotic is needed to reach it -- no `this`, no
// member function, and no contract on the other declaration: a redeclaration
// whose predicate does not compile is enough.  One error should be reported,
// and then the compiler should still be running.

void f();
void f() pre(nonesuch) { } // expected-error {{use of undeclared identifier 'nonesuch'}}

// The other order: the bad predicate on the first declaration.
void g() pre(alsonone); // expected-error {{use of undeclared identifier 'alsonone'}}
void g() { }

// Both declarations bad, so the matching code sees an invalid sequence on
// each side.
void h() pre(neither);  // expected-error {{use of undeclared identifier 'neither'}}
void h() pre(norother); // expected-error {{use of undeclared identifier 'norother'}}

// A member function, whose contract is late-parsed and therefore reaches the
// same place by a different route.
struct S {
  void m() pre(missing); // expected-error {{use of undeclared identifier 'missing'}}
};
void S::m() { }

// The error must not swallow the rest of the file: this declaration after the
// bad ones is well-formed and must still be diagnosed on its own terms.
void later(int x) pre(x > 0);
void later(int x) pre(x > 0) { (void)x; }
