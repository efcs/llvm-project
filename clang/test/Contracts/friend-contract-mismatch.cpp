// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// Two friend declarations of the same function whose contracts DISAGREE are
// a redeclaration with a different function-contract-specifier-seq
// ([dcl.contract.func]/4) and must be diagnosed like any other redeclaration
// path.  Their contracts are late-parsed (deferred to the end of the class),
// which is what makes this a separate path from the namespace-scope control.
//
// (GCC accepts this silently; Clang gets it right.)

struct C {
  // expected-note@+1 {{contract previously specified with a non-equivalent condition}}
  friend int f(int x) pre(x > 0);
  // expected-error@+2 {{function redeclaration differs in contract specifier sequence}}
  // expected-note@+1 {{in contract specified here}}
  friend int f(int x) pre(x < 0);
};

int f(int x) { return x; }

// CONTROL: the identical mismatch at namespace scope IS diagnosed, which is
// what places the gap on the friend path rather than on contract matching.
// expected-note@+1 {{contract previously specified with a non-equivalent condition}}
int g(int x) pre(x > 0);
// expected-error@+2 {{function redeclaration differs in contract specifier sequence}}
// expected-note@+1 {{in contract specified here}}
int g(int x) pre(x < 0);
