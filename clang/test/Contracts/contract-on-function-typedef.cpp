// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A contract specifier written inside a typedef declaration must be
// diagnosed.  A function-contract-specifier-seq is part of a function
// declarator ([dcl.contract.func]); a typedef declaration declares a
// typedef-name, not a function, so there is nothing for the contract to
// belong to and no call is ever checked against it.
//
// It used to be accepted and silently dropped -- the worst shape a contract
// facility can fail in, since the user writes a precondition, is told
// nothing, and gets no check.  The guard asked `isFunctionDeclarator()`,
// which is purely about declarator CHUNKS and is true here: `typedef int
// F(int)` really does have a function chunk.  What disqualifies it is the
// storage class, which lives on the leading DeclSpec.
//
// GCC has the same gap and there it is upstream's; the mirror is
// gcc/testsuite/g++.dg/contracts/cpp26/contract-on-non-function-declarator.C.

typedef int FTypedef(int) pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}

// The post spelling, so a fix that only guards `pre` is still caught.
typedef int FTypedefPost(int) post(r : r > 0); // expected-error {{function contract specifiers are only allowed on function declarations}}

// A member typedef reaches the same place through the class-member path,
// where the contract's tokens are also cached for late parsing.
struct S {
  typedef int FMember(int) pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
};

// In a template, so the non-dependent path is not the only one covered.
template <typename T>
struct U {
  typedef T FDependent(T) pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
};

template struct U<int>;

// The alias-declaration spelling of the same mistake is rejected too, but by
// the alias grammar rather than by the contract rule -- `pre` is simply the
// wrong token after the type.  Kept here so that the two spellings stay
// visibly different, and so a later change that routes alias-declarations
// through the contract guard shows up as a change to this line.
using FAlias = int(int) pre(true); // expected-error {{expected ';' after alias declaration}}

// Controls.  A typedef with no contract, and a real function declaration that
// uses the typedef'd type, must both still work.
typedef int FPlain(int);
int ok(int x) pre(x > 0);

struct T {
  typedef int FMemberPlain(int);
  int ok_member(int x) pre(x > 0);
};
