// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s
// XFAIL: *
//
// OPEN BUG area: constant evaluation (core language, no contracts).

// OPEN BUG.  GCC has it too.
//
// Forming the address of a subobject before its construction has begun is not
// a core constant expression: [class.cdtor]/1 makes referring to a member or
// base of an object with a non-trivial constructor, before that constructor
// begins, undefined.
//
// Clang used to get THIS shape right by accident: the implicit constructor of
// a class with a virtual base was not constexpr, so the notes below were
// produced. Since virtual base classes became usable in constant expressions
// (C++26), that constructor is constexpr and Clang accepts the program
// without diagnosing the out-of-lifetime access, like GCC.
// Its mirror in the gnu_gcc fork --
// gcc/testsuite/g++.dg/contracts/cpp26/open-bug-address-before-ctor.C -- is
// xfailed for the same shape, because GCC accepts it.  Same content,
// per-compiler expectation.
//
// See also address-before-ctor-member.cpp in this directory, the shape both
// compilers get wrong.
//
// No contracts involved; pure core-language constexpr.

namespace vbase {
struct W { int j; };
struct X : virtual W {}; // expected-note {{declared here}}

// Declaration ORDER is load-bearing: `p` must come first, so that `x` is
// genuinely unbuilt when `p` is initialised. With `x` first the program is
// legal and the test pins nothing.
struct Y {
  int *p;
  X x;
  constexpr Y() : p(&x.j) {} // expected-note {{non-constexpr constructor 'X' cannot be used in a constant expression}}
};

// expected-note@+1 {{in call to 'Y()'}}
constexpr int f() { Y y; return y.p != nullptr; }

// expected-error@+2 {{static assertion expression is not an integral constant expression}}
// expected-note@+1 {{in call to 'f()'}}
static_assert((f(), true));
} // namespace vbase
