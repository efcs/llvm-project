// RUN: %clang_cc1 -std=c++23 -fsyntax-only -verify %s
// XFAIL: *
//
// OPEN BUG area: sema (core language, no contracts).

// OPEN BUG: `this` is accepted in the
// NOEXCEPT-SPECIFIER of an explicit object member function.
//
// [expr.prim.this]/3, as amended by P0847R7: `this` "shall not appear within
// the declaration of either a static member function or an explicit object
// member function of the current class".
//
// THIS ROW IS CLANG-ONLY. Measured 2026-09-05: GCC rejects it correctly and
// only accepts the trailing-return-type row, so the gnu_gcc mirror expects a
// real diagnostic here rather than xfailing it. Same test content, opposite
// expectation -- which is why the mirror is worth having.
//
// Mirror: gcc/testsuite/g++.dg/contracts/cpp26/open-bug-this-in-xobj-declaration.C

struct S {
  int x;
  static int s;

  // THE OPEN BUG (Clang only).
  // expected-error@+1 {{'this' cannot be used in a static member function declaration}}
  void f2(this S &self) noexcept(noexcept(this->x));

  // Control: the static form is correctly rejected today.
  // expected-error@+1 {{'this' cannot be used in a static member function declaration}}
  static void g2() noexcept(noexcept(this->x));

  // Control: the implicit object form must stay accepted.
  void h2() noexcept(noexcept(this->x));
};
