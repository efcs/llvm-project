// RUN: %clang_cc1 -std=c++23 -fsyntax-only -verify %s
// XFAIL: *
//
// OPEN BUG area: sema (core language, no contracts).

// OPEN BUG: `this` is
// accepted in the TRAILING RETURN TYPE of an explicit object member
// function.
//
// [expr.prim.this]/3, as amended by P0847R7: `this` "shall not appear within
// the declaration of either a static member function or an explicit object
// member function of the current class". One sentence, two kinds of function;
// the static half IS diagnosed, which is what shows only half of it is being
// applied.
//
// Split from the noexcept-specifier row into its own file because lit's XFAIL
// is per FILE: with both rows here, fixing one would leave the file still
// failing and we would get no signal. The gnu_gcc mirror keeps them together
// because DejaGnu's xfail is per line. Both compilers accept this row.
//
// Mirror: gcc/testsuite/g++.dg/contracts/cpp26/open-bug-this-in-xobj-declaration.C

struct S {
  int x;
  static int s;

  // THE OPEN BUG.
  // expected-error@+1 {{'this' cannot be used in a static member function declaration}}
  auto f1(this S &self) -> decltype(this->x);

  // Control: the static form is correctly rejected today.
  // expected-error@+1 {{'this' cannot be used in a static member function declaration}}
  static auto g1() -> decltype(this->x);

  // Control: the implicit object form must stay accepted.
  auto h1() -> decltype(this->x);
};
