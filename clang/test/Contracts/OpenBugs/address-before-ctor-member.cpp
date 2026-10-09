// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s
// XFAIL: *
//
// OPEN BUG area: constant evaluation (core language, no contracts).

// OPEN BUG.  GCC has it too.
//
// Forming the address of a direct member with a non-trivial constructor,
// before that constructor has begun, is accepted in a constant expression.
// [class.cdtor]/1 makes it undefined, so `f()` is not a core constant
// expression and the static_assert must be an error.
//
// BOTH compilers accept this, so both mirrors are xfailed -- unlike the
// virtual-base shape in address-before-ctor-vbase.cpp, which Clang gets
// right.  When this starts being diagnosed the XFAIL becomes an XPASS, which
// lit reports, and that is the signal to close the bug.
//
// No contracts involved; pure core-language constexpr.

namespace direct_member {
struct Z {
  int k;
  constexpr Z() : k(0) {} // user-provided -> non-trivial
};

// Declaration ORDER is load-bearing: `p` first, so `z` is genuinely unbuilt.
struct Y {
  int *p;
  Z z;
  // The -Wuninitialized warning is accounted for here so that the ONLY unmet
  // expectation in this file is the missing error -- otherwise the file would
  // keep failing after the bug is fixed and the XPASS signal would be lost.
  // expected-warning@+1 {{field 'z' is uninitialized when used here}}
  constexpr Y() : p(&z.k) {} // must be diagnosed: z is not yet built
};

constexpr int f() { Y y; return y.p != nullptr; }

// expected-error@+1 {{static assertion expression is not an integral constant expression}}
static_assert((f(), true));
} // namespace direct_member
