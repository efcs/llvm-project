// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts
// XFAIL: *
//
// OPEN BUG (sema-constification-captures): inside a lambda in a contract
// predicate that captures `*this` by copy, a non-static data member reached
// through the captured copy is wrongly constified.
//   Expected (P2900R14 [expr.prim.id.unqual] example: `++this->z; // OK,
//     captured *this` and `++z; // OK, captured *this`): no diagnostic for
//     the two increments below; the lambda is mutable and the member belongs
//     to the closure's own copy of the object, which is not "a variable
//     declared outside of C".
//   Actual: "cannot assign to non-static data member because it is
//     considered 'const' inside of a contract" on both lines.
// The rest of the standard's example is in
// Sema/contract-predicate-constify-in-lambda.cpp.

struct Y {
  int z = 0;

  void f(int i)
      pre([=, *this]() mutable {
        ++this->z; // OK: the captured *this
        ++z;       // OK: the captured *this
        (void)i;
        return true;
      }()) {}
};

// CONTROL: the same member through the real `this` IS constified.
struct Z {
  int z = 0;
  // expected-error@+1 {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
  void f() pre(++z);
};
