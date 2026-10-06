// [over.call.func]/p3.1 and [expr.prim.id.general]: an unqualified reference to a
// non-static member (which forms an implied 'this' access) in a constructor
// precondition or a destructor postcondition is ill-formed, because no object is
// within its lifetime at that point. Qualified access (this->m), constructor
// postconditions, and ordinary member functions are unaffected.
// (GCC mirror: g++.dg/contracts/cpp26/over.call.func.p3.1.C.)
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// XFAIL: *
//
// OPEN BUG (sema): an unqualified reference to a non-static member in the
// precondition of a constructor or the postcondition of a destructor is
// accepted.
//   Expected (P2900R14 [expr.prim.id.general]/2: "If this transformation
//     occurs in the predicate of a precondition assertion of a constructor of
//     X or a postcondition assertion of a destructor of X, the expression is
//     ill-formed", and [over.call.func]/3 for the member-call form; the
//     standard's own example is `C() pre(b) // error`): errors for
//     `S() pre(k)` and `~S() post(f())`.
//   Actual: `S() pre(k)` is accepted; `~S() post(f())` is rejected only
//     because f() is not const (constified `this`), not for this reason.

struct S {
  bool f();
  bool k;

  S() pre(k); // expected-error {{cannot access non-static member 'k' in the precondition of a constructor because no object is within its lifetime there}}
  S(int) pre(this->k);     // OK: qualified access is not an unqualified member reference
  S(long) post(k);         // OK: constructor postcondition -- object is alive
  ~S() post(f());          // expected-error {{cannot access non-static member 'f' in the postcondition of a destructor because no object is within its lifetime there}}
  void g() pre(k) post(k); // OK: ordinary member function
};

// A destructor precondition and a constructor postcondition are fine (object
// within its lifetime); only ctor-pre / dtor-post are restricted.
struct T {
  bool f() const; // const so the constified 'this' in a contract can call it
  bool k;
  ~T() pre(f());   // OK: destructor precondition
  T() post(k);     // OK: constructor postcondition
};
