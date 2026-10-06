// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts

// `this` is unavailable inside an explicit object member function
// ([expr.prim.this]/1), and a contract predicate on such a function is inside
// it.  Naming `this` -- explicitly, or implicitly by naming a non-static data
// member unqualified, which means (*this).m -- must be rejected.
//
// Accepting any of it reaches CodeGen and asserts:
//
//   CodeGenFunction.h: LoadCXXThis():
//   Assertion `CXXThisValue && "no 'this' value for this function"' failed.
//
// Sema::CheckCXXThisType handles the explicit-object case correctly, but it
// only fires when the `this` type is null, so the contract parser must not
// push a CXXThisScopeRAII for an explicit object member function: pushing one
// for any member declarator -- as the shared helper does, its notion of
// "member function" predating P0847 -- makes that branch unreachable.
//
// The control at the bottom is what makes this contracts-specific rather than
// a general deducing-this hole: the identical use of `this` in the function
// BODY is rejected correctly.  Only the predicate needs the guard.
//
// GCC mirror: g++.dg/contracts/cpp26/deducing-this-no-this-in-predicate.C

struct S {
  int x = 0;
};

// Explicit `this` in a precondition.
struct ExplicitThis : S {
  // expected-error@+1 {{invalid use of 'this' in a function with an explicit object parameter}}
  void f(this ExplicitThis &self) pre(this->x == 0);
};

// Explicit `this` in a POSTCONDITION, and in one with a result-name.  A
// postcondition is parsed on a different path -- the result name is a
// declaration, and the predicate is dependent while it is being parsed -- so
// the precondition case above is not enough on its own.  Added by the audit of
// 2026-09-05, which found the explicit spelling covered only for `pre`.
struct ExplicitThisPost : S {
  // expected-error@+1 {{invalid use of 'this' in a function with an explicit object parameter}}
  void f(this ExplicitThisPost &self) post(this->x == 0);
};

struct ExplicitThisPostResult : S {
  // expected-error@+1 {{invalid use of 'this' in a function with an explicit object parameter}}
  int f(this ExplicitThisPostResult &self) post(r : this->x == r);
};

// And in an assertion-statement, for the same reason the implicit spelling is
// covered there.
struct ExplicitThisAssert : S {
  void f(this ExplicitThisAssert &self) {
    // expected-error@+1 {{invalid use of 'this' in a function with an explicit object parameter}}
    contract_assert(this->x == 0);
  }
};

// An unqualified non-static data member in a precondition.
struct ImplicitThisPre : S {
  // expected-error@+1 {{invalid use of member 'x' in explicit object member function}}
  void f(this ImplicitThisPre &self) pre(x == 0);
};

// The same in a postcondition.
struct ImplicitThisPost : S {
  // expected-error@+1 {{invalid use of member 'x' in explicit object member function}}
  int f(this ImplicitThisPost &self) post(r : x == r);
};

// The same in an assertion-statement in the body.
struct ImplicitThisAssert : S {
  void f(this ImplicitThisAssert &self) {
    // expected-error@+1 {{invalid use of member 'x' in explicit object member function}}
    contract_assert(x == 0);
  }
};

// CONTROL: the explicit object parameter itself is of course usable.
struct ViaSelf : S {
  void f(this ViaSelf &self) pre(self.x == 0);
};

// CONTROL: an ordinary (implicit object) member function may name `this` and
// its members in a predicate.
struct ImplicitObject : S {
  void g() const pre(this->x == 0) pre(x == 0);
};

// CONTROL: the same use of `this` in the BODY is already rejected.  This one
// passes today; it is here so that a future fix cannot be mistaken for having
// merely disabled the body check.
struct ThisInBody : S {
  void f(this ThisInBody &self) {
    (void)self;
    // expected-error@+1 {{invalid use of 'this' in a function with an explicit object parameter}}
    (void)this->x;
  }
};
