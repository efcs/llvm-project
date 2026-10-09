// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -ast-dump %s | FileCheck %s
// expected-no-diagnostics

// A contract specifier is part of the declarator, so on an OUT-OF-LINE member
// definition it sits inside the class's scope: its predicate may name members
// and `this`, exactly as the in-class spelling may.
//
// This did not work.  The contract was parsed with CurContext still at the
// enclosing namespace, so `this` had no type ("invalid use of 'this' outside
// of a non-static member function") and an unqualified member name was an
// undeclared identifier -- and the resulting invalid contract then tripped an
// assertion in CheckEquivalentContractSequence.  A requires-clause in the same
// position had always worked, because ParseDeclGroup called
// ParseTrailingRequiresClauseWithScope for it and a bare
// ParseContractSpecifierSequence for the contract.
//
// The qualifiers on `this` come from the FUNCTION CHUNK, not from the
// declarator's leading DeclSpec -- the `const` of `int S::f(int) const` is a
// trailing qualifier.  Predicate const-ification hides a mistake there, since
// it makes `this` const whatever the chunk says, so `volatile` is what
// actually pins it: the Volatile rows below are the ones that fail if the
// wrong DeclSpec is consulted.

struct S {
  int limit;

  int inclass(int i) const pre(this->limit >= 0) { return i; }
  int outline(int i) const pre(this->limit >= 0);

  // Unqualified, so the predicate needs the class scope and not merely `this`.
  int unqualified(int i) const pre(limit >= 0);

  int vol(int i) volatile pre(this->limit >= 0);
  int cv(int i) const volatile pre(this->limit >= 0);

  ~S() pre(this->limit >= 0);
  int result() const post(r : r >= 0);
};

int S::outline(int i) const pre(this->limit >= 0) { return i; }
int S::unqualified(int i) const pre(limit >= 0) { return i; }
int S::vol(int i) volatile pre(this->limit >= 0) { return i; }
int S::cv(int i) const volatile pre(this->limit >= 0) { return i; }
S::~S() pre(this->limit >= 0) {}
int S::result() const post(r : r >= 0) { return limit; }

// Out of line through a nested-name-specifier that is itself qualified.
namespace API {
struct Service {
  int cap;
  int call(const int n) const pre(this->cap >= 0 && n > 0) post(r : r > 0);
};
} // namespace API

int API::Service::call(const int n) const
    pre(this->cap >= 0 && n > 0) post(r : r > 0) {
  return n;
}

// CHECK-NOT: CXXThisExpr {{.*}} 'S *'
// CHECK-NOT: CXXThisExpr {{.*}} 'volatile S *'
//
// Both volatile members, in-class and out-of-line, must agree that `this` is
// const volatile.  A count is deliberate: two declarations of `vol` and two of
// `cv`, so a fix that repairs only the in-class half leaves this short.
// CHECK-COUNT-4: CXXThisExpr {{.*}} 'const volatile S *'
