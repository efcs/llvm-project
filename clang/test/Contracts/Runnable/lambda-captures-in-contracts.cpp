// Check the runtime behaviour of entities captured by lambda-expressions
// within contract predicates: captures by copy are copies which can be
// modified without affecting the entity, captures by reference and '*this'
// refer to the entity, and the closure types are the same whether or not the
// predicate is evaluated.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.O0
// RUN: %t.O0
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.O2
// RUN: %t.O2
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -O0 -DIGNORE %s -o %t.ignore
// RUN: %t.ignore

#include "contracts-runtime.h"
#include "my_assert.h"

#include <cstdio>

int Violations = 0;
void handle_contract_violation(const std::contracts::contract_violation &V) { ++Violations; }

int Evals = 0;
bool count() {
  ++Evals;
  return true;
}

// Modifying a copy within the predicate does not affect the entity.
int copies(int x, int &ref) {
  int local = 10;
  contract_assert([=]() mutable { ++x; ++ref; ++local; return count() && x == 2 && ref == 101 && local == 11; }());
  contract_assert([x, &ref]() mutable { ++x; return count() && x == 2 && ref == 100; }());
  contract_assert([y = x, &z = local]() mutable { ++y; return count() && y == 2 && z == 10; }());
  return x + ref + local;
}

// A copy of '*this' can be modified; a reference to it cannot (and is const).
struct S {
  int m = 1;
  mutable int mm = 0;
  int f() {
    contract_assert([*this]() mutable { ++m; return count() && m == 2; }());
    contract_assert([=, *this]() mutable { m = 5; return count() && m == 5; }());
    contract_assert([this] { ++mm; return count() && m == 1; }());
    contract_assert([&] { ++mm; return count() && m == 1; }());
    contract_assert([*this]() mutable { return [&] { ++m; return count() && m == 2; }(); }());
    return m;
  }
  int g() pre([*this]() mutable { m = 7; return count() && m == 7; }()) post(r: [this, r] { return count() && r == m; }()) { return m; }
};

// Parameters captured by lambdas in function contract assertions.
int params(const int x, int &ref) pre([&] { return count() && x == 1 && ref == 100; }())
    pre([x] { return count() && x == 1; }())
    post(r: [&] { return count() && r == x + ref; }())
    post(r: [=] { return count() && r == x + ref; }()) {
  return x + ref;
}

int params2(int x, int &ref) pre([=]() mutable { ++x; ++ref; return count() && x == 2 && ref == 101; }()) {
  return x + ref;
}

// Structured bindings.
struct P { int a, b; };
int bindings(P p) {
  auto [a, b] = p;
  auto &[ra, rb] = p;
  contract_assert([=]() mutable { ++a; return count() && a == 2 && b == 3; }());
  contract_assert([&] { return count() && a == 1 && &ra == &p.a; }());
  contract_assert([a, &rb]() mutable { ++a; return count() && a == 2 && rb == 3; }());
  return a + b + ra + rb;
}

// Templates.
template <class T> T tmpl(T x) pre([&] { return count() && x == 1; }()) pre([=]() mutable { ++x; return count() && x == 2; }()) {
  T local = x;
  contract_assert([=]() mutable { ++local; return count() && local == 2; }());
  contract_assert([&] { return count() && local == 1; }());
  contract_assert([&](auto y) { return count() && local == y; }(local));
  contract_assert([=](auto y) mutable { ++local; return count() && local == y + 1; }(local));
  return local;
}

int main() {
  int ref = 100;
  assert(copies(1, ref) == 111 && ref == 100);
  S s;
  assert(s.f() == 1 && s.m == 1 && s.g() == 1 && s.m == 1);
  assert(params(1, ref) == 101 && ref == 100);
  assert(params2(1, ref) == 101 && ref == 100);
  assert(bindings(P{1, 3}) == 8);
  assert(tmpl(1) == 1 && tmpl(1L) == 1);
#ifdef IGNORE
  assert(Evals == 0 && Violations == 0);
#else
  assert(Evals == 30 && Violations == 0);
#endif
  std::puts("ok");
  return 0;
}
