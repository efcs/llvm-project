// Check that a result binding captured by a lambda-expression within a
// postcondition denotes the actual result object (or referent) of the
// function, by copy and by reference, at -O0 and -O2.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.O0
// RUN: %t.O0
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.O2
// RUN: %t.O2
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce -O0 -DENFORCE %s -o %t.enforce
// RUN: %t.enforce

#include "contracts-runtime.h"
#include "my_assert.h"

#include <cstdio>
#include <cstdlib>

int Violations = 0;
void handle_contract_violation(const std::contracts::contract_violation &V) {
  ++Violations;
#ifdef ENFORCE
  std::exit(0);
#endif
}

const void *Seen = nullptr;
bool see(const void *p) {
  Seen = p;
  return true;
}

// Captures of a scalar result.
int f1(int x) post(r: [&] { return r == 42; }()) { return x; }
int f2(int x) post(r: [r] { return r == 42; }()) { return x; }
int f3(int x) post(r: [=] { return r == 42; }()) { return x; }
int f4(int x) post(r: [y = r] { return y == 42; }()) { return x; }
int f5(int x) post(r: [&y = r] { return y == 42; }()) { return x; }
int f6(int x) post(r: [&r] { return r == 42; }()) { return x; }
// Modifying a copy does not affect the result.
int f7(int x) post(r: [=]() mutable { ++r; return r == 43; }()) post(r: r == 42) { return x; }
int f8(int x) post(r: [&] { return [&] { return [=] { return r == 42; }(); }(); }()) { return x; }

// Functions returning references: the binding denotes the referent.
int G = 42;
int &g1(int &x) post(r: [&] { return &r == &G; }()) { return x; }
int &g2(int &x) post(r: [=]() mutable { ++r; return r == G + 1; }()) post(r: r == 42) { return x; }
int &g3(int &x) post(r: [&r] { return see(&r); }()) { return x; }
int &g4(int &x) post(r: [&y = r] { return see(&y); }()) { return x; }

// Class types: the binding denotes the result object itself, which is not
// copied unless the lambda captures it by copy.
struct NonTrivial {
  int v;
  NonTrivial(int v) : v(v) {}
  NonTrivial(const NonTrivial &o) : v(o.v) { ++Copies; }
  ~NonTrivial() {}
  static int Copies;
};
int NonTrivial::Copies = 0;
NonTrivial h1(int x) post(r: [&] { return see(&r) && r.v == 42; }()) { return NonTrivial(x); }
NonTrivial h2(int x) post(r: [&] { return see(&r) && r.v == 42; }()) { NonTrivial n(x); return n; }
NonTrivial h3(int x) post(r: [r] { return r.v == 42; }()) { return NonTrivial(x); }
NonTrivial h4(int x) post(r: [=] { return [&] { return r.v == 42; }(); }()) { return NonTrivial(x); }
NonTrivial h5(int x) post(r: [&r] { return [r] { return r.v == 42; }(); }()) { return NonTrivial(x); }

// Templates, deduced return types and member functions.
template <class T> T t1(const T x) post(r: [&] { return r == x; }()) { return x; }
template <class T> auto t2(const T x) post(r: [&] { return r == x; }()) post(r: [r, x] { return r == x; }()) { return x; }
auto a1(const int x) post(r: [&] { return r == x; }()) { return x; }
auto a2(const int x) post(r: [r, x] { int j = r; return j == x; }()) { return x; }
auto a3(const int x) post(r: [x] { struct L { static int h(int v) { static int s = 0; return ++s + v; } }; return L::h(x) > 0; }() && r == x) { return x; }
struct S {
  int m = 42;
  int get() post(r: [&] { return r == m; }()) post(r: [=, this] { return r == m; }()) { return m; }
  const int &ref() post(r: [&] { return &r == &m; }()) { return m; }
  auto deduced() post(r: [this, r] { return r == m; }()) { return m; }
};
void lambdas() {
  auto l1 = [](const int x) post(r: [&] { return r == 42; }()) { return x; };
  auto l2 = [](const int x) post(r: [r] { int j = r; return j == 42; }()) { return x; };
  auto l3 = [](const auto x) post(r: [r] { return r == 42; }()) { return x; };
  assert(l1(42) == 42 && l2(42) == 42 && l3(42) == 42 && Violations == 0);
  l1(1);
  l2(1);
  l3(1);
  assert(Violations == 3);
  Violations = 0;
}

int main() {
  assert(f1(42) == 42 && Violations == 0); f1(1); assert(Violations == 1); Violations = 0;
  assert(f2(42) == 42 && Violations == 0); f2(1); assert(Violations == 1); Violations = 0;
  assert(f3(42) == 42 && Violations == 0); f3(1); assert(Violations == 1); Violations = 0;
  assert(f4(42) == 42 && Violations == 0); f4(1); assert(Violations == 1); Violations = 0;
  assert(f5(42) == 42 && Violations == 0); f5(1); assert(Violations == 1); Violations = 0;
  assert(f6(42) == 42 && Violations == 0); f6(1); assert(Violations == 1); Violations = 0;
  assert(f7(42) == 42 && Violations == 0); f7(1); assert(Violations == 2); Violations = 0;
  assert(f8(42) == 42 && Violations == 0); f8(1); assert(Violations == 1); Violations = 0;

  assert(&g1(G) == &G && Violations == 0);
  int L = 1;
  g1(L);
  assert(Violations == 1);
  Violations = 0;
  assert(&g2(G) == &G && G == 42 && Violations == 0);
  g3(G);
  assert(Seen == &G && Violations == 0);
  g4(G);
  assert(Seen == &G && Violations == 0);

  {
    NonTrivial n = h1(42);
    assert(Seen == &n && Violations == 0 && NonTrivial::Copies == 0);
  }
  {
    NonTrivial n = h2(42);
    assert(Seen == &n && Violations == 0);
  }
  NonTrivial::Copies = 0;
  {
    NonTrivial n = h3(42);
    assert(Violations == 0 && NonTrivial::Copies == 1);
    h3(1);
    assert(Violations == 1);
    Violations = 0;
  }
  NonTrivial::Copies = 0;
  {
    NonTrivial n = h4(42);
    assert(Violations == 0 && NonTrivial::Copies == 1);
  }
  NonTrivial::Copies = 0;
  {
    NonTrivial n = h5(42);
    assert(Violations == 0 && NonTrivial::Copies == 1);
  }

  assert(t1(5) == 5 && t1(5L) == 5 && t2(7) == 7 && a1(3) == 3 && a2(4) == 4 && a3(42) == 42 && Violations == 0);
  S s;
  assert(s.get() == 42 && &s.ref() == &s.m && s.deduced() == 42 && Violations == 0);
  lambdas();
  std::puts("ok");
#ifdef ENFORCE
  // Every violation above exits the program; check that one actually does.
  f1(1);
  std::puts("unreachable");
  return 1;
#endif
  return 0;
}
