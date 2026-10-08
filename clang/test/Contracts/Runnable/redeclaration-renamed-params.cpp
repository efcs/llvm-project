// The contracts of a function are those of its first declaration; a definition
// which omits them (and renames the parameters and the result binding)
// evaluates them against its own parameters and result, exactly once.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.O0
// RUN: %t.O0
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.O2
// RUN: %t.O2
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -DREPEAT -O0 %s -o %t.R0
// RUN: %t.R0

#include "contracts-runtime.h"
#include "my_assert.h"
using namespace std::contracts;
int Violations = 0;
void handle_contract_violation(const contract_violation &V) { ++Violations; }

#ifdef REPEAT
#define CONTRACTS(...) __VA_ARGS__
#else
#define CONTRACTS(...)
#endif

int Evals = 0;
bool count(bool B) { ++Evals; return B; }

// Free function: the definition renames the parameters and the result name.
int add(const int x, const int w) pre(count(x > w)) post(r : count(r > x));
int add(const int a, const int b) CONTRACTS(pre(count(a > b)) post(s : count(s > a))) {
  return a + b;
}

// A lambda-expression in the predicate, capturing the parameter.
int lam(int x) pre([&] { return count(x > 0); }());
int lam(int y) CONTRACTS(pre([&] { return count(y > 0); }())) { return y; }

// Member function defined out of line.
struct S {
  int m;
  int get(const int x) pre(count(x > m)) post(r : count(r == x + m));
  int& ref(int &x) post(r : count(&r == &x));
};
int S::get(const int y) CONTRACTS(pre(count(y > m)) post(s : count(s == y + m))) {
  return y + m;
}
int& S::ref(int &y) CONTRACTS(post(s : count(&s == &y))) { return y; }

// Function template declared with the contracts, defined without.
template <class T> T tmpl(const T x) pre(count(x > T{})) post(r : count(r > x));
template <class U> U tmpl(const U y) CONTRACTS(pre(count(y > U{})) post(s : count(s > y))) {
  return y + 1;
}

// Member of a class template declared in-class, defined out of line.
template <class T> struct CT {
  T get(const T x) pre(count(x > T{})) post(r : count(r > x));
};
template <class T> T CT<T>::get(const T y) CONTRACTS(pre(count(y > T{})) post(s : count(s > y))) {
  return y + 1;
}

int main() {
  assert(add(2, 1) == 3);
  assert(Evals == 2 && Violations == 0);
  add(1, 2);
  assert(Evals == 4 && Violations == 1);

  Evals = Violations = 0;
  lam(1);
  assert(Evals == 1 && Violations == 0);
  lam(0);
  assert(Evals == 2 && Violations == 1);

  Evals = Violations = 0;
  S s{5};
  assert(s.get(6) == 11);
  assert(Evals == 2 && Violations == 0);
  s.get(1);
  assert(Evals == 4 && Violations == 1);
  int v = 0;
  assert(&s.ref(v) == &v);
  assert(Evals == 5 && Violations == 1);

  Evals = Violations = 0;
  assert(tmpl(1) == 2 && tmpl(1L) == 2L);
  assert(Evals == 4 && Violations == 0);
  tmpl(0);
  assert(Evals == 6 && Violations == 1);

  Evals = Violations = 0;
  assert(CT<int>().get(1) == 2);
  assert(Evals == 2 && Violations == 0);
  CT<int>().get(0);
  assert(Evals == 4 && Violations == 1);
}
