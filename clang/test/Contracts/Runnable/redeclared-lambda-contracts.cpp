// Check that function contract specifiers which contain lambda-expressions
// and which are repeated on a redeclaration are evaluated exactly once.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.O0
// RUN: %t.O0
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.O2
// RUN: %t.O2

#include "contracts-runtime.h"
#include "my_assert.h"
using namespace std::contracts;
int Violations = 0;
void handle_contract_violation(const contract_violation &V) { ++Violations; }

int Evals = 0;
bool count(bool B) { ++Evals; return B; }
int f(int x) pre([&](int v) { return count(v > 0); }(x));
int f(int y) pre([&](int v) { return count(v > 0); }(y)) { return y; }

struct S {
  int m = 5;
  int g(int x) pre([this](int v) { return count(v > m); }(x)) post(r : [](int v) { return v > 10; }(r));
};
int S::g(int y) pre([this](int v) { return count(v > m); }(y)) post(s : [](int v) { return v > 10; }(s)) { return y; }

template <class T> T t(T x) pre([](T v) { return v > 0; }(x));
template <class U> U t(U y) pre([](U v) { return v > 0; }(y)) { return y; }

int main() {
  f(1);
  assert(Evals == 1 && Violations == 0);
  f(0);
  assert(Evals == 2 && Violations == 1);
  S s;
  s.g(11);
  assert(Evals == 3 && Violations == 1);
  s.g(1);
  assert(Evals == 4 && Violations == 3);
  t(1); assert(Violations == 3);
  t(0L); assert(Violations == 4);
}
