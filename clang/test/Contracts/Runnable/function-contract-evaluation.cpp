// Check that function contract assertions are evaluated exactly once, at the
// right time, with the right value for the result name, and that throwing
// from a violation handler or a predicate behaves correctly.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.O0
// RUN: %t.O0
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.O2
// RUN: %t.O2
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 -Xclang -mno-constructor-aliases %s -o %t.noalias
// RUN: %t.noalias

#include "contracts-runtime.h"
#include "my_assert.h"

using namespace std::contracts;

int Violations = 0;
detection_mode LastMode = detection_mode::predicate_false;
assertion_kind LastKind = assertion_kind::assert;
bool ThrowFromHandler = false;

void handle_contract_violation(const contract_violation &V) {
  ++Violations;
  LastMode = V.detection_mode();
  LastKind = V.kind();
  if (ThrowFromHandler)
    throw 42;
}

int Evals = 0;
__attribute__((noinline)) bool count() {
  ++Evals;
  return true;
}

void reset() {
  Violations = 0;
  Evals = 0;
  ThrowFromHandler = false;
}

// Postconditions on void functions.
void void_fn() post(count()) {}
void void_fn_early(bool B) post(count()) {
  if (B)
    return;
}

// Structors.
struct Plain {
  Plain() pre(count()) post(count()) {}
  ~Plain() pre(count()) post(count()) {}
};

struct VBase {
  virtual ~VBase() {}
};
struct WithVBase : virtual VBase {
  WithVBase() pre(count()) post(count()) {}
  ~WithVBase() pre(count()) post(count()) {}
};
struct Derived : WithVBase {};

struct Inheriting : Plain {
  using Plain::Plain;
};

// Result names.
__attribute__((noinline)) int ret_param(const int X) post(R : R == X) {
  return X;
}
__attribute__((noinline)) int ret_branches(int X) post(R : R == 5) {
  if (X)
    return 5;
  return 5;
}
struct Big {
  int A[8];
};
__attribute__((noinline)) Big ret_big(const int X) post(R : R.A[0] == X) {
  Big B{};
  B.A[0] = X;
  return B;
}

int Global = 7;
int *GlobalAddr = nullptr;
__attribute__((noinline)) int &ret_ref() post(R : (GlobalAddr = &R, R == 7)) {
  return Global;
}
__attribute__((noinline)) const int &ret_cref() post(R : &R == &Global) {
  return Global;
}

// Return objects must be destroyed when a postcondition exits via exception.
int Live = 0;
struct Obj {
  int V;
  Obj(int V) : V(V) { ++Live; }
  Obj(const Obj &O) : V(O.V) { ++Live; }
  ~Obj() { --Live; }
};
Obj make(int X) post(R : R.V == 0) { return Obj(X); }
Obj make_nrvo(int X) post(R : R.V == 0) {
  Obj O(X);
  return O;
}

// Exceptions from constructors in the predicate are reported as
// evaluation_exception.
struct Thrower {
  Thrower() { throw 1; }
  bool ok() const noexcept { return true; }
};
void throwing_ctor() pre(Thrower().ok()) {}
void throwing_new() pre(new Thrower) {}

int main() {
  reset();
  void_fn();
  assert(Evals == 1);
  void_fn_early(true);
  void_fn_early(false);
  assert(Evals == 3);

  reset();
  { Plain P; }
  assert(Evals == 4);
  reset();
  { Plain *P = new Plain; delete P; }
  assert(Evals == 4);
  reset();
  { WithVBase W; }
  assert(Evals == 4);
  reset();
  { VBase *W = new WithVBase; delete W; }
  assert(Evals == 4);
  reset();
  { Derived D; }
  assert(Evals == 4);
  reset();
  { Inheriting I; }
  assert(Evals == 4);

  reset();
  assert(ret_param(42) == 42);
  assert(ret_branches(1) == 5);
  assert(ret_branches(0) == 5);
  assert(ret_big(6).A[0] == 6);
  assert(Violations == 0);

  assert(&ret_ref() == &Global);
  assert(GlobalAddr == &Global);
  assert(&ret_cref() == &Global);
  assert(Violations == 0);

  reset();
  ThrowFromHandler = true;
  try {
    make(1);
    assert(false);
  } catch (int) {
  }
  assert(Violations == 1 && LastKind == assertion_kind::post);
  assert(Live == 0);
  try {
    make_nrvo(1);
    assert(false);
  } catch (int) {
  }
  assert(Violations == 2);
  assert(Live == 0);

  reset();
  throwing_ctor();
  assert(Violations == 1 && LastMode == detection_mode::evaluation_exception);
  throwing_new();
  assert(Violations == 2 && LastMode == detection_mode::evaluation_exception);
  return 0;
}
