// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts \
// RUN:   -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

// A contract declared in-class on a member of a CLASS TEMPLATE, where that
// member is DEFINED OUT-OF-LINE.
//
// Two contract specifiers exist on the pattern's redeclaration chain: the
// in-class declaration's, which class-template instantiation installs on the
// instantiated member, and the out-of-line definition's own copy, which is
// what InstantiateFunctionDefinition substitutes from.  A substitution that
// mistakes one for the other leaves the instantiation holding a dependent
// predicate that references the *pattern's* ParmVarDecls, and CodeGen then
// hits whichever assertion the predicate's contents reach first -- a
// parameter reference, `sizeof(T)`, a member call, and so on.  One bug,
// several faces; the shapes below cover them.
//
// The test is a RUNTIME one on purpose.  Compiling is not the property worth
// pinning: a fix that substituted against the wrong parameter would still
// compile and would silently check garbage.  What each case asserts is that
// the predicate is evaluated against THIS call's arguments -- it must fire on
// the bad call and stay silent on the good one.

#include <contracts>
#include <cstdio>

static int violation_count = 0;

void handle_contract_violation(const std::contracts::contract_violation &) {
  ++violation_count;
}

static int failures = 0;

static void expect(int expected, const char *what) {
  if (violation_count != expected) {
    std::printf("FAIL: %s: expected %d violation(s), got %d\n", what, expected,
                violation_count);
    ++failures;
  }
  violation_count = 0;
}

// ---------------------------------------------------------------------------
// The crashing shapes: member of a class template, defined out-of-line.
// ---------------------------------------------------------------------------

// Predicate names a parameter.
template <int N> struct ParmRef {
  int f(int rhs) pre(rhs >= 0);
};
template <int N> int ParmRef<N>::f(int rhs) { return rhs; }

// Predicate names a parameter of dependent type, and the class template is
// parameterized by a type rather than a value.
template <class T> struct DependentParm {
  T f(T lo, T hi) pre(lo <= hi);
};
template <class T> T DependentParm<T>::f(T lo, T hi) { return hi; }

// Predicate mentions the template parameter itself as well as a parameter.
template <int N> struct TemplateParmRef {
  int f(int i) pre(i < N);
};
template <int N> int TemplateParmRef<N>::f(int i) { return i; }

// Predicate is a dependent type-trait expression plus a parameter.
template <class T> struct SizeofPred {
  int f(T v) pre(sizeof(T) > 0 && v != T());
};
template <class T> int SizeofPred<T>::f(T v) { return 1; }

// Predicate calls another member -- the "Call must have function pointer
// type" face of the same bug.
template <class T> struct MemberCall {
  bool ok(T v) const;
  int f(T v) pre(ok(v));
};
template <class T> bool MemberCall<T>::ok(T v) const { return v > T(); }
template <class T> int MemberCall<T>::f(T v) { return 1; }

// Deduced return type on the out-of-line definition.
template <class T> struct AutoReturn {
  auto f(T v) pre(v != T());
};
template <class T> auto AutoReturn<T>::f(T v) { return v; }

// A postcondition naming a parameter (which must be const) and the result.
template <int N> struct PostParm {
  int f(const int rhs) post(r : r >= rhs);
};
template <int N> int PostParm<N>::f(const int rhs) { return rhs - N; }

// Both a pre and a post on one out-of-line member.
template <int N> struct Both {
  int f(const int rhs) pre(rhs >= 0) post(r : r == rhs);
};
template <int N> int Both<N>::f(const int rhs) { return rhs; }

// ---------------------------------------------------------------------------
// Controls: these always worked and must keep working.  A fix that stopped
// distinguishing "placeholder" from "already substituted" would either
// re-substitute these or skip them.
// ---------------------------------------------------------------------------

template <int N> struct InlineDef {
  int f(int rhs) pre(rhs >= 0) { return rhs; }
};

struct NonTemplate {
  int f(int rhs) pre(rhs >= 0);
};
int NonTemplate::f(int rhs) { return rhs; }

template <int N> int freeFn(int rhs) pre(rhs >= 0);
template <int N> int freeFn(int rhs) { return rhs; }

int main() {
  ParmRef<1> pr;
  pr.f(5);
  expect(0, "ParmRef good");
  pr.f(-1);
  expect(1, "ParmRef bad");

  // A second instantiation must get its own substituted specifier.
  ParmRef<2> pr2;
  pr2.f(-1);
  expect(1, "ParmRef<2> bad");

  DependentParm<long> dp;
  dp.f(1, 2);
  expect(0, "DependentParm good");
  dp.f(2, 1);
  expect(1, "DependentParm bad");

  TemplateParmRef<4> tp;
  tp.f(1);
  expect(0, "TemplateParmRef good");
  tp.f(9);
  expect(1, "TemplateParmRef bad");

  SizeofPred<int> sp;
  sp.f(1);
  expect(0, "SizeofPred good");
  sp.f(0);
  expect(1, "SizeofPred bad");

  MemberCall<int> mc;
  mc.f(1);
  expect(0, "MemberCall good");
  mc.f(-1);
  expect(1, "MemberCall bad");

  AutoReturn<int> ar;
  ar.f(1);
  expect(0, "AutoReturn good");
  ar.f(0);
  expect(1, "AutoReturn bad");

  PostParm<0> pp0;
  pp0.f(5);
  expect(0, "PostParm good");
  PostParm<1> pp1;
  pp1.f(5); // returns 4, which is < rhs
  expect(1, "PostParm bad");

  Both<1> bo;
  bo.f(3);
  expect(0, "Both good");
  bo.f(-3); // pre fires; post then compares -3 == -3 and holds
  expect(1, "Both bad");

  InlineDef<1> id;
  id.f(-1);
  expect(1, "InlineDef bad");

  NonTemplate nt;
  nt.f(-1);
  expect(1, "NonTemplate bad");

  freeFn<1>(-1);
  expect(1, "freeFn bad");

  if (failures == 0)
    std::puts("PASS");
  return failures;
}
