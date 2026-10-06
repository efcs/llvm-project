// A contract whose predicate is type-dependent but nonetheless holds a
// temporary with a non-trivial destructor leaves the enclosing cleanup state
// dirty: ActOnContractAssertCondition returns early for a dependent predicate
// and so never runs the ActOnFinishFullExpr that would reset it.  Finishing
// the statement with ActOnFinishFullStmt in that state wraps its argument in
// CompoundStmt -> StmtExpr -> ExprWithCleanups.  The result is no longer a
// ContractStmt, and every collection site stores it with
// ActionResult::getAs<ContractStmt>() -- a static_cast, not a dyn_cast -- so
// the wrapper is filed into the ContractSpecifierDecl as a bogus
// ContractStmt.  Every later walk of the contract list then reads garbage:
// printing its source range crashes, and rebuilding it for an out-of-line
// definition segfaults in TransformContractStmt.
//
// The shape is an ordinary one, and turns up in a contracts-enabled build of
// BDE (bdlmt_eventscheduler.h): a member function template declared in class
// with a precondition over a chrono duration, defined out of line.
//
// This is a RUNTIME test on purpose.  A fix that merely stops the crash but
// leaves a mis-typed node in the specifier would still compile; only
// evaluating each predicate, against the definition's own parameters, shows
// that the contract survived intact.  Predicates are written so that a wrong
// answer differs from the right one: 10 <= 5 is false, 10 <= 20 is true.
//
// GCC accepts all of this already; the guard is mirrored as
// g++.dg/contracts/cpp26/contract-dependent-predicate-temporary.C.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts \
// RUN:   -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

#include <contracts>
#include <cstdio>
#include <cstdlib>

static int Violations = 0;

void handle_contract_violation(const std::contracts::contract_violation &) {
  ++Violations;
}

// A class type with a NON-TRIVIAL destructor: that is what makes
// MaybeBindToTemporary set the "expression needs cleanups" flag.  The
// destructor must stay non-trivial or this test stops testing anything.
struct Guard {
  long V;
  Guard(long X) : V(X) {}
  ~Guard() {}
};
bool operator<=(const Guard &A, long B) { return A.V <= B; }

// A trivially destructible counterpart, for the control below.
struct Plain {
  long V;
  Plain(long X) : V(X) {}
};
bool operator<=(const Plain &A, long B) { return A.V <= B; }

//===--------------------------------------------------------------------===//
// The crashing shape: a member function template declared in class with a
// contract and defined out of line WITHOUT one, so the contract has to be
// rebuilt against the definition's parameters.
//===--------------------------------------------------------------------===//

struct MemberOutOfLine {
  template <class T>
  void pre_only(const T &V) pre(Guard(10) <= V);

  // The return type is T, so the result name is dependent too.
  template <class T>
  T post_only(const T &V) post(R : Guard(10) <= R);

  template <class T>
  T both(const T &V) pre(Guard(10) <= V) post(R : Guard(10) <= R);

  // Two preconditions, so a fix that only ever repairs the first is caught.
  template <class T>
  void two_pre(const T &V) pre(Guard(10) <= V) pre(Guard(1) <= V);
};

template <class T>
void MemberOutOfLine::pre_only(const T &V) { (void)V; }

template <class T>
T MemberOutOfLine::post_only(const T &V) { return V; }

template <class T>
T MemberOutOfLine::both(const T &V) { return V; }

template <class T>
void MemberOutOfLine::two_pre(const T &V) { (void)V; }

//===--------------------------------------------------------------------===//
// Free function template: declaration, then definition without the contract.
//===--------------------------------------------------------------------===//

template <class T>
void free_decl_then_def(const T &V) pre(Guard(10) <= V);

template <class T>
void free_decl_then_def(const T &V) { (void)V; }

//===--------------------------------------------------------------------===//
// Contract written on the definition itself: no rebuild, but the node still
// has to come out of the parse as a real ContractStmt.
//===--------------------------------------------------------------------===//

template <class T>
void on_definition(const T &V) pre(Guard(10) <= V) { (void)V; }

//===--------------------------------------------------------------------===//
// contract_assert in a template body.
//===--------------------------------------------------------------------===//

template <class T>
void in_body(const T &V) { contract_assert(Guard(10) <= V); }

//===--------------------------------------------------------------------===//
// Controls that must keep working.
//===--------------------------------------------------------------------===//

// Trivially destructible temporary: the cleanup flag is never set.
struct TrivialTemp {
  template <class T>
  void f(const T &V) pre(Plain(10) <= V);
};
template <class T>
void TrivialTemp::f(const T &V) { (void)V; }

// Non-dependent predicate: ActOnContractAssertCondition runs the
// full-expression finalization, which resets the cleanup flag.
struct NonDependent {
  template <class T>
  void f(const T &, long W) pre(Guard(10) <= W);
};
template <class T>
void NonDependent::f(const T &, long W) { (void)W; }

// Not a template at all.
struct NotATemplate {
  void f(long W) pre(Guard(10) <= W);
};
void NotATemplate::f(long W) { (void)W; }

//===--------------------------------------------------------------------===//

static void expect(int Want, const char *What) {
  if (Violations != Want) {
    std::printf("FAIL: %s: expected %d violations, got %d\n", What, Want,
                Violations);
    std::exit(1);
  }
}

int main() {
  MemberOutOfLine M;

  Violations = 0;
  M.pre_only(20);
  expect(0, "pre_only satisfied");
  M.pre_only(5);
  expect(1, "pre_only violated");

  Violations = 0;
  M.post_only(20);
  expect(0, "post_only satisfied");
  M.post_only(5);
  expect(1, "post_only violated");

  Violations = 0;
  M.both(20);
  expect(0, "both satisfied");
  M.both(5);
  expect(2, "both violated"); // precondition and postcondition

  Violations = 0;
  M.two_pre(20);
  expect(0, "two_pre satisfied");
  M.two_pre(5);
  expect(1, "two_pre: only the 10 <= V precondition fails");
  M.two_pre(0);
  expect(3, "two_pre: both preconditions fail");

  Violations = 0;
  free_decl_then_def(20);
  expect(0, "free_decl_then_def satisfied");
  free_decl_then_def(5);
  expect(1, "free_decl_then_def violated");

  Violations = 0;
  on_definition(20);
  expect(0, "on_definition satisfied");
  on_definition(5);
  expect(1, "on_definition violated");

  Violations = 0;
  in_body(20);
  expect(0, "in_body satisfied");
  in_body(5);
  expect(1, "in_body violated");

  Violations = 0;
  TrivialTemp T;
  T.f(20);
  expect(0, "TrivialTemp satisfied");
  T.f(5);
  expect(1, "TrivialTemp violated");

  Violations = 0;
  NonDependent N;
  N.f(0, 20);
  expect(0, "NonDependent satisfied");
  N.f(0, 5);
  expect(1, "NonDependent violated");

  Violations = 0;
  NotATemplate P;
  P.f(20);
  expect(0, "NotATemplate satisfied");
  P.f(5);
  expect(1, "NotATemplate violated");

  std::puts("PASS");
  return 0;
}
