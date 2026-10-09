// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts \
// RUN:   -fcontract-evaluation-semantic=observe %libcxx_flags -o %t
// RUN: %t

// Mirror of g++.dg/contracts/cpp26/contract-declarator-positions-run.C, for
// the rows that ask WHICH parameter a contract predicate's name resolves to.
//
// A function-contract-specifier-seq follows the complete declarator, so it
// belongs to the function that declarator declares -- however complicated the
// declarator is, and whether or not the return type happens to contain a
// parameter list of its own.  Two things can go wrong when it does, and only
// a test that RUNS can tell them apart:
//
//   * the contract binds to an abstract declarator that never becomes a
//     function and is silently discarded, so no call is ever checked; or
//   * it reaches the right function, but its predicate was parsed while the
//     return type's parameter scope was still open, so a name in it resolves
//     to a parameter of the return type instead.
//
// [basic.scope.param]/1.1 extends the function parameter scope to the end of
// the init-declarator, and the contract is inside it, so the function's own
// parameters are what the predicate sees.  The return type's
// parameter-declaration-clause is a separate parameter scope and does not
// reach here at all.
//
// This is GCC-45 / PR127572 on the GCC side: stock g++ checks the classic
// spelling, drops both trailing-return spellings, and resolves d1's predicate
// against the return type's parameter.  Clang has all three right; this file
// is the regression pin.

#include <contracts>
#include <cstdio>

static int violations = 0;
static int expected = 0;

void handle_contract_violation(const std::contracts::contract_violation &) {
  ++violations;
}

// Call after a call that owes exactly one violation.  The semantic is
// `observe`, so the call returns and execution continues.
static void expect_one(const char *what) {
  ++expected;
  if (violations != expected) {
    std::printf("contract not checked: %s\n", what);
    __builtin_abort();
  }
}

// Call after a call whose contract must hold.  A dropped contract is
// indistinguishable from a satisfied one here, which is why each expect_none
// row is paired with an expect_one row inverting the same predicate.
static void expect_none(const char *what) {
  if (violations != expected) {
    std::printf("contract wrongly violated: %s\n", what);
    __builtin_abort();
  }
}

static int callee(int) { return 0; }
static long callee_long(long) { return 0; }
static bool flag = false;

template <class T, class U> constexpr bool same_v = false;
template <class T> constexpr bool same_v<T, T> = true;

// The contract must be attached even though a parameter list belonging to the
// return type stands between it and the declarator-id.

int (*b1(int i))(int) pre(i > 0);
int (*b1(int i))(int) { (void)i; return callee; }

int (*b2(int))(int) pre(flag);
int (*b2(int))(int) { return callee; }

// The same declaration with a trailing return type.  The parameter list now
// sits inside the type-id; the contract still belongs to the function.

auto b3(int) -> int (*)(int) pre(flag);
auto b3(int) -> int (*)(int) { return callee; }

auto b4(int) -> int (&)(int) post(r : flag);
auto b4(int) -> int (&)(int) { return callee; }

// Which parameter does the name find?  The function's own parameter is
// `int i`; the return type `long (*)(long)` declares its own `long i`.
// d1 must be silent and d2 must report.  A compiler that binds to the wrong
// parameter inverts both; one that drops the contract fails d2 alone -- so
// neither row can pass for the wrong reason.

long (*d1(int i))(long i) pre(same_v<decltype(i), int>);
long (*d1(int i))(long) { (void)i; return callee_long; }

long (*d2(int i))(long i) pre(!same_v<decltype(i), int>);
long (*d2(int i))(long) { (void)i; return callee_long; }

int main() {
  b1(-1); expect_one("b1  classic, predicate names a parameter");
  b2(0);  expect_one("b2  classic, parameter-free predicate");
  b3(0);  expect_one("b3  trailing return, pointer to function");
  b4(0);  expect_one("b4  trailing return, reference to function, post");

  d1(1);  expect_none("d1  predicate names the function's parameter, not the "
                      "return type's");
  d2(1);  expect_one("d2  the same, inverted");

  return 0;
}
