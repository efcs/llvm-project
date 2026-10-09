// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fno-exceptions -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t && %t

// The contracts surface under -fno-exceptions.
//
// The whole violation model is exception-shaped -- a handler may throw, a
// predicate may throw -- so every part of it has to degrade sensibly when
// there are no exceptions at all, and produce the same observable behaviour
// as with exceptions enabled.
//
// GCC mirror: g++.dg/contracts/cpp26/noexceptions-contracts.C

#include <contracts>
#include <coroutine>

static int violations = 0;

void handle_contract_violation(const std::contracts::contract_violation &) {
  ++violations;
}

static int clamp(const int x) pre(x >= 0) post(r : r >= 0) { return x; }

// A predicate that calls a function with no exception specification is what
// turns on the catch around predicate evaluation when exceptions are enabled;
// with -fno-exceptions it must simply not be built.
bool maybe_throws(int x) { return x > 0; }

static int predicate_may_throw(const int x) pre(maybe_throws(x)) { return x; }

template <class T> static T templated(const T x) pre(x != T{}) post(r : r == x) {
  return x;
}

static int non_throwing(const int x) noexcept pre(x > 0) { return x; }

// A coroutine: its promise still has unhandled_exception, and the ramp still
// evaluates the contracts, with no exceptions available anywhere.
struct Task {
  struct promise_type {
    int value = 0;
    Task get_return_object() {
      return Task{std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    std::suspend_never initial_suspend() noexcept { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void return_value(int v) { value = v; }
    void unhandled_exception() {}
  };

  std::coroutine_handle<promise_type> h;
  ~Task() {
    if (h)
      h.destroy();
  }
  int get() const { return h.promise().value; }
};

static Task doubler(const int x) pre(x > 0) post(r : true) { co_return x * 2; }

int main() {
  int before;

  before = violations;
  clamp(1);
  if (violations != before)
    __builtin_abort();

  before = violations;
  clamp(-1); // both the precondition and the postcondition fail
  if (violations - before != 2)
    __builtin_abort();

  before = violations;
  predicate_may_throw(-1);
  if (violations - before != 1)
    __builtin_abort();

  before = violations;
  templated(7);
  templated(0);
  if (violations - before != 1)
    __builtin_abort();

  before = violations;
  non_throwing(-1);
  if (violations - before != 1)
    __builtin_abort();

  before = violations;
  contract_assert(1 == 2);
  if (violations - before != 1)
    __builtin_abort();

  before = violations;
  {
    Task t = doubler(3);
    if (t.get() != 6 || violations != before)
      __builtin_abort();
  }
  before = violations;
  {
    Task t = doubler(-1);
    (void)t;
    if (violations - before != 1)
      __builtin_abort();
  }

  // A non-throwing function stays non-throwing; a contract does not make an
  // ordinary function noexcept.
  if (!noexcept(non_throwing(1)))
    __builtin_abort();
  if (noexcept(clamp(1)))
    __builtin_abort();

  return 0;
}
