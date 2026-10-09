// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=enforce %libcxx_flags -o %t && %t

// A throwing violation handler on a FUNCTION contract assertion escapes every
// try/catch inside the function.
//
// [basic.contract.eval]/17: "If a contract-violation handler invoked from the
// evaluation of a function contract assertion ([dcl.contract.func]) exits via
// an exception, the behavior is as if the function body exits via that same
// exception.  [Note: A function-try-block ([except.pre]) is the function body
// when present and thus does not have an opportunity to catch the exception.
// ...]"
//
// So a try/catch WITHIN the body cannot catch it either: a postcondition is
// evaluated after the body has been exited and after locals are destroyed
// ([stmt.return]), and a precondition before it is entered.
//
// The control is the same paragraph's second note: an exception from a
// handler invoked by an ASSERTION-STATEMENT continues the search for a
// handler from that statement, so a try around a contract_assert does catch.
//
// Both compilers are correct here; nothing covered it.
//
// GCC mirror: g++.dg/contracts/cpp26/postcondition-throw-escapes-try.C

#include <contracts>

struct Bail {};

void handle_contract_violation(const std::contracts::contract_violation &) {
  throw Bail{};
}

static bool inner_catch_ran = false;
static bool dtor_ran = false;

struct Noisy {
  ~Noisy() { dtor_ran = true; }
};

// A try/catch wrapping the return statement.
static int post_around_return() post(r : r > 0) {
  try {
    return -1;
  } catch (...) {
    inner_catch_ran = true;
    return 1;
  }
}

// A function-try-block: it IS the function body.
static int post_function_try_block() post(r : r > 0) try {
  return -1;
} catch (...) {
  inner_catch_ran = true;
  return 1;
}

// A precondition: the body has not been entered.
static int pre_with_body_try() pre(false) {
  try {
    return 1;
  } catch (...) {
    inner_catch_ran = true;
    return 2;
  }
}

// CONTROL: an assertion-statement inside the try IS caught there.
static int assert_inside_try() post(r : r > 0) {
  try {
    contract_assert(false);
    return -1;
  } catch (...) {
    inner_catch_ran = true;
    return 1;
  }
}

// A local with a destructor: destroyed before the postcondition runs, and the
// catch still must not run.
static int post_with_local_dtor() post(r : r > 0) {
  try {
    Noisy n;
    (void)n;
    return -1;
  } catch (...) {
    inner_catch_ran = true;
    return 1;
  }
}

// Nested try/catch, both enclosing the return: neither may run.
static int post_nested_try() post(r : r > 0) {
  try {
    try {
      return -1;
    } catch (...) {
      inner_catch_ran = true;
      return 1;
    }
  } catch (...) {
    inner_catch_ran = true;
    return 2;
  }
}

// A try/catch that does not enclose the return statement at all.
static int post_try_not_around_return() post(r : r > 0) {
  try {
    inner_catch_ran = false;
  } catch (...) {
    inner_catch_ran = true;
  }
  return -1;
}

// Call `f`, requiring that Bail reach us and that no handler inside f ran.
template <class F> static void must_escape(F f) {
  inner_catch_ran = false;
  bool caught = false;
  bool completed = false;
  try {
    f();
    completed = true;
  } catch (const Bail &) {
    caught = true;
  }
  if (!caught)
    __builtin_abort();
  if (inner_catch_ran)
    __builtin_abort();
  if (completed)
    __builtin_abort();
}

int main() {
  must_escape(post_around_return);
  must_escape(post_function_try_block);
  must_escape(pre_with_body_try);
  must_escape(post_nested_try);
  must_escape(post_try_not_around_return);

  dtor_ran = false;
  must_escape(post_with_local_dtor);
  if (!dtor_ran) // locals are destroyed before the postcondition is evaluated
    __builtin_abort();

  // The control: this one IS caught inside the function, which then returns
  // normally and satisfies its own postcondition.
  inner_catch_ran = false;
  if (assert_inside_try() != 1)
    __builtin_abort();
  if (!inner_catch_ran)
    __builtin_abort();

  return 0;
}
