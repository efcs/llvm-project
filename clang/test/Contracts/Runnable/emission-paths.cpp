// Check that function contract assertions are evaluated exactly once per call
// no matter how the function is called or emitted: through pointers to
// functions and members, lambda call operators and their conversion
// functions, inlined functions, main, and so on.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.observe
// RUN: %t.observe
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.observe.O2
// RUN: %t.observe.O2
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce -O1 %s -o %t.enforce
// RUN: %t.enforce
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=quick_enforce -O2 %s -o %t.quick
// RUN: %t.quick
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -O0 -DIGNORED %s -o %t.ignore
// RUN: %t.ignore
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 -fno-exceptions -fno-rtti %s -o %t.noeh
// RUN: %t.noeh

#include "contracts-runtime.h"
#include "event-log.h"
#include <stdarg.h>

void handle_contract_violation(const std::contracts::contract_violation &) {
  ev("violation");
}

__attribute__((noinline)) bool check(const char *Event) { return ev(Event); }

// The contracts in this test are all of this form.
#define CONTRACTS(RESULT)                                                      \
  pre(check("pre")) post(R : check("post") && R == (RESULT))
// The events of a call to a function with these contracts.
#define CALL(BODY) C("pre;") BODY C("post;")

__attribute__((noinline)) int function(const int X) CONTRACTS(X) {
  ev("function");
  return X;
}

struct S {
  int M = 1;
  __attribute__((noinline)) int member(const int X) const CONTRACTS(X + M) {
    ev("member");
    return X + M;
  }
  __attribute__((noinline)) static int static_member(const int X) CONTRACTS(X) {
    ev("static_member");
    return X;
  }
  // A function contract assertion can be omitted from the definition.
  int declared(const int X) const CONTRACTS(X + M);
};
__attribute__((noinline)) int S::declared(const int Y) const {
  ev("declared");
  return Y + M;
}

__attribute__((always_inline)) inline int always_inline(const int X)
    CONTRACTS(X) {
  ev("always_inline");
  return X;
}
[[gnu::flatten]] __attribute__((noinline)) int flatten(const int X)
    CONTRACTS(X) {
  ev("flatten");
  return always_inline(X);
}

template <class T> __attribute__((noinline)) T templ(const T X) CONTRACTS(X) {
  ev("templ");
  return X;
}

constexpr int constexpr_fn(const int X) CONTRACTS(X) { return X; }

__attribute__((noinline)) int variadic(const int N, ...) CONTRACTS(N * 2) {
  va_list Args;
  va_start(Args, N);
  int Sum = 0;
  for (int I = 0; I < N; ++I)
    Sum += va_arg(Args, int);
  va_end(Args);
  ev("variadic");
  return Sum;
}

__attribute__((noinline)) int recursive(const int X) CONTRACTS(X) {
  ev("recursive");
  if (X == 0)
    return 0;
  return recursive(X - 1) + 1;
}

__attribute__((noinline)) int default_arg(
    int X = [](const int Y) CONTRACTS(Y) { return ev("lambda"), Y; }(3)) {
  return X;
}

template <class F> __attribute__((noinline)) int call(F Fn, int X) {
  return Fn(X);
}

void test() {
  function(1);
  EXPECT_EVENTS(CALL("function;"));
  int (*volatile FnPtr)(int) = function;
  FnPtr(1);
  EXPECT_EVENTS(CALL("function;"));

  S Obj;
  Obj.member(1);
  EXPECT_EVENTS(CALL("member;"));
  int (S::*volatile MemPtr)(int) const = &S::member;
  (Obj.*MemPtr)(1);
  EXPECT_EVENTS(CALL("member;"));
  S::static_member(1);
  EXPECT_EVENTS(CALL("static_member;"));
  Obj.declared(1);
  EXPECT_EVENTS(CALL("declared;"));

  always_inline(1);
  EXPECT_EVENTS(CALL("always_inline;"));
  flatten(1);
  EXPECT_EVENTS(CALL("flatten;" CALL("always_inline;")));

  templ(1);
  templ(1L);
  EXPECT_EVENTS(CALL("templ;") CALL("templ;"));

  volatile int One = 1;
  constexpr_fn(One);
  EXPECT_EVENTS(CALL(""));

  variadic(2, 1, 3);
  EXPECT_EVENTS(CALL("variadic;"));

  recursive(2);
  EXPECT_EVENTS(C("pre;") "recursive;" C("pre;") "recursive;" C("pre;")
                "recursive;" C("post;post;post;"));

  // Lambdas: the contracts belong to the function call operator. The
  // conversion function to pointer to function returns a function which calls
  // the call operator.
  auto Lambda = [](const int X) CONTRACTS(X) { return ev("lambda"), X; };
  Lambda(1);
  EXPECT_EVENTS(CALL("lambda;"));
  int (*volatile LambdaPtr)(int) = Lambda;
  LambdaPtr(1);
  EXPECT_EVENTS(CALL("lambda;"));
  call(Lambda, 1);
  EXPECT_EVENTS(CALL("lambda;"));

  auto Generic = [](const auto X) CONTRACTS(X) { return ev("generic"), X; };
  Generic(1);
  Generic(1L);
  EXPECT_EVENTS(CALL("generic;") CALL("generic;"));
  int (*volatile GenericPtr)(int) = Generic;
  GenericPtr(1);
  EXPECT_EVENTS(CALL("generic;"));

  auto Static = [](const int X) static CONTRACTS(X) {
    return ev("static"), X;
  };
  Static(1);
  EXPECT_EVENTS(CALL("static;"));
  int (*volatile StaticPtr)(int) = Static;
  StaticPtr(1);
  EXPECT_EVENTS(CALL("static;"));

  int Captured = 2;
  auto Capturing = [Captured](const int X) CONTRACTS(X + Captured) {
    return ev("capturing"), X + Captured;
  };
  Capturing(1);
  EXPECT_EVENTS(CALL("capturing;"));

  default_arg();
  EXPECT_EVENTS(CALL("lambda;"));
  default_arg();
  EXPECT_EVENTS(CALL("lambda;"));
}

// main can have function contract assertions too. Check that they have been
// evaluated when the program exits.
bool MainReturned = false;
struct CheckAtExit {
  ~CheckAtExit() {
    if (!MainReturned)
      return;
    EXPECT_EVENTS("main;" C("main.post;"));
  }
} CheckAtExitObj;

int main(int, char **)
    pre(check("main.pre"))
    post(R : check("main.post") && R == 0) {
  EXPECT_EVENTS(C("main.pre;"));
  test();
  ev("main");
  MainReturned = true;
}
