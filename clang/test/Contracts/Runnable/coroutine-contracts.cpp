// Check when the function contract assertions of a coroutine are evaluated.
//
// The function contract assertions apply to the function as seen by its
// callers (the "ramp"), not to the coroutine body:
//  - The preconditions are evaluated after the parameters have been
//    initialized and before anything else, in particular before the coroutine
//    state is allocated and the parameters are copied into it.
//  - The postconditions are evaluated when control returns to the caller for
//    the first time, i.e. when the coroutine first suspends or completes.
//    The result binding names the object returned to the caller.
//  - They are evaluated exactly once, and never when the coroutine is
//    resumed.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.observe
// RUN: %t.observe
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.observe.O2
// RUN: %t.observe.O2
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce -O1 -DENFORCE %s -o %t.enforce
// RUN: %t.enforce
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=quick_enforce -O2 -DNO_HANDLER %s -o %t.quick
// RUN: %t.quick
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -O0 -DIGNORED -DNO_HANDLER %s -o %t.ignore
// RUN: %t.ignore
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 -fno-exceptions %s -o %t.noeh
// RUN: %t.noeh

#include "../../CodeGenCoroutines/Inputs/coroutine.h"
#include "contracts-runtime.h"
#include "event-log.h"
#include <new>

using namespace std::contracts;

bool ThrowFromHandler = false;
void handle_contract_violation(const contract_violation &) {
  ev("violation");
#if __cpp_exceptions
  if (ThrowFromHandler)
    throw 42;
#endif
}

const char *Fail = "";
__attribute__((noinline)) bool check(const char *Event) {
  ev(Event);
  return strcmp(Fail, Event) != 0;
}

// Record that a step of the coroutine machinery ran and throw if requested.
const char *ThrowAt = "";
void step(const char *Event) {
  ev(Event);
#if __cpp_exceptions
  if (strcmp(ThrowAt, Event) == 0)
    throw 42;
#endif
}

bool FailAllocation = false;

// A parameter which is destroyed by the callee.
struct [[clang::trivial_abi]] Param {
  bool IsCopy = false;
  int Value = 1;
  Param() { ev("Param()"); }
  Param(Param &&O) : IsCopy(true), Value(O.Value) {
    O.Value = -1;
    ev("Param(move)");
  }
  ~Param() { ev(IsCopy ? "~Param(copy)" : "~Param()"); }
};

template <bool IsEager> struct Task {
  struct promise_type {
    promise_type() { step("promise()"); }
    ~promise_type() { ev("~promise()"); }
    Task get_return_object() {
      step("get_return_object");
      return Task(std::coroutine_handle<promise_type>::from_promise(*this));
    }
    struct InitialAwaiter {
      bool await_ready() {
        step("await_ready");
        return IsEager;
      }
      void await_suspend(std::coroutine_handle<>) { step("await_suspend"); }
      void await_resume() { ev("await_resume"); }
    };
    InitialAwaiter initial_suspend() {
      step("initial_suspend");
      return {};
    }
    std::suspend_always final_suspend() noexcept {
      ev("final_suspend");
      return {};
    }
    void return_void() { ev("return_void"); }
    void unhandled_exception() { ev("unhandled_exception"); }

    void *operator new(decltype(sizeof(0)) N) {
      step("alloc");
      return ::operator new(N);
    }
    void operator delete(void *P) {
      ev("dealloc");
      ::operator delete(P);
    }
  };

  std::coroutine_handle<promise_type> Handle;
  explicit Task(std::coroutine_handle<promise_type> H) : Handle(H) {
    ev("Task()");
  }
  Task(Task &&) = delete;
  ~Task() {
    ev("~Task()");
    if (Handle)
      Handle.destroy();
  }
  bool valid() const { return (bool)Handle; }
  void resume() { Handle.resume(); }
};

// A task whose promise type handles allocation failure.
struct NoThrowTask {
  struct promise_type {
    NoThrowTask get_return_object() {
      ev("get_return_object");
      return NoThrowTask(
          std::coroutine_handle<promise_type>::from_promise(*this));
    }
    static NoThrowTask get_return_object_on_allocation_failure() {
      ev("get_return_object_on_allocation_failure");
      return NoThrowTask(std::coroutine_handle<promise_type>());
    }
    std::suspend_always initial_suspend() { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}

    void *operator new(decltype(sizeof(0)) N) noexcept {
      ev("alloc");
      return FailAllocation ? nullptr : ::operator new(N, std::nothrow);
    }
    void operator delete(void *P) {
      ev("dealloc");
      ::operator delete(P);
    }
  };

  std::coroutine_handle<promise_type> Handle;
  explicit NoThrowTask(std::coroutine_handle<promise_type> H) : Handle(H) {
    ev("Task()");
  }
  NoThrowTask(NoThrowTask &&) = delete;
  ~NoThrowTask() {
    ev("~Task()");
    if (Handle)
      Handle.destroy();
  }
  bool valid() const { return (bool)Handle; }
};

using Lazy = Task<false>;
using Eager = Task<true>;

__attribute__((noinline)) Lazy lazy(Param P, const int &X)
    pre(check("pre") && P.Value == 1 && X == 1)
    post(R : check("post") && R.valid() && X == 1) {
  ev("body1");
  co_await std::suspend_always{};
  ev("body2");
}

__attribute__((noinline)) Eager eager(Param P, const int &X)
    pre(check("pre") && P.Value == 1 && X == 1)
    post(R : check("post") && R.valid() && X == 1) {
  ev("body1");
  co_await std::suspend_always{};
  ev("body2");
}

// A coroutine which does not suspend before running to its final suspend
// point.
__attribute__((noinline)) Eager eager_to_completion()
    pre(check("pre"))
    post(R : check("post") && R.valid()) {
  ev("body");
  co_return;
}

__attribute__((noinline)) NoThrowTask nothrow_alloc()
    pre(check("pre"))
    post(R : check("post") && R.valid() != FailAllocation) {
  ev("body");
  co_return;
}

// The type returned by get_return_object is converted to the return type of
// the function when it returns to its caller; the result binding names the
// result of the conversion.
struct Converted {
  struct promise_type;
  struct ReturnObject {
    std::coroutine_handle<promise_type> Handle;
  };
  struct promise_type {
    ReturnObject get_return_object() {
      ev("get_return_object");
      return {std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}
  };
  std::coroutine_handle<promise_type> Handle;
  bool IsConverted;
  Converted(ReturnObject O) : Handle(O.Handle), IsConverted(true) {
    ev("Converted()");
  }
  Converted(Converted &&) = delete;
  ~Converted() { Handle.destroy(); }
};

__attribute__((noinline)) Converted converted()
    pre(check("pre"))
    post(R : check("post") && R.IsConverted) {
  ev("body");
  co_await std::suspend_always{};
  ev("resumed");
}

// A coroutine without a result object, which runs to completion and destroys
// its coroutine state before returning to its caller. The postconditions are
// evaluated after that, so they must not depend on anything in the coroutine
// state, such as the copies of the parameters.
struct Detached {};
template <class... Args> struct std::coroutine_traits<void, Detached, Args...> {
  struct promise_type {
    void get_return_object() {}
    std::suspend_never initial_suspend() { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}

    void *operator new(decltype(sizeof(0)) N) { return ::operator new(N); }
    void operator delete(void *P, decltype(sizeof(0)) N) {
      // Make any use of the coroutine state after its destruction obvious.
      memset(P, 0xCD, N);
      ::operator delete(P);
    }
  };
};

__attribute__((noinline)) void detached(Detached, Param P, const int &X)
    pre(check("pre") && X == 1)
    post(check("post") && X == 1) {
  ev("body");
  co_return;
}

struct Member {
  int Value = 1;
  __attribute__((noinline)) Lazy coro(const int X)
      pre(check("pre") && Value == X)
      post(R : check("post") && R.valid() && Value == 1) {
    ev("body");
    co_return;
  }
};

void test_order() {
  int One = 1;
  {
    Lazy T = lazy(Param(), One);
    EXPECT_EVENTS("Param();" C("pre;")
                  "alloc;Param(move);promise();get_return_object;Task();"
                  "initial_suspend;await_ready;await_suspend;" C("post;")
                  "~Param();");
    // Resuming the coroutine does not evaluate the contracts again.
    T.resume();
    EXPECT_EVENTS("await_resume;body1;");
    T.resume();
    EXPECT_EVENTS("body2;return_void;final_suspend;");
  }
  EXPECT_EVENTS("~Task();~promise();~Param(copy);dealloc;");

  {
    Eager T = eager(Param(), One);
    EXPECT_EVENTS("Param();" C("pre;")
                  "alloc;Param(move);promise();get_return_object;Task();"
                  "initial_suspend;await_ready;await_resume;body1;" C("post;")
                  "~Param();");
    T.resume();
    EXPECT_EVENTS("body2;return_void;final_suspend;");
  }
  EXPECT_EVENTS("~Task();~promise();~Param(copy);dealloc;");

  { Eager T = eager_to_completion(); }
  EXPECT_EVENTS(C("pre;") "alloc;promise();get_return_object;Task();"
                "initial_suspend;await_ready;await_resume;body;return_void;"
                "final_suspend;" C("post;") "~Task();~promise();dealloc;");

  { auto T = nothrow_alloc(); }
  EXPECT_EVENTS(C("pre;") "alloc;get_return_object;Task();" C("post;")
                "~Task();dealloc;");
  // The object returned when the allocation fails is the result object.
  FailAllocation = true;
  { auto T = nothrow_alloc(); }
  FailAllocation = false;
  EXPECT_EVENTS(C("pre;") "alloc;get_return_object_on_allocation_failure;"
                "Task();" C("post;") "~Task();");

  {
    Converted T = converted();
    EXPECT_EVENTS(C("pre;") "get_return_object;body;Converted();" C("post;"));
    T.Handle.resume();
    EXPECT_EVENTS("resumed;");
  }

  detached(Detached(), Param(), One);
  EXPECT_EVENTS("Param();" C("pre;") "Param(move);body;~Param(copy);"
                C("post;") "~Param();");

  {
    Member M;
    Lazy T = M.coro(1);
    EXPECT_EVENTS(C("pre;") "alloc;promise();get_return_object;Task();"
                  "initial_suspend;await_ready;await_suspend;" C("post;"));
  }
  EXPECT_EVENTS("~Task();~promise();dealloc;");
}

#if __cpp_exceptions
// If the ramp exits via an exception, the postconditions are not evaluated.
void test_ramp_exceptions() {
  int One = 1;
  auto Run = [&](const char *At) {
    ThrowAt = At;
    try {
      Lazy T = lazy(Param(), One);
      ev("returned");
    } catch (int) {
      ev("caught");
    }
    ThrowAt = "";
  };
  Run("alloc");
  EXPECT_EVENTS("Param();" C("pre;") "alloc;~Param();caught;");
  Run("promise()");
  EXPECT_EVENTS("Param();" C("pre;") "alloc;Param(move);promise();"
                "~Param(copy);dealloc;~Param();caught;");
  Run("get_return_object");
  EXPECT_EVENTS("Param();" C("pre;") "alloc;Param(move);promise();"
                "get_return_object;~promise();~Param(copy);dealloc;~Param();"
                "caught;");
}

#ifndef NO_HANDLER
template <class F> void expect_throw(F Fn) {
  ThrowFromHandler = true;
  try {
    Fn();
    ev("returned");
  } catch (int) {
    ev("caught");
  }
  ThrowFromHandler = false;
  Fail = "";
}

void test_handler_exceptions() {
  static int One = 1;
  // A precondition: the coroutine has not been set up at all.
  Fail = "pre";
  expect_throw([] { Lazy T = lazy(Param(), One); });
  EXPECT_EVENTS("Param();pre;violation;~Param();caught;");

  // A postcondition: the result object is destroyed, which in this case
  // destroys the suspended coroutine.
  Fail = "post";
  expect_throw([] { Lazy T = lazy(Param(), One); });
  EXPECT_EVENTS("Param();pre;alloc;Param(move);promise();get_return_object;"
                "Task();initial_suspend;await_ready;await_suspend;post;"
                "violation;~Task();~promise();~Param(copy);dealloc;~Param();"
                "caught;");
  Fail = "post";
  expect_throw([] { Eager T = eager(Param(), One); });
  EXPECT_EVENTS("Param();pre;alloc;Param(move);promise();get_return_object;"
                "Task();initial_suspend;await_ready;await_resume;body1;post;"
                "violation;~Task();~promise();~Param(copy);dealloc;~Param();"
                "caught;");
  Fail = "post";
  expect_throw([] { detached(Detached(), Param(), One); });
  EXPECT_EVENTS("Param();pre;Param(move);body;~Param(copy);post;violation;"
                "~Param();caught;");
}
#else
void test_handler_exceptions() {}
#endif
#else
void test_ramp_exceptions() {}
void test_handler_exceptions() {}
#endif

int main() {
  test_order();
  test_ramp_exceptions();
  test_handler_exceptions();
  return 0;
}
