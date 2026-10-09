// A contract on a coroutine names the coroutine's own parameters, not the
// copies the body uses.
//
// [dcl.fct.def.coroutine]/5: "a copy is created for each coroutine parameter
// AT THE BEGINNING OF THE REPLACEMENT BODY".  A contract-specifier is part of
// the declaration and is evaluated by the ramp, so it names the parameters
// themselves.  The copies are the body's.
//
// Clang named the copies from the postcondition while naming the parameters
// from the precondition -- one name meaning two objects on one declaration.
// The cause was upstream and not contracts-specific:
// ParamReferenceReplacerRAII redirects each parameter to its frame copy in
// LocalDeclMap for the body, and restored them with DenseMap::insert, which
// does nothing when the key is already present.  addCopy overwrites the value
// in place and never erases the key, so the restore was a no-op and every
// parameter stayed pointing at its frame copy for the rest of the function.
// Nothing upstream reads a parameter after the body; a postcondition, emitted
// from the epilogue, does.
//
// This is not merely the wrong object.  A coroutine whose final suspend does
// not suspend destroys its own frame during the call, so by the time the ramp
// evaluates the postcondition the frame copies are GONE -- reading one is a
// use-after-free.  reference_survives_frame below is that case.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t && %t

#include <contracts>
#include <coroutine>
#include <cstdio>

void handle_contract_violation(const std::contracts::contract_violation &) {}

static int failures = 0;

// Suspends initially: the body does not run until resumed.
struct Lazy {
  struct promise_type {
    Lazy get_return_object() {
      return Lazy{std::coroutine_handle<promise_type>::from_promise(*this)};
    }
    std::suspend_always initial_suspend() noexcept { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}
  };
  std::coroutine_handle<promise_type> h;
  ~Lazy() {
    if (h)
      h.destroy();
  }
  void resume() {
    if (h && !h.done())
      h.resume();
  }
};

// Runs eagerly AND destroys its own frame at final suspend, so the frame is
// already gone when the ramp evaluates its postcondition.
struct Fire {
  struct promise_type {
    Fire get_return_object() { return {}; }
    std::suspend_never initial_suspend() noexcept { return {}; }
    std::suspend_never final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}
  };
};

static const void *pre_addr, *post_addr, *body_addr;
static int pre_val, post_val;

static bool notePre(const int &v) {
  pre_addr = &v;
  pre_val = v;
  return true;
}
static bool notePost(const int &v) {
  post_addr = &v;
  post_val = v;
  return true;
}

// A precondition may name a by-value parameter (the const rule is a
// postcondition rule).  It must name the ramp's parameter: the body's copy
// does not exist yet when the precondition runs.
static Lazy pre_by_value(int x) pre(notePre(x)) {
  body_addr = &x;
  co_return;
}

// A postcondition may name a REFERENCE parameter.  [dcl.fct.def.coroutine]/5
// makes the frame's copy of a reference "bound to the same object", so the
// value is the same either way -- but the frame's copy dies with the frame,
// and the ramp's parameter does not.
static Fire reference_survives_frame(const int &x) post(notePost(x)) {
  body_addr = &x;
  co_return;
}

// Both contracts on one declaration must name the same entity.
static const void *b_pre, *b_post;
static bool nA(const int &v) {
  b_pre = &v;
  return true;
}
static bool nB(const int &v) {
  b_post = &v;
  return true;
}
static Lazy both(const int &x) pre(nA(x)) post(nB(x)) { co_return; }

static void check(const char *what, bool ok) {
  if (!ok) {
    std::printf("FAIL: %s\n", what);
    ++failures;
  }
}

int main() {
  {
    pre_addr = nullptr;
    body_addr = nullptr;
    pre_val = -1;
    Lazy c = pre_by_value(7);
    check("precondition sees the argument's value", pre_val == 7);
    c.resume();
    check("precondition named the parameter, not the frame copy",
          pre_addr != body_addr);
  }

  {
    int arg = 9;
    post_addr = nullptr;
    body_addr = nullptr;
    post_val = -1;
    reference_survives_frame(arg);
    // The frame is destroyed before the ramp's postcondition runs, so naming
    // the frame's copy of the reference would be a use-after-free.
    check("postcondition sees the referred-to object's value", post_val == 9);
    check("postcondition did not read the destroyed frame",
          post_addr == &arg);
  }

  {
    int arg = 1;
    b_pre = b_post = nullptr;
    Lazy c = both(arg);
    check("pre and post name the same entity", b_pre == b_post);
    check("and it is the referred-to object", b_pre == &arg);
  }

  if (failures)
    __builtin_abort();
  return 0;
}
