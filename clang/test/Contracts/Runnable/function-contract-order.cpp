// Check when function contract assertions are evaluated relative to the
// initialization and destruction of parameters, local variables, temporaries
// and the result object ([expr.call]p7, [expr.call]p10, [stmt.return]p5), and
// how they interact with a function-try-block ([basic.contract.eval]p17).
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

#include "contracts-runtime.h"
#include "event-log.h"

using namespace std::contracts;

bool ThrowFromHandler = false;
void handle_contract_violation(const contract_violation &V) {
  ev(V.detection_mode() == detection_mode::evaluation_exception
         ? "violation(exception)"
         : "violation");
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

// An object which records its construction and destruction.
struct Obj {
  const char *Name;
  char Buf[32];
  explicit Obj(const char *Name) : Name(Name) {
    ev(strcat(strcat(strcpy(Buf, Name), "("), ")"));
  }
  Obj(const Obj &O) : Name(O.Name) {
    ev(strcat(strcat(strcpy(Buf, Name), "("), "copy)"));
  }
  ~Obj() { ev(strcat(strcat(strcpy(Buf, "~"), Name), "()")); }
};

// A parameter of this type is destroyed by the callee.
struct [[clang::trivial_abi]] Param {
  Param() { ev("Param()"); }
  Param(const Param &) { ev("Param(copy)"); }
  ~Param() { ev("~Param()"); }
};

//===----------------------------------------------------------------------===//
// Order of evaluation.
//===----------------------------------------------------------------------===//

__attribute__((noinline)) void locals(Param P)
    pre(check("pre"))
    post(check("post")) {
  Obj L("L");
  ev("body");
}

__attribute__((noinline)) void early_return(Param P, int X)
    post(check("post")) {
  Obj L1("L1");
  if (X) {
    Obj L2("L2");
    if (X > 1)
      return;
    ev("X==1");
  }
  Obj L3("L3");
}

__attribute__((noinline)) Obj result(Param P)
    pre(check("pre"))
    post(R : check("post") && strcmp(R.Name, "Result") == 0) {
  Obj L("L");
  return Obj("Result");
}

__attribute__((noinline)) Obj nrvo(Param P, bool B)
    post(R : check("post") && strcmp(R.Name, "Result") == 0) {
  Obj L("L");
  Obj Result("Result");
  if (B)
    return Result;
  ev("!B");
  return Result;
}

__attribute__((noinline)) int temporaries(Param P)
    post(R : check("post") && R == 6) {
  return (int)strlen(Obj("Temp").Name) + 2;
}

__attribute__((noinline)) void multiple(Param P)
    pre(check("pre1")) pre(check("pre2"))
    post(check("post1")) post(check("post2")) {
  ev("body");
}

void test_order() {
  locals(Param());
  EXPECT_EVENTS("Param();" C("pre;") "L();body;~L();" C("post;") "~Param();");

  early_return(Param(), 0);
  EXPECT_EVENTS("Param();L1();L3();~L3();~L1();" C("post;") "~Param();");
  early_return(Param(), 1);
  EXPECT_EVENTS("Param();L1();L2();X==1;~L2();L3();~L3();~L1();" C("post;")
                "~Param();");
  early_return(Param(), 2);
  EXPECT_EVENTS("Param();L1();L2();~L2();~L1();" C("post;") "~Param();");

  { Obj R = result(Param()); }
  EXPECT_EVENTS("Param();" C("pre;") "L();Result();~L();" C("post;")
                "~Param();~Result();");

  { Obj R = nrvo(Param(), true); }
  EXPECT_EVENTS("Param();L();Result();~L();" C("post;") "~Param();~Result();");
  { Obj R = nrvo(Param(), false); }
  EXPECT_EVENTS("Param();L();Result();!B;~L();" C("post;")
                "~Param();~Result();");

  temporaries(Param());
  EXPECT_EVENTS("Param();Temp();~Temp();" C("post;") "~Param();");

  multiple(Param());
  EXPECT_EVENTS("Param();" C("pre1;") C("pre2;") "body;" C("post1;")
                C("post2;") "~Param();");
}

//===----------------------------------------------------------------------===//
// Exceptions.
//===----------------------------------------------------------------------===//

#if __cpp_exceptions
// Postconditions are not evaluated when the function exits via an exception.
__attribute__((noinline)) void throws(Param P) post(check("post")) {
  Obj L("L");
  throw 42;
}

// The handlers of a function-try-block can neither handle an exception thrown
// by the contract-violation handler, nor an exception thrown by a predicate.
__attribute__((noinline)) int try_block(Param P, const int X)
    pre(check("pre"))
    post(R : check("post") && R == X)
try {
  Obj L("L");
  if (X == 2)
    throw 1.0;
  return X;
} catch (double) {
  ev("handler");
  return X;
} catch (...) {
  ev("wrong handler");
  return -1;
}

__attribute__((noinline)) bool throwing_predicate(const char *Event) {
  ev(Event);
  throw 1.0;
}
__attribute__((noinline)) int try_block_predicate(Param P)
    pre(throwing_predicate("pre"))
    post(throwing_predicate("post"))
try {
  ev("body");
  return 0;
} catch (...) {
  ev("wrong handler");
  return -1;
}

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

void test_exceptions() {
  try {
    throws(Param());
  } catch (int) {
    ev("caught");
  }
  EXPECT_EVENTS("Param();L();~L();~Param();caught;");

  // The try block returns normally, or its handler does.
  try_block(Param(), 1);
  EXPECT_EVENTS("Param();" C("pre;") "L();~L();" C("post;") "~Param();");
  try_block(Param(), 2);
  EXPECT_EVENTS("Param();" C("pre;") "L();~L();handler;" C("post;")
                "~Param();");

#ifndef NO_HANDLER
#ifndef ENFORCE
  // A violated assertion whose handler returns.
  Fail = "pre";
  try_block(Param(), 1);
  Fail = "";
  EXPECT_EVENTS("Param();pre;violation;L();~L();post;~Param();");

  // An exception thrown by a predicate is a contract violation, and is not
  // seen by the handlers of the function-try-block.
  try_block_predicate(Param());
  EXPECT_EVENTS("Param();pre;violation(exception);body;post;"
                "violation(exception);~Param();");
#endif

  // An exception thrown by the contract-violation handler propagates out of
  // the function as if the function body had exited via the exception.
  Fail = "pre";
  expect_throw([] { locals(Param()); });
  EXPECT_EVENTS("Param();pre;violation;~Param();caught;");
  Fail = "post";
  expect_throw([] { locals(Param()); });
  EXPECT_EVENTS("Param();pre;L();body;~L();post;violation;~Param();caught;");

  // The result object has been initialized and is destroyed.
  Fail = "post";
  expect_throw([] { Obj R = result(Param()); });
  EXPECT_EVENTS("Param();pre;L();Result();~L();post;violation;~Result();"
                "~Param();caught;");
  Fail = "post";
  expect_throw([] { Obj R = nrvo(Param(), false); });
  EXPECT_EVENTS("Param();L();Result();!B;~L();post;violation;~Result();"
                "~Param();caught;");

  Fail = "pre";
  expect_throw([] { try_block(Param(), 1); });
  EXPECT_EVENTS("Param();pre;violation;~Param();caught;");
  Fail = "post";
  expect_throw([] { try_block(Param(), 1); });
  EXPECT_EVENTS("Param();pre;L();~L();post;violation;~Param();caught;");
  Fail = "post";
  expect_throw([] { try_block(Param(), 2); });
  EXPECT_EVENTS("Param();pre;L();~L();handler;post;violation;~Param();"
                "caught;");
  expect_throw([] { try_block_predicate(Param()); });
  EXPECT_EVENTS("Param();pre;violation(exception);~Param();caught;");
#endif
}
#else
void test_exceptions() {}
#endif

int main() {
  test_order();
  test_exceptions();
  return 0;
}
