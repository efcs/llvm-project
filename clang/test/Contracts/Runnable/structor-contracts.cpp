// Check when the function contract assertions of constructors and destructors
// are evaluated relative to the initialization and destruction of the
// subobjects, that they are evaluated exactly once per call, and what happens
// when the contract-violation handler throws.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.observe
// RUN: %t.observe
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.observe.O2
// RUN: %t.observe.O2
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 -Xclang -mno-constructor-aliases %s -o %t.noalias
// RUN: %t.noalias
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce -O1 %s -o %t.enforce
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
void handle_contract_violation(const contract_violation &) {
  ev("violation");
#if __cpp_exceptions
  if (ThrowFromHandler)
    throw 42;
#endif
}

// Predicates which pass or fail depending on Fail.
const char *Fail = "";
__attribute__((noinline)) bool check(const char *Event) {
  ev(Event);
  return strcmp(Fail, Event) != 0;
}

struct VBase {
  VBase() { ev("VBase()"); }
  ~VBase() { ev("~VBase()"); }
};
struct Base {
  Base() { ev("Base()"); }
  ~Base() { ev("~Base()"); }
};
struct Member {
  const char *Name;
  char Buf[32];
  Member(const char *Name = "M") : Name(Name) {
    ev(strcat(strcat(strcpy(Buf, Name), "("), ")"));
  }
  ~Member() { ev(strcat(strcat(strcpy(Buf, "~"), Name), "()")); }
};

//===----------------------------------------------------------------------===//
// Order of evaluation.
//===----------------------------------------------------------------------===//

struct A : Base, virtual VBase {
  Member M;
  A() pre(check("A.pre")) post(check("A.post")) { ev("A.body"); }
  A(int) pre(check("A(int).pre")) post(check("A(int).post")) : A() {
    ev("A(int).body");
  }
  ~A() noexcept(false) pre(check("~A.pre")) post(check("~A.post")) {
    ev("~A.body");
  }
};

struct B : A {
  Member M2{"M2"};
  B() pre(check("B.pre")) post(check("B.post")) { ev("B.body"); }
  ~B() noexcept(false) pre(check("~B.pre")) post(check("~B.post")) {
    ev("~B.body");
  }
};

#if __cpp_exceptions
// The function contract assertions are not part of the function-try-block.
struct T : Base, virtual VBase {
  Member M;
  T() pre(check("T.pre")) post(check("T.post")) try : M("M") {
    ev("T.body");
  } catch (...) {
    ev("T.handler");
  }
  ~T() noexcept(false) pre(check("~T.pre")) post(check("~T.post")) try {
    ev("~T.body");
  } catch (...) {
    ev("~T.handler");
  }
};
#endif

// A destructor with an empty body, a single base class and no members could be
// emitted as an alias of the base class destructor, were it not for the
// contracts.
struct EmptyBody : Base {
  ~EmptyBody() pre(check("~EmptyBody.pre")) post(check("~EmptyBody.post")) {}
};

// [class.cdtor]p4: During the evaluation of a postcondition of a constructor
// or a precondition of a destructor, virtual functions resolve to the final
// overrider in the class of the constructor or destructor.
struct Poly1 {
  virtual const char *name() const { return "Poly1"; }
  __attribute__((noinline)) bool who() const { return ev(name()); }
  Poly1() post(who()) {}
  ~Poly1() pre(who()) {}
};
struct Poly2 : Poly1 {
  const char *name() const override { return "Poly2"; }
  Poly2() post(who()) {}
  ~Poly2() pre(who()) {}
};
struct Poly3 : Poly2 {
  const char *name() const override { return "Poly3"; }
};

// The same applies to the contracts of the destructors of the members.
struct Outer1;
struct Field {
  const Outer1 *O;
  ~Field() pre(who()) {}
  bool who() const;
};
struct OuterBase {
  virtual const char *name() const { return "OuterBase"; }
};
struct Outer1 : OuterBase {
  Field F{this};
  const char *name() const override { return "Outer1"; }
  ~Outer1() {}
};
struct Outer2 : Outer1 {
  const char *name() const override { return "Outer2"; }
  ~Outer2() {}
};
__attribute__((noinline)) bool Field::who() const { return ev(O->name()); }

__attribute__((noinline)) void destroy(A *P) { delete P; }

void test_order() {
  // A complete object: the precondition of the constructor is evaluated
  // before any subobject is initialized; the postcondition of the destructor
  // is evaluated after all subobjects, including the virtual base, have been
  // destroyed.
  { A Obj; }
  EXPECT_EVENTS(C("A.pre;") "VBase();Base();M();A.body;" C("A.post;")
                C("~A.pre;") "~A.body;~M();~Base();~VBase();" C("~A.post;"));

  // A base class subobject: the virtual base is destroyed by the destructor
  // of the derived class, after the contracts of A have been evaluated.
  { B Obj; }
  EXPECT_EVENTS(C("B.pre;") "VBase();" C("A.pre;") "Base();M();A.body;"
                C("A.post;") "M2();B.body;" C("B.post;")
                C("~B.pre;") "~B.body;~M2();" C("~A.pre;")
                "~A.body;~M();~Base();" C("~A.post;") "~VBase();"
                C("~B.post;"));

  // Delete expressions and arrays destroy complete objects.
  destroy(new A);
  EXPECT_EVENTS(C("A.pre;") "VBase();Base();M();A.body;" C("A.post;")
                C("~A.pre;") "~A.body;~M();~Base();~VBase();" C("~A.post;"));
  { A Arr[2]; }
  EXPECT_EVENTS(C("A.pre;") "VBase();Base();M();A.body;" C("A.post;")
                C("A.pre;") "VBase();Base();M();A.body;" C("A.post;")
                C("~A.pre;") "~A.body;~M();~Base();~VBase();" C("~A.post;")
                C("~A.pre;") "~A.body;~M();~Base();~VBase();" C("~A.post;"));

  // A delegating constructor: both constructors evaluate their contracts.
  {
    A Obj(1);
    EXPECT_EVENTS(C("A(int).pre;") C("A.pre;") "VBase();Base();M();A.body;"
                  C("A.post;") "A(int).body;" C("A(int).post;"));
  }
  EXPECT_EVENTS(C("~A.pre;") "~A.body;~M();~Base();~VBase();" C("~A.post;"));

#if __cpp_exceptions
  { T Obj; }
  EXPECT_EVENTS(C("T.pre;") "VBase();Base();M();T.body;" C("T.post;")
                C("~T.pre;") "~T.body;~M();~Base();~VBase();" C("~T.post;"));
#endif

  { EmptyBody Obj; }
  EXPECT_EVENTS("Base();" C("~EmptyBody.pre;") "~Base();"
                C("~EmptyBody.post;"));

  { Poly3 Obj; }
  EXPECT_EVENTS(C("Poly1;Poly2;Poly2;Poly1;"));

  { Outer2 Obj; }
  EXPECT_EVENTS(C("Outer1;"));
}

//===----------------------------------------------------------------------===//
// Exceptions thrown by the contract-violation handler.
//===----------------------------------------------------------------------===//

#if __cpp_exceptions && !defined(NO_HANDLER)
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

struct U {
  Member M;
  U() : M("UM") {}
  ~U() {}
};
union Un {
  Member M;
  int I;
  Un() post(check("Un.post")) : M("UnM") {}
  ~Un() {}
};

void test_exceptions() {
  // A precondition of a constructor: nothing has been initialized.
  Fail = "A.pre";
  expect_throw([] { A Obj; });
  EXPECT_EVENTS("A.pre;violation;caught;");

  // A postcondition of a constructor: all the subobjects are destroyed, but
  // the destructor is not called.
  Fail = "A.post";
  expect_throw([] { A Obj; });
  EXPECT_EVENTS("A.pre;VBase();Base();M();A.body;A.post;violation;"
                "~M();~Base();~VBase();caught;");

  // For a base class subobject, the virtual base is destroyed by the
  // constructor of the derived class.
  Fail = "A.post";
  expect_throw([] { B Obj; });
  EXPECT_EVENTS("B.pre;VBase();A.pre;Base();M();A.body;A.post;violation;"
                "~M();~Base();~VBase();caught;");

  // The base class subobject is destroyed with its destructor.
  Fail = "B.post";
  expect_throw([] { B Obj; });
  EXPECT_EVENTS("B.pre;VBase();A.pre;Base();M();A.body;A.post;M2();B.body;"
                "B.post;violation;~M2();~A.pre;~A.body;~M();~Base();~A.post;"
                "~VBase();caught;");

  // A delegating constructor: the object is complete once the target
  // constructor returns, so the destructor is called.
  Fail = "A(int).post";
  expect_throw([] { A Obj(1); });
  EXPECT_EVENTS("A(int).pre;A.pre;VBase();Base();M();A.body;A.post;"
                "A(int).body;A(int).post;violation;"
                "~A.pre;~A.body;~M();~Base();~VBase();~A.post;caught;");
  Fail = "A(int).pre";
  expect_throw([] { A Obj(1); });
  EXPECT_EVENTS("A(int).pre;violation;caught;");

  // A precondition of a destructor: the subobjects are still destroyed.
  Fail = "~A.pre";
  expect_throw([] { A Obj; });
  EXPECT_EVENTS("A.pre;VBase();Base();M();A.body;A.post;~A.pre;violation;"
                "~M();~Base();~VBase();caught;");
  Fail = "~A.pre";
  expect_throw([] { B Obj; });
  EXPECT_EVENTS("B.pre;VBase();A.pre;Base();M();A.body;A.post;M2();B.body;"
                "B.post;~B.pre;~B.body;~M2();~A.pre;violation;"
                "~M();~Base();~VBase();caught;");

  // A postcondition of a destructor: everything has been destroyed.
  Fail = "~A.post";
  expect_throw([] { A Obj; });
  EXPECT_EVENTS("A.pre;VBase();Base();M();A.body;A.post;~A.pre;~A.body;"
                "~M();~Base();~VBase();~A.post;violation;caught;");
  Fail = "~A.post";
  expect_throw([] { B Obj; });
  EXPECT_EVENTS("B.pre;VBase();A.pre;Base();M();A.body;A.post;M2();B.body;"
                "B.post;~B.pre;~B.body;~M2();~A.pre;~A.body;~M();~Base();"
                "~A.post;violation;~VBase();caught;");

  // The handlers of a function-try-block are not entered.
  Fail = "T.pre";
  expect_throw([] { T Obj; });
  EXPECT_EVENTS("T.pre;violation;caught;");
  Fail = "T.post";
  expect_throw([] { T Obj; });
  EXPECT_EVENTS("T.pre;VBase();Base();M();T.body;T.post;violation;"
                "~M();~Base();~VBase();caught;");
  Fail = "~T.pre";
  expect_throw([] { T Obj; });
  EXPECT_EVENTS("T.pre;VBase();Base();M();T.body;T.post;~T.pre;violation;"
                "~M();~Base();~VBase();caught;");
  Fail = "~T.post";
  expect_throw([] { T Obj; });
  EXPECT_EVENTS("T.pre;VBase();Base();M();T.body;T.post;~T.pre;~T.body;"
                "~M();~Base();~VBase();~T.post;violation;caught;");

  // The member of a union initialized by the constructor is destroyed.
  Fail = "Un.post";
  expect_throw([] { Un Obj; });
  EXPECT_EVENTS("UnM();Un.post;violation;~UnM();caught;");
}
#else
void test_exceptions() {}
#endif

int main() {
  test_order();
  test_exceptions();
  return 0;
}
