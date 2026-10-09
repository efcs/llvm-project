// [over.call.func]/p3.1 and [expr.prim.id.general]/p2: an unqualified reference
// to a non-static member (which forms an implied '(*this).' access) in a
// constructor precondition or a destructor postcondition is ill-formed: no
// object is within its lifetime at that point. Qualified access (this->m),
// constructor postconditions, destructor preconditions and ordinary member
// functions are unaffected. Based on the standard's own example:
//
//   struct C {
//     bool b;
//     C() pre(b)              // error
//         pre(&this->b)       // OK
//         pre(sizeof(b) > 0); // OK, b is not potentially evaluated
//   };
//
// (GCC mirror: g++.dg/contracts/cpp26/over.call.func.p3.1.C.)
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

struct S {
  bool f();
  bool fc() const;
  bool k;
  static bool sk;
  static bool sf();

  S() pre(k); // expected-error {{implicit access to non-static member 'k' of the object under construction is not allowed in the precondition of a constructor}}
  S(int) pre(this->k);     // OK: qualified access is not an unqualified member reference
  S(long) post(k);         // OK: constructor postcondition -- object is alive
  ~S() post(fc());         // expected-error {{implicit access to non-static member 'fc' of the object under destruction is not allowed in the postcondition of a destructor}}
  void g() pre(k) post(k); // OK: ordinary member function

  // The member need not be potentially evaluated, and static members and
  // explicit object expressions are fine.
  S(char) pre(this != nullptr) // expected-warning {{'this' pointer cannot be null in well-defined C++ code; comparison may be assumed to always evaluate to true}}
          pre(sizeof(this->k) > 0)
          pre(sizeof(k) > 0)
          pre(&this->k != nullptr) // expected-warning {{comparison of address of 'this->k' not equal to a null pointer is always true}}
          pre(sk) pre(sf()) pre(S::sk) pre((*this).k);
  // A qualified name still forms an implicit member access.
  S(short) pre(S::k); // expected-error {{implicit access to non-static member 'k' of the object under construction is not allowed in the precondition of a constructor}}
  // A member function call forms one too ([over.call.func]/3).
  S(unsigned) pre(fc()); // expected-error {{implicit access to non-static member 'fc' of the object under construction is not allowed in the precondition of a constructor}}
  // Including in the body of a lambda-expression within the predicate.
  S(float) pre([this] { return k; }()); // expected-error {{implicit access to non-static member 'k' of the object under construction is not allowed in the precondition of a constructor}}
  S(double) pre([*this] { return k; }()); // expected-error {{implicit access to non-static member 'k' of the object under construction is not allowed in the precondition of a constructor}}
  S(bool) pre([this] { return this->k; }()); // OK
  // But not of a class other than a closure type nested within the predicate.
  S(const char *) pre([] { struct L { bool m; bool f() { return m; } }; return L{true}.f(); }()); // OK
};

// A destructor precondition and a constructor postcondition are fine (object
// within its lifetime); only ctor-pre / dtor-post are restricted.
struct T {
  bool f() const; // const so the constified 'this' in a contract can call it
  bool k;
  ~T() pre(f());   // OK: destructor precondition
  T() post(k);     // OK: constructor postcondition
};

template <class X> struct TS {
  X k;
  bool f() const;
  TS() pre(k); // expected-error {{implicit access to non-static member 'k' of the object under construction is not allowed in the precondition of a constructor}}
  TS(int) pre(this->k);
  ~TS() post(f()); // expected-error {{implicit access to non-static member 'f' of the object under destruction is not allowed in the postcondition of a destructor}}
};
