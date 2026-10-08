// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,enforce %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=observe -verify=expected,observe %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify=expected,enforce %s -fexperimental-new-constant-interpreter

// The function contract assertions of constructors, destructors, assignment
// operators and lambdas are evaluated during constant evaluation.

namespace constructor {
struct S {
  int v;
  constexpr S(int x) pre(x > 0) post(v == 1 || v == 2) : v(x) {} // #S
};

constexpr S ok(1);

constexpr S pre_fails(0);
// observe-warning@#S {{precondition 'x > 0' evaluated to false}}
// observe-warning@#S {{postcondition 'v == 1 || v == 2' evaluated to false}}
// observe-note@-3 2 {{in call to 'S(0)'}}
// enforce-error@-4 {{constexpr variable 'pre_fails' must be initialized by a constant expression}}
// enforce-note@#S {{contract violation with the 'enforce' evaluation semantic: precondition 'x > 0' evaluated to false}}
// enforce-note@-6 {{in call to 'S(0)'}}

constexpr S post_fails(3);
// observe-warning@#S {{postcondition 'v == 1 || v == 2' evaluated to false}}
// observe-note@-2 {{in call to 'S(3)'}}
// enforce-error@-3 {{constexpr variable 'post_fails' must be initialized by a constant expression}}
// enforce-note@#S {{contract violation with the 'enforce' evaluation semantic: postcondition 'v == 1 || v == 2' evaluated to false}}
// enforce-note@-5 {{in call to 'S(3)'}}

// The precondition assertions are evaluated before the mem-initializers, so
// they can't read the members.
struct ReadsMember {
  int v;
  constexpr ReadsMember(int x) pre(this->v == 0) : v(x) {} // #ReadsMember
};
constexpr ReadsMember reads_member(0);
// observe-warning@#ReadsMember {{contract violation with the 'observe' evaluation semantic: precondition 'this->v == 0' is not a constant expression}}
// observe-note@#ReadsMember {{read of object outside its lifetime is not allowed in a constant expression}}
// observe-note@-3 {{in call to 'ReadsMember(0)'}}
// observe-note@-4 {{declared here}}
// enforce-error@-5 {{constexpr variable 'reads_member' must be initialized by a constant expression}}
// enforce-note@#ReadsMember {{contract violation with the 'enforce' evaluation semantic: precondition 'this->v == 0' is not a constant expression}}
// enforce-note@#ReadsMember {{read of object outside its lifetime is not allowed in a constant expression}}
// enforce-note@-8 {{in call to 'ReadsMember(0)'}}
// enforce-note@-9 {{declared here}}

// ... but the postcondition assertions are evaluated after the body.
struct Body {
  int v;
  constexpr Body(const int x) post(v == x + 1) : v(x) { ++v; }
};
constexpr Body body(1);
static_assert(body.v == 2);

// Delegating and inheriting constructors.
struct Delegating {
  int v;
  constexpr Delegating() pre(true) post(v == 2) : Delegating(2) {}
  constexpr Delegating(const int x) pre(x != 3) post(v == x) : v(x) {} // #Delegating
  constexpr Delegating(int, int) : Delegating(3) {}              // #Delegating2
};
constexpr Delegating delegating;
static_assert(delegating.v == 2);
constexpr Delegating delegating_fails(0, 0);
// observe-warning@#Delegating {{precondition 'x != 3' evaluated to false}}
// observe-note@#Delegating2 {{in call to 'Delegating(3)'}}
// observe-note@-3 {{in call to 'Delegating(0, 0)'}}
// enforce-error@-4 {{constexpr variable 'delegating_fails' must be initialized by a constant expression}}
// enforce-note@#Delegating {{precondition 'x != 3' evaluated to false}}
// enforce-note@#Delegating2 {{in call to 'Delegating(3)'}}
// enforce-note@-7 {{in call to 'Delegating(0, 0)'}}

struct Base {
  int b;
  constexpr Base(int x) pre(x > 0) : b(x) {} // #Base
};
struct Inheriting : Base {
  using Base::Base; // #Inheriting
  int d = 7;
};
constexpr Inheriting inheriting(1);
static_assert(inheriting.b == 1 && inheriting.d == 7);
constexpr Inheriting inheriting_fails(0);
// observe-warning@#Base {{precondition 'x > 0' evaluated to false}}
// observe-note@#Inheriting {{in call to 'Base(0)'}}
// observe-note@-3 {{in implicit initialization for inherited constructor of 'Inheriting'}}
// enforce-error@-4 {{constexpr variable 'inheriting_fails' must be initialized by a constant expression}}
// enforce-note@#Base {{precondition 'x > 0' evaluated to false}}
// enforce-note@#Inheriting {{in call to 'Base(0)'}}
// enforce-note@-7 {{in implicit initialization for inherited constructor of 'Inheriting'}}
} // namespace constructor

namespace destructor {
struct D {
  int v;
  constexpr D(int v) : v(v) {}
  constexpr ~D() pre(v != 1) post(true) {} // #D
};
constexpr int f(int x) {
  D d(x); // #D-var
  return x;
}
static_assert(f(0) == 0);
constexpr int fails = f(1);
// observe-warning@#D {{precondition 'v != 1' evaluated to false}}
// observe-note@#D-var {{in call to 'd.~D()'}}
// observe-note@-3 {{in call to 'f(1)'}}
// enforce-error@-4 {{constexpr variable 'fails' must be initialized by a constant expression}}
// enforce-note@#D {{precondition 'v != 1' evaluated to false}}
// enforce-note@#D-var {{in call to 'd.~D()'}}
// enforce-note@-7 {{in call to 'f(1)'}}

// The postcondition assertions of a destructor are evaluated after the
// members and base classes have been destroyed (like at run time), so they
// can't read the members.
struct Late {
  int v;
  constexpr Late(int v) : v(v) {}
  constexpr ~Late() pre(this->v == 1) post(this->v == 1) {} // #Late
};
constexpr int g() {
  Late l(1); // #Late-var
  return 0;
}
constexpr int late = g();
// observe-warning@#Late {{contract violation with the 'observe' evaluation semantic: postcondition 'this->v == 1' is not a constant expression}}
// observe-note@#Late {{the postcondition assertions of a destructor are evaluated after the members and base classes of the 'Late' object have been destroyed}}
// observe-note@#Late {{read of object outside its lifetime is not allowed in a constant expression}}
// observe-note@#Late-var {{in call to 'l.~Late()'}}
// observe-note@-5 {{in call to 'g()'}}
// observe-note@#Late-var {{declared here}}
// enforce-error@-7 {{constexpr variable 'late' must be initialized by a constant expression}}
// enforce-note@#Late {{contract violation with the 'enforce' evaluation semantic: postcondition 'this->v == 1' is not a constant expression}}
// enforce-note@#Late {{the postcondition assertions of a destructor are evaluated after the members and base classes of the 'Late' object have been destroyed}}
// enforce-note@#Late {{read of object outside its lifetime is not allowed in a constant expression}}
// enforce-note@#Late-var {{in call to 'l.~Late()'}}
// enforce-note@-12 {{in call to 'g()'}}
// enforce-note@#Late-var {{declared here}}

// A base class is destroyed too, and a postcondition assertion that reads
// one is diagnosed the same way.
struct LateBase { int b = 1; };
struct LateDerived : LateBase {
  constexpr ~LateDerived() post(this->b == 1) {} // #LateDerived
};
constexpr int gb() {
  LateDerived d; // #LateDerived-var
  return 0;
}
constexpr int late_base = gb();
// observe-warning@#LateDerived {{postcondition 'this->b == 1' is not a constant expression}}
// observe-note@#LateDerived {{the postcondition assertions of a destructor are evaluated after the members and base classes of the 'LateDerived' object have been destroyed}}
// observe-note@#LateDerived {{read of object outside its lifetime is not allowed in a constant expression}}
// observe-note@#LateDerived-var {{in call to 'd.~LateDerived()'}}
// observe-note@-5 {{in call to 'gb()'}}
// observe-note@#LateDerived-var {{declared here}}
// enforce-error@-7 {{constexpr variable 'late_base' must be initialized by a constant expression}}
// enforce-note@#LateDerived {{postcondition 'this->b == 1' is not a constant expression}}
// enforce-note@#LateDerived {{the postcondition assertions of a destructor are evaluated after the members and base classes of the 'LateDerived' object have been destroyed}}
// enforce-note@#LateDerived {{read of object outside its lifetime is not allowed in a constant expression}}
// enforce-note@#LateDerived-var {{in call to 'd.~LateDerived()'}}
// enforce-note@-12 {{in call to 'gb()'}}
// enforce-note@#LateDerived-var {{declared here}}

// A postcondition assertion that doesn't read the object is fine.
struct LateOk {
  int v;
  constexpr LateOk(int v) : v(v) {}
  constexpr ~LateOk() post(true) post(sizeof(this->v) == sizeof(int)) {}
};
constexpr int gok() {
  LateOk l(1);
  return 1;
}
static_assert(gok() == 1);

// Whether a variable has constant destruction does not depend on the
// contract assertions of its destructor, but those of a constexpr variable
// are checked (see constexpr-constant-destruction.cpp).
constexpr D constant_destruction(2);
} // namespace destructor

namespace assignment {
struct A {
  int x = 1;
  constexpr A &operator=(const A &o) pre(o.x > 0) post(r : r.x == o.x) { // #A
    x = o.x;
    return *this;
  }
};
constexpr int f(int v) {
  A a, b;
  b.x = v;
  a = b; // #A-assign
  return a.x;
}
static_assert(f(2) == 2);
constexpr int fails = f(0);
// observe-warning@#A {{precondition 'o.x > 0' evaluated to false}}
// observe-note@#A-assign {{in call to 'a.operator=(b)'}}
// observe-note@-3 {{in call to 'f(0)'}}
// enforce-error@-4 {{constexpr variable 'fails' must be initialized by a constant expression}}
// enforce-note@#A {{precondition 'o.x > 0' evaluated to false}}
// enforce-note@#A-assign {{in call to 'a.operator=(b)'}}
// enforce-note@-7 {{in call to 'f(0)'}}

// A defaulted assignment operator of a union, which is evaluated as a copy
// of the object representation.
union U {
  int i;
  float f;
  constexpr U(int i) : i(i) {}
  constexpr U &operator=(const U &o) pre(o.i != 3) post(r : &r == this); // #U
};
constexpr U &U::operator=(const U &) = default;
constexpr int g(int v) {
  U a(1), b(v);
  a = b; // #U-assign
  return a.i;
}
static_assert(g(2) == 2);
constexpr int union_fails = g(3);
// observe-warning@#U {{precondition 'o.i != 3' evaluated to false}}
// observe-note@#U-assign {{in call to 'a.operator=(b)'}}
// observe-note@-3 {{in call to 'g(3)'}}
// enforce-error@-4 {{constexpr variable 'union_fails' must be initialized by a constant expression}}
// enforce-note@#U {{precondition 'o.i != 3' evaluated to false}}
// enforce-note@#U-assign {{in call to 'a.operator=(b)'}}
// enforce-note@-7 {{in call to 'g(3)'}}
} // namespace assignment

namespace lambdas {
constexpr auto lam = [](int x) pre(x > 0) post(r : r > 1) { return x; }; // #lam
static_assert(lam(2) == 2);
constexpr int a = lam(0);
// observe-warning@#lam {{precondition 'x > 0' evaluated to false}}
// observe-warning@#lam {{postcondition 'r : r > 1' evaluated to false}}
// observe-note@-3 2 {{in call to 'lam.operator()(0)'}}
// enforce-error@-4 {{constexpr variable 'a' must be initialized by a constant expression}}
// enforce-note@#lam {{precondition 'x > 0' evaluated to false}}
// enforce-note@-6 {{in call to 'lam.operator()(0)'}}

// The contracts are evaluated once when the lambda is called through its
// conversion to a function pointer.
constexpr int (*fp)(int) = lam;
constexpr int b = fp(1);
// observe-warning@#lam {{postcondition 'r : r > 1' evaluated to false}}
// observe-note@* {{in call to}}
// enforce-error@-3 {{constexpr variable 'b' must be initialized by a constant expression}}
// enforce-note@#lam {{postcondition 'r : r > 1' evaluated to false}}
// enforce-note@* {{in call to}}

// Captures are available to the contracts.
constexpr int captures(int v) {
  int limit = 3;
  auto l = [limit, v](int x) pre(x < limit) post(r : r < limit + v) { return x + v; }; // #captures
  return l(v); // #captures-call
}
static_assert(captures(1) == 2);
constexpr int c = captures(3);
// observe-warning@#captures {{precondition 'x < limit' evaluated to false}}
// observe-warning@#captures {{postcondition 'r : r < limit + v' evaluated to false}}
// observe-note@#captures-call 2 {{in call to 'l.operator()(3)'}}
// observe-note@-4 2 {{in call to 'captures(3)'}}
// enforce-error@-5 {{constexpr variable 'c' must be initialized by a constant expression}}
// enforce-note@#captures {{precondition 'x < limit' evaluated to false}}
// enforce-note@#captures-call {{in call to 'l.operator()(3)'}}
// enforce-note@-8 {{in call to 'captures(3)'}}

// Generic lambdas.
constexpr auto generic = [](auto x) pre(x > 0) { return x; }; // #generic
static_assert(generic(1) == 1 && generic(2.0) == 2.0);
constexpr long d = generic(0L);
// observe-warning@#generic {{precondition 'x > 0' evaluated to false}}
// observe-note@-2 {{in call to 'generic.operator()<long>(0)'}}
// enforce-error@-3 {{constexpr variable 'd' must be initialized by a constant expression}}
// enforce-note@#generic {{precondition 'x > 0' evaluated to false}}
// enforce-note@-5 {{in call to 'generic.operator()<long>(0)'}}
} // namespace lambdas

namespace redeclaration {
// The contracts are written on the first declaration only.
constexpr int f(int x) pre(x > 0) post(r : r > 1); // #redecl
constexpr int f(int y) { return y; }
static_assert(f(2) == 2);
constexpr int a = f(0);
// observe-warning@#redecl {{precondition 'x > 0' evaluated to false}}
// observe-warning@#redecl {{postcondition 'r : r > 1' evaluated to false}}
// observe-note@-3 2 {{in call to 'f(0)'}}
// enforce-error@-4 {{constexpr variable 'a' must be initialized by a constant expression}}
// enforce-note@#redecl {{precondition 'x > 0' evaluated to false}}
// enforce-note@-6 {{in call to 'f(0)'}}

struct S {
  constexpr int m(int x) const pre(x > v) post(r : r == v); // #redecl-member
  int v;
};
constexpr int S::m(int) const { return v; }
constexpr S s{1};
static_assert(s.m(2) == 1);
constexpr int b = s.m(1);
// observe-warning@#redecl-member {{precondition 'x > v' evaluated to false}}
// observe-note@-2 {{in call to 's.m(1)'}}
// enforce-error@-3 {{constexpr variable 'b' must be initialized by a constant expression}}
// enforce-note@#redecl-member {{precondition 'x > v' evaluated to false}}
// enforce-note@-5 {{in call to 's.m(1)'}}
} // namespace redeclaration
