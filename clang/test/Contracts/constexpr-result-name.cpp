// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcontract-evaluation-semantic=enforce -verify %s

// The result name of a postcondition assertion denotes the result object or
// the returned glvalue of the call during constant evaluation
// ([dcl.contract.res]).

namespace scalar {
constexpr int f(const int x) post(r : r == x + 1) { return x + 1; }
static_assert(f(1) == 2);

constexpr int g(const int x) post(r : r == x) { return x + 1; } // #scalar-g
constexpr int bad = g(1);
// expected-error@-1 {{constexpr variable 'bad' must be initialized by a constant expression}}
// expected-note@#scalar-g {{contract violation with the 'enforce' evaluation semantic: postcondition 'r : r == x' evaluated to false}}
// expected-note@-3 {{in call to 'g(1)'}}

constexpr double d(const double x) post(r : r > x) { return x * 2; }
static_assert(d(1.5) == 3.0);

constexpr const int *p(const int *const q) post(r : r == q) post(r : *r == 3) { return q; }
constexpr int three = 3;
static_assert(*p(&three) == 3);

struct M { int a, b; };
constexpr int M::*mp(const bool first) post(r : r == (first ? &M::a : &M::b)) { return first ? &M::a : &M::b; }
static_assert(mp(true) == &M::a && mp(false) == &M::b);

enum class E { A, B };
constexpr E e(const int x) post(r : r == (x ? E::B : E::A)) { return x ? E::B : E::A; }
static_assert(e(1) == E::B);

// The result name can be used several times and by several postcondition
// assertions.
constexpr int multi(const int x) post(a : a > 0 && a == x) post(b : b + b == 2 * x) post(x > 0) { return x; }
static_assert(multi(21) == 21);

// A postcondition assertion without a result name still sees the parameters.
constexpr int no_name(const int x) post(x > 0) { return 0; } // #no-name
constexpr int bad_no_name = no_name(0);
// expected-error@-1 {{constexpr variable 'bad_no_name' must be initialized by a constant expression}}
// expected-note@#no-name {{postcondition 'x > 0' evaluated to false}}
// expected-note@-3 {{in call to 'no_name(0)'}}

// The result can be observed through the result name by later postcondition
// assertions and by the caller.
constexpr int modify(const int x) post(r : ++const_cast<int &>(r) == x + 1) post(r : r == x + 1) { return x; }
static_assert(modify(1) == 2);
} // namespace scalar

namespace reference {
constexpr int &ref(int &x) post(r : &r == &x) post(r : r > 0) { return x; } // #ref
constexpr int use(int v) {
  int x = v;
  ref(x) = 5; // #ref-call
  return x;
}
static_assert(use(1) == 5);
constexpr int bad = use(0);
// expected-error@-1 {{constexpr variable 'bad' must be initialized by a constant expression}}
// expected-note@#ref {{postcondition 'r : r > 0' evaluated to false}}
// expected-note@#ref-call {{in call to 'ref(x)'}}
// expected-note@-4 {{in call to 'use(0)'}}

constexpr const int &cref(const int &x) post(r : r == 3) { return x; }
constexpr int three = 3;
static_assert(cref(three) == 3);
static_assert(&cref(three) == &three);

constexpr int &&rref(int &&x) post(r : r == 7) { return static_cast<int &&>(x); }
static_assert(rref(7) == 7);

struct S { int m; };
constexpr int &member(S &s) post(r : &r == &s.m) { return s.m; }
constexpr int use_member() {
  S s{1};
  member(s) = 2;
  return s.m;
}
static_assert(use_member() == 2);
} // namespace reference

namespace class_type {
struct P {
  int x, y;
  constexpr P(int x, int y) : x(x), y(y) {}
  constexpr int sum() const { return x + y; }
};
constexpr P make(const int a, const int b) post(r : r.x == a && r.y == b) post(r : r.sum() == a + b) { return P(a, b); }
constexpr P wrong(const int a) post(r : r.x != a) { return P(a, 0); } // #wrong

// The result object is the variable being initialized, ...
constexpr P var = make(1, 2);
static_assert(var.x == 1 && var.y == 2);
constexpr P bad = wrong(1);
// expected-error@-1 {{constexpr variable 'bad' must be initialized by a constant expression}}
// expected-note@#wrong {{postcondition 'r : r.x != a' evaluated to false}}
// expected-note@-3 {{in call to 'wrong(1)'}}

// ... a subobject of it, ...
struct Agg { P a, b; };
constexpr Agg agg{make(1, 2), make(3, 4)};
static_assert(agg.a.y == 2 && agg.b.x == 3);
constexpr P array[3] = {make(5, 6), make(7, 8), make(9, 10)};
static_assert(array[1].y == 8 && array[2].x == 9);
struct Derived : P { int z; constexpr Derived() : P(make(11, 12)), z(make(13, 14).y) {} };
static_assert(Derived().x == 11 && Derived().z == 14);

// ... a temporary, a parameter, a local variable, or a heap allocation.
static_assert(make(15, 16).y == 16);
constexpr int take(P p) { return p.x; }
static_assert(take(make(17, 18)) == 17);
constexpr int local() {
  P p = make(1, 2);
  P *heap = new P(make(3, 4));
  int v = p.x + heap->y;
  delete heap;
  return v;
}
static_assert(local() == 5);

// Guaranteed copy elision through several calls with postconditions.
constexpr P forward(const int a) post(r : r.x == a) { return make(a, a); }
constexpr P twice = forward(19);
static_assert(twice.y == 19);

// The address of the result object is the address of the variable.
struct Self {
  const Self *self;
  constexpr Self() : self(this) {}
};
constexpr Self make_self() post(r : r.self == &r) { return Self(); }
constexpr Self self = make_self();
static_assert(self.self == &self);

// A class type that is returned in registers (trivially copyable).
struct Small { int v; };
constexpr Small small(const int v) post(r : r.v == v) { return {v}; }
static_assert(small(3).v == 3);
constexpr Small smalls[2] = {small(1), small(2)};
static_assert(smalls[1].v == 2);

// A union.
union U { int i; float f; };
constexpr U make_union(const int v) post(r : r.i == v) { return U{.i = v}; }
static_assert(make_union(4).i == 4);
} // namespace class_type

namespace templates {
template <class T> constexpr T id(const T x) post(r : r == x) { return x; }
static_assert(id(1) == 1 && id(2.5) == 2.5 && id('c') == 'c');

template <class T> constexpr T broken(const T x) post(r : r != x) { return x; } // #broken
constexpr long bad = broken(1L);
// expected-error@-1 {{constexpr variable 'bad' must be initialized by a constant expression}}
// expected-note@#broken {{postcondition 'r : r != x' evaluated to false}}
// expected-note@-3 {{in call to 'broken<long>(1)'}}

constexpr auto deduced(const int x) post(r : r == x) { return x; }
static_assert(deduced(3) == 3);

struct Member {
  int v;
  constexpr int get() const post(r : r == v) { return v; }
  constexpr const Member &self() const post(r : &r == this) { return *this; }
};
static_assert(Member{4}.get() == 4);
constexpr Member m{5};
static_assert(&m.self() == &m);
} // namespace templates

namespace not_evaluated {
// A result name can't be evaluated outside of the postcondition assertions
// of a call, for instance when the predicate is analyzed for warnings.
constexpr int f(int x) post(r : r / 1 == r) post(r : r && true) { return x; }
static_assert(f(1) == 1);
} // namespace not_evaluated
