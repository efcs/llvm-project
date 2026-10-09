// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -Wno-vla-cxx-extension %s

// [expr.prim.id.unqual]p7, [expr.prim.id.qual]p7, [expr.prim.this]p3: within
// the predicate of a contract assertion, id-expressions naming variables and
// structured bindings declared outside of it and 'this' are const. Within a
// lambda-expression in the predicate, an entity captured by copy names the
// member of the closure type instead ([expr.prim.id.unqual]p4), which is
// const unless the lambda is mutable.

template <class T, class U> constexpr bool same = false;
template <class T> constexpr bool same<T, T> = true;
#define SAME(...) static_assert(same<__VA_ARGS__>)

struct NC {};
struct CC {};
template <class T> NC cq(T &);
template <class T> CC cq(const T &);
// Whether an expression is a const lvalue.
#define IS_CONST(e) same<decltype(cq(e)), CC>

int g = 0;
namespace N { int n = 0; }
struct S { static int sm; int m; };

// Nested lambdas, in both directions.
void nested(int x) {
  contract_assert([&] {
    int j = 0;
    contract_assert(++j); // expected-error {{cannot assign to variable 'j' because it is considered 'const' inside of a contract}}
    contract_assert([&] { ++j; return true; }()); // expected-error {{cannot assign to variable 'j' because it is considered 'const' inside of a contract}}
    ++j; // OK, declared within the predicate.
    [&] { ++j; ++x; }(); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
    [&] {
      int k = 0;
      ++x; // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
      contract_assert([&] { ++k; return true; }()); // expected-error {{cannot assign to variable 'k' because it is considered 'const' inside of a contract}}
    }();
    return true;
  }());
  [&] {
    ++x; // OK
    contract_assert([&] { ++x; return true; }()); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
    [&] { ++x; contract_assert(++x); }(); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  }();
  // A capture by copy within the predicate names the closure member.
  contract_assert([=]() mutable { ++x; return [&] { ++x; return [=]() mutable { ++x; return true; }(); }(); }());
  contract_assert([=]() { return [&] { return [=]() mutable { ++x; return true; }(); }(); }()); // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  contract_assert([&]() { return [&] { return [&]() mutable { ++x; return true; }(); }(); }()); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  // A capture by copy by a lambda outside of the predicate does not help.
  [x]() mutable { contract_assert(++x); }(); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  [=]() mutable { contract_assert(++x); ++x; }(); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert([x]() mutable { contract_assert(++x); return true; }()); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
}

// 'this'.
struct T {
  int m = 0;
  mutable int mm = 0;
  static int sm;
  bool nc(); // expected-note 2 {{'nc' declared here}}
  bool c() const;
  void f(int x) {
    contract_assert(same<decltype(this), const T *>);
    contract_assert(same<decltype((m)), const int &>);
    contract_assert(same<decltype(m), int>);
    contract_assert(nc()); // expected-error {{'this' argument to member function 'nc' has type 'const T', but function is not marked const}}
    // expected-note@-1 {{'this' is 'const' within contract introduced here}}
    contract_assert(c());
    contract_assert(++mm);
    contract_assert(++sm); // expected-error {{cannot assign to variable 'sm' because it is considered 'const' inside of a contract}}
    contract_assert(++m); // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
    contract_assert([this] { SAME(decltype(this), const T *); return true; }());
    contract_assert([this]() mutable { SAME(decltype(this), const T *); return true; }());
    contract_assert([&] { SAME(decltype(this), const T *); ++m; return true; }()); // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
    contract_assert([=] { SAME(decltype(this), const T *); return true; }());
    contract_assert([*this] { SAME(decltype(this), const T *); return true; }());
    contract_assert([*this]() mutable { SAME(decltype(this), T *); ++m; return true; }());
    contract_assert([=, *this]() mutable { SAME(decltype(this), T *); ++m; return true; }());
    contract_assert([*this]() mutable { return [&] { SAME(decltype(this), T *); ++m; return true; }(); }());
    contract_assert([*this]() mutable { return [this] { SAME(decltype(this), T *); ++m; return true; }(); }());
    contract_assert([*this]() { return [this]() mutable { SAME(decltype(this), const T *); return true; }(); }());
    contract_assert([*this]() mutable { return [*this]() { SAME(decltype(this), const T *); return true; }(); }());
    contract_assert([*this]() mutable { return [*this]() mutable { SAME(decltype(this), T *); ++m; return true; }(); }());
    // A copy of '*this' captured outside of the predicate is const within it.
    contract_assert([*this]() mutable { contract_assert(same<decltype(this), const T *>); return true; }());
    [*this]() mutable { contract_assert(same<decltype(this), const T *>); ++m; }();
    [this]() mutable { contract_assert(same<decltype(this), const T *>); ++m; }();
    [&] { contract_assert(same<decltype(this), const T *>); ++m; }();
    // The current class of a local class does not enclose the contract assertion.
    contract_assert([] {
      struct L {
        int z;
        void h() { ++z; ++g; SAME(decltype(this), L *); } // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}
      };
      return true;
    }());
  }
  void cf() const {
    contract_assert([*this]() mutable { SAME(decltype(this), const T *); return true; }());
  }
  static void sf() pre(++sm); // expected-error {{cannot assign to variable 'sm' because it is considered 'const' inside of a contract}}
  void pre_post() pre(++m) post(++m); // expected-error 2 {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
  void pre_post2() pre([*this]() mutable { ++m; return true; }()) post([this] { ++mm; return true; }());
  void pre_post3() pre(nc()); // expected-error {{'this' argument to member function 'nc' has type 'const T', but function is not marked const}}
  // expected-note@-1 {{'this' is 'const' within contract introduced here}}
};

// Init-captures, decltype, references and pointers.
void init_captures(int x, int &ref, int *p) {
  contract_assert([y = x] { SAME(decltype(y), int); return true; }());
  contract_assert([&y = x] { SAME(decltype(y), const int &); return true; }());
  contract_assert([y = ref] { SAME(decltype(y), int); return true; }());
  contract_assert([&y = ref] { SAME(decltype(y), const int &); return true; }());
  contract_assert([y = &x] { SAME(decltype(y), const int *); return true; }());
  contract_assert([&y = x]() mutable { ++y; return true; }()); // expected-error {{cannot assign to variable 'y' with const-qualified type 'const int &'}}
  // expected-note@-1 {{variable 'y' declared const here}}
  contract_assert([y = x]() mutable { ++y; return true; }());
  [y = x]() mutable { contract_assert(++y); }(); // expected-error {{cannot assign to variable 'y' because it is considered 'const' inside of a contract}}
  [&y = x]() mutable { contract_assert(++y); }(); // expected-error {{cannot assign to variable 'y' because it is considered 'const' inside of a contract}}

  contract_assert(same<decltype(x), int>);
  contract_assert(same<decltype((x)), const int &>);
  contract_assert(same<decltype(ref), int &>);
  contract_assert(same<decltype((ref)), const int &>);
  contract_assert(same<decltype(p), int *>);
  contract_assert(same<decltype((p)), int *const &>);
  contract_assert(same<decltype(*p), int &>);
  contract_assert(same<decltype((g)), const int &>);
  contract_assert(same<decltype((::g)), const int &>);
  contract_assert(same<decltype((N::n)), const int &>);
  contract_assert(same<decltype((S::sm)), const int &>);
  contract_assert(++*p); // OK, not deep
  contract_assert(++ref); // expected-error {{cannot assign to variable 'ref' because it is considered 'const' inside of a contract}}
  contract_assert(++N::n); // expected-error {{cannot assign to variable 'n' because it is considered 'const' inside of a contract}}
  contract_assert([=] { SAME(decltype(x), int); SAME(decltype((x)), const int &); return true; }());
  contract_assert([=]() mutable { SAME(decltype((x)), int &); SAME(decltype((ref)), int &); return true; }());
  contract_assert([&] { SAME(decltype((x)), const int &); SAME(decltype((ref)), const int &); return true; }());
  contract_assert([&]() mutable { SAME(decltype((x)), const int &); return true; }());
  contract_assert([=]() { SAME(decltype((ref)), const int &); return true; }());
  contract_assert([]() mutable { SAME(decltype((g)), const int &); return true; }());
  contract_assert([]() { SAME(decltype((x)), const int &); return true; }()); // OK, unevaluated operand.
  contract_assert(sizeof(++x)); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert(noexcept(++x)); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
}

// Overload resolution sees the const lvalue.
void overloads(int x, int &r) pre(IS_CONST(x)) pre(IS_CONST(r)) pre(IS_CONST(g)) {
  contract_assert(IS_CONST(x) && IS_CONST(r) && IS_CONST(g));
  contract_assert([&] { return IS_CONST(x) && IS_CONST(r) && IS_CONST(g); }());
  contract_assert([=]() mutable { return !IS_CONST(x) && !IS_CONST(r) && IS_CONST(g); }());
  contract_assert([=]() { return IS_CONST(x) && IS_CONST(r) && IS_CONST(g); }());
}

// Structured bindings.
namespace std {
template <class T> struct tuple_size;
template <unsigned long I, class T> struct tuple_element;
} // namespace std
struct Tup {
  int v;
  template <unsigned long I> int &get() { return v; }
  template <unsigned long I> const int &get() const { return v; }
};
template <> struct std::tuple_size<Tup> { static constexpr unsigned long value = 2; };
template <unsigned long I> struct std::tuple_element<I, Tup> { using type = int; };
struct Agg { int a; int b : 3; };
void bindings(Agg agg, Tup tup) {
  auto [a, b] = agg;
  auto &[ra, rb] = agg;
  auto [t0, t1] = tup;
  auto &[rt0, rt1] = tup;
  contract_assert(++a); // expected-error {{cannot assign to structured binding 'a' because it is considered 'const' inside of a contract}}
  contract_assert(++b); // expected-error {{cannot assign to structured binding 'b' because it is considered 'const' inside of a contract}}
  contract_assert(++ra); // expected-error {{cannot assign to structured binding 'ra' because it is considered 'const' inside of a contract}}
  contract_assert(++rb); // expected-error {{cannot assign to structured binding 'rb' because it is considered 'const' inside of a contract}}
  contract_assert(++t0); // expected-error {{cannot assign to structured binding 't0' because it is considered 'const' inside of a contract}}
  contract_assert(++rt0); // expected-error {{cannot assign to structured binding 'rt0' because it is considered 'const' inside of a contract}}
  contract_assert(same<decltype(a), int> && same<decltype((a)), const int &>);
  contract_assert(same<decltype(t0), int> && same<decltype((t0)), const int &>);
  contract_assert([=]() mutable { ++a; ++t0; return true; }());
  contract_assert([&]() mutable { ++a; return true; }()); // expected-error {{cannot assign to structured binding 'a' because it is considered 'const' inside of a contract}}
  contract_assert([&]() mutable { ++t0; return true; }()); // expected-error {{cannot assign to structured binding 't0' because it is considered 'const' inside of a contract}}
  contract_assert([a]() mutable { ++a; return true; }());
  contract_assert([&a]() mutable { ++a; return true; }()); // expected-error {{cannot assign to structured binding 'a' because it is considered 'const' inside of a contract}}
  contract_assert([a]() { ++a; return true; }()); // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
}

// Parameters of the enclosing function in unevaluated operands and local
// classes.
void local_classes(int x) pre(sizeof(x) == sizeof(int)) // expected-note {{'x' declared here}}
  pre([] { struct L { int f() { return sizeof(x); } }; return L().f(); }())
  pre([] { struct L { int f() { return x; } }; return L().f(); }()); // expected-error {{reference to local variable 'x' declared in enclosing function 'local_classes'}}

// Pack expansions.
template <class... Ts> void packs(Ts... ts) pre((... && same<decltype((ts)), const Ts &>)) pre((++ts, ...)) {} // expected-error {{cannot assign to variable 'ts' because it is considered 'const' inside of a contract}}
template void packs<int, long>(int, long); // expected-note {{in instantiation of function template specialization 'packs<int, long>' requested here}}
template <class... Ts> void packs2(Ts... ts) {
  contract_assert((... && IS_CONST(ts)));
  contract_assert([&] { return (... && IS_CONST(ts)); }());
  contract_assert([=] { return (... && IS_CONST(ts)); }());
  contract_assert([=]() mutable { return (... && !IS_CONST(ts)); }());
  contract_assert([ts...]() mutable { return (... && !IS_CONST(ts)); }());
  contract_assert([&ts...]() mutable { return (... && IS_CONST(ts)); }());
  contract_assert([... us = ts]() mutable { return (... && !IS_CONST(us)); }());
  contract_assert([&... us = ts]() mutable { return (... && IS_CONST(us)); }());
}
template void packs2<int, long>(int, long);

// requires-expressions and their parameters.
template <class T> void req(T x) {
  contract_assert(!requires { ++x; });
  contract_assert(requires(T y) { ++y; });
  contract_assert(!requires(T y) { ++y; ++x; });
  contract_assert(requires(T y) { requires same<decltype((y)), T &>; requires same<decltype((x)), const T &>; });
}
template void req<int>(int);

// Statement-expressions.
void stmt_exprs(int x) {
  contract_assert(({ int z = 0; ++z; z; }));
  contract_assert(({ int z = 0; [&] { ++z; return true; }(); }));
  contract_assert(({ int z = 0; contract_assert(++z); true; })); // expected-error {{cannot assign to variable 'z' because it is considered 'const' inside of a contract}}
  contract_assert(({ ++x; true; })); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert(({ static int sz = 0; ++sz; }));
  contract_assert(({ struct L { static void f(int a) { ++a; ++g; } }; true; })); // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}
}

// Default arguments of lambdas declared in predicates.
void default_args(int x) {
  contract_assert([](int a = g) { return a; }());
  contract_assert([](int a = ++g) { return a; }()); // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}
  contract_assert([](int a = sizeof(++x)) { return a; }()); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert([](bool a = same<decltype((g)), const int &>) { return a; }());
}

// Immediate lambdas.
void immediate(int x) {
  contract_assert([]() consteval { return same<decltype((g)), const int &>; }());
  contract_assert([](int a) consteval { ++a; return a; }(1));
  contract_assert([](int a) consteval { return ++g; }(1)); // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}
}

// Template parameters of reference type.
int gi;
template <int N, int &R, int *P, const int &CR> void nttp()
  pre(++R) // expected-error {{cannot assign to variable 'R' because it is considered 'const' inside of a contract}}
  pre(++(*P))
  pre(same<decltype((R)), const int &>)
  pre(same<decltype(R), int &>)
  pre(same<decltype((CR)), const int &>)
{
  contract_assert(++R); // expected-error {{cannot assign to variable 'R' because it is considered 'const' inside of a contract}}
  contract_assert([] { return ++R; }()); // expected-error {{cannot assign to variable 'R' because it is considered 'const' inside of a contract}}
  contract_assert([] { static_assert(same<decltype((R)), const int &>); return true; }());
}
void use_nttp() { nttp<1, gi, &gi, gi>(); }
template <int &R> struct X {
  void f() pre(++R) { contract_assert(++R); } // expected-error 2 {{cannot assign to variable 'R' because it is considered 'const' inside of a contract}}
};
void use_x() { X<gi> x; x.f(); }

// Dependent contexts.
template <class T> struct D {
  T m;
  static T sm;
  void f(T x) pre(++x) pre(++m) pre(++sm) pre(++D::sm) pre(++this->m) {} // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  // expected-error@-1 2 {{cannot assign to variable 'sm' because it is considered 'const' inside of a contract}}
  // expected-error@-2 2 {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
  template <class U> void g(U u) {
    contract_assert(++u); // expected-error {{cannot assign to variable 'u' because it is considered 'const' inside of a contract}}
    contract_assert([&] { ++u; ++m; return true; }()); // expected-error {{cannot assign to variable 'u' because it is considered 'const' inside of a contract}}
    // expected-error@-1 {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
    // expected-note@-2 {{while substituting into a lambda expression here}}
    contract_assert([=]() mutable { ++u; return true; }());
    contract_assert([=, *this]() mutable { ++m; return true; }());
  }
};
void use_d() {
  D<int> d;
  d.f(1); // expected-note {{in instantiation of member function 'D<int>::f' requested here}}
  d.g(1); // expected-note 2 {{in instantiation of function template specialization 'D<int>::g<int>' requested here}}
}
template <class T> void dep(T x) {
  contract_assert(++T::sm); // expected-error {{cannot assign to variable 'sm' because it is considered 'const' inside of a contract}}
  contract_assert([](auto y) { ++T::sm; return y; }(1)); // expected-error {{cannot assign to variable 'sm' because it is considered 'const' inside of a contract}}
  // expected-note@-1 {{while substituting into a lambda expression here}}
  contract_assert([&](auto y) { static_assert(same<decltype((x)), const T &>); return y; }(1));
}
struct HasSM { static int sm; };
void use_dep() { dep(HasSM()); } // expected-note 2 {{in instantiation of function template specialization 'dep<HasSM>' requested here}}

// Generic lambdas in predicates whose call operators are instantiated later,
// and generic lambdas defined before the predicate.
template <class F> bool call_later(F f) { return f(1); }
template <class F> auto call_now(F f) { return f(1); }
struct G {
  int m;
  NC q(int);
  CC q(int) const;
  void f(int x) {
    contract_assert([&](auto y) { static_assert(IS_CONST(x)); return (void)x, y; }(1));
    contract_assert(call_now([&](auto y) { static_assert(IS_CONST(x)); return (void)x, y; }));
    contract_assert(call_later([&](auto y) { static_assert(IS_CONST(x)); return (void)x, y; }));
    contract_assert(call_later([&](auto y) { static_assert(IS_CONST(g)); return y; }));
    contract_assert(call_later([&](auto y) { static_assert(IS_CONST(m)); return (void)m, y; }));
    contract_assert(call_later([&](auto y) { static_assert(same<decltype(this), const G *>); return (void)m, y; }));
    contract_assert(call_later([&](auto y) { static_assert(same<decltype(this->q(y)), CC>); return (void)m, y; }));
    contract_assert(call_later([&](auto y) { static_assert(same<decltype(q(y)), CC>); return (void)m, y; }));
    contract_assert(call_later([=](auto y) mutable { static_assert(!IS_CONST(x)); return (void)x, y; }));
    auto gl = [&](auto y) {
      ++x;
      ++g;
      ++m;
      static_assert(!IS_CONST(x) && !IS_CONST(g) && !IS_CONST(m));
      static_assert(same<decltype(this->q(y)), NC>);
      return y;
    };
    contract_assert(gl(1));
    contract_assert(call_now(gl));
  }
};
template <class T> struct TG {
  T m;
  NC q(int);
  CC q(int) const;
  void f(T x) {
    contract_assert([&](auto y) { static_assert(IS_CONST(x)); return (void)x, y; }(1));
    contract_assert(call_later([&](auto y) { static_assert(IS_CONST(x)); return (void)x, y; }));
    contract_assert(call_later([&](auto y) { static_assert(IS_CONST(g)); return y; }));
    contract_assert(call_later([&](auto y) { static_assert(IS_CONST(m)); return (void)m, y; }));
    contract_assert(call_later([&](auto y) { static_assert(same<decltype(this->q(y)), CC>); return (void)m, y; }));
    contract_assert(call_later([&](auto y) { static_assert(same<decltype(q(y)), CC>); return (void)m, y; }));
    contract_assert(call_later([=](auto y) mutable { static_assert(!IS_CONST(x)); return (void)x, y; }));
    auto gl = [&](auto y) {
      ++x;
      ++g;
      ++m;
      static_assert(!IS_CONST(x) && !IS_CONST(g) && !IS_CONST(m));
      static_assert(same<decltype(this->q(y)), NC>);
      return y;
    };
    contract_assert(gl(1));
  }
};
template struct TG<int>;

// Variable length arrays.
void vla(int n) {
  int a[n];
  contract_assert(sizeof(a) > 0);
  contract_assert(a[0] == 0);
  contract_assert([&] { return sizeof(a) > 0 && a[0] == 0; }());
  contract_assert([n] { int b[n]; return sizeof(b) > 0; }());
  contract_assert([=] { int b[n]; return sizeof(b) > 0; }());
}
