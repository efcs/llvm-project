// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [expr.prim.id.unqual], [expr.prim.id.qual]: Within a contract predicate,
// an id-expression naming a variable or structured binding declared outside
// of the contract assertion is const.

int g = 0;
static int s = 0;
namespace N { int n = 0; }
struct C { static int sm; int m; mutable int mm; bool nc(); }; // expected-note {{'nc' declared here}}

void globals() {
  static int ls = 0;
  thread_local int t = 0;
  contract_assert(++g); // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}
  contract_assert(++s); // expected-error {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
  contract_assert(++N::n); // expected-error {{cannot assign to variable 'n' because it is considered 'const' inside of a contract}}
  contract_assert(++::g); // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}
  contract_assert(++C::sm); // expected-error {{cannot assign to variable 'sm' because it is considered 'const' inside of a contract}}
  contract_assert(++ls); // expected-error {{cannot assign to variable 'ls' because it is considered 'const' inside of a contract}}
  contract_assert(++t); // expected-error {{cannot assign to variable 't' because it is considered 'const' inside of a contract}}
}
void pre_global() pre(++g); // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}

void pointers(int *p, C *pc, C &rc, C c) {
  contract_assert(++*p); // OK, not deep
  contract_assert(++pc->m); // OK
  contract_assert(++c.mm); // OK, mutable
  contract_assert(++rc.m); // expected-error {{cannot assign to variable 'rc' because it is considered 'const' inside of a contract}}
  contract_assert(c.nc()); // expected-error {{'this' argument to member function 'nc' has type 'const C', but function is not marked const}}
}

struct Agg { int a, b; };
void bindings(Agg x) {
  auto [a, b] = x;
  contract_assert(++a); // expected-error {{cannot assign to structured binding 'a' because it is considered 'const' inside of a contract}}
  int arr[2] = {};
  auto &[c, d] = arr;
  contract_assert(++c); // expected-error {{cannot assign to structured binding 'c' because it is considered 'const' inside of a contract}}
  static_assert(__is_same(decltype((a)), int &));
  contract_assert(__is_same(decltype((a)), const int &));
}

// Variables declared within the predicate are not const.
void inside(int x) {
  contract_assert([] { int y = 0; ++y; return y; }() > 0);
  contract_assert(({ int z = 0; ++z; z; }) > 0);
  contract_assert([x]() mutable { ++x; return x; }() > 0);
  contract_assert([=]() mutable { ++x; return x; }() > 0);
  contract_assert([y = x]() mutable { ++y; return y; }() > 0);
  contract_assert([&x] { ++x; return x; }() > 0); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
}

// A lambda defined outside of the contract assertion is unaffected.
void outside(int x) {
  [&] { contract_assert(x > 0); ++x; }();
  auto gl = [&](auto y) { ++x; return y; };
  contract_assert(gl(1) > 0);
}

// Nested contract assertions (e.g. in a statement-expression) are allowed.
void nested(int x) {
  contract_assert(({ contract_assert(x > 0); true; }));
  contract_assert(({ contract_assert(++x); true; })); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
}

// Overload resolution sees the const lvalue.
struct Sel {
  bool select() &;
  bool select() const & { return true; }
};
struct SelDel {
  bool select() = delete;
  bool select() const { return true; }
};
void overloads(Sel v, SelDel w) pre(v.select()) pre(w.select()) {
  contract_assert(w.select());
}

// 'this' is const, including in lambdas within the predicate.
struct T {
  int m;
  void f() {
    contract_assert([&] { ++m; return true; }()); // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
    [&] {
      contract_assert([&] { ++m; return true; }()); // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
      ++m;
    }();
  }
};
