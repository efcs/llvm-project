// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -fblocks %s

// [dcl.contract.res], [basic.pre]: a result binding is a local entity, which
// can be captured by a lambda-expression within the postcondition.
// [expr.prim.id.unqual]p6: an id-expression naming it is const, unless it
// names the member of a closure type declared for a capture by copy
// ([expr.prim.id.unqual]p4), which is const unless the lambda is mutable.

template <class T, class U> constexpr bool same = false;
template <class T> constexpr bool same<T, T> = true;
#define SAME(...) static_assert(same<__VA_ARGS__>)

int f1() post(r: [&] { return r > 0; }()) { return 1; }
int f2() post(r: [r] { return r > 0; }()) { return 1; }
int f3() post(r: [=] { return r > 0; }()) { return 1; }
int f4() post(r: [x = r] { return x > 0; }()) { return 1; }
int f5() post(r: [&x = r] { return x > 0; }()) { return 1; }
int f6() post(r: [&r] { return r > 0; }()) { return 1; }
int f7() post(r: [=]() mutable { ++r; return r > 0; }()) { return 1; } // OK, refers to member of closure type
int f8() post(r: [r]() mutable { ++r; return r > 0; }()) { return 1; }
int f9() post(r: [&]() mutable { ++r; return r > 0; }()) { return 1; } // expected-error {{cannot assign to result name 'r' because it is considered 'const' inside of a contract}}
int f10() post(r: [&r]() mutable { ++r; return r > 0; }()) { return 1; } // expected-error {{cannot assign to result name 'r' because it is considered 'const' inside of a contract}}
int f11() post(r: [=]() { ++r; return r > 0; }()) { return 1; } // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
int f12() post(r: [x = r]() mutable { ++x; return x > 0; }()) { return 1; }
int f13() post(r: [&x = r]() mutable { ++x; return x > 0; }()) { return 1; } // expected-error {{cannot assign to variable 'x' with const-qualified type 'const int &'}}
// expected-note@-1 {{variable 'x' declared const here}}
int f14() post(r: ++r) { return 1; } // expected-error {{cannot assign to result name 'r' because it is considered 'const' inside of a contract}}
int f15() post(r: [&] { return [&] { return [=] { return r > 0; }(); }(); }()) { return 1; }
int f16() post(r: [&] { return [=]() mutable { ++r; return r > 0; }(); }()) { return 1; }
int f17() post(r: [=] { return [&]() mutable { ++r; return r > 0; }(); }()) { return 1; } // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}

// Types.
int t1() post(r: [&] { SAME(decltype(r), int); SAME(decltype((r)), const int &); return true; }()) { return 1; }
int t2() post(r: [=] { SAME(decltype(r), int); SAME(decltype((r)), const int &); return true; }()) { return 1; }
int t3() post(r: [=]() mutable { SAME(decltype(r), int); SAME(decltype((r)), int &); return true; }()) { return 1; }
int t4() post(r: same<decltype(r), int> && same<decltype((r)), const int &>) { return 1; }
int &t5() post(r: same<decltype(r), int &> && same<decltype((r)), const int &>);
const int &t6() post(r: same<decltype(r), const int &> && same<decltype((r)), const int &>);
int &&t7() post(r: same<decltype(r), int &&> && same<decltype((r)), const int &>);
int &t8() post(r: [=]() mutable { ++r; SAME(decltype((r)), int &); return true; }());
int &t9() post(r: [&]() mutable { ++r; return true; }()); // expected-error {{cannot assign to result name 'r' because it is considered 'const' inside of a contract}}
int &t10() post(r: [x = r] { SAME(decltype(x), int); return true; }());
int &t11() post(r: [&x = r] { SAME(decltype(x), const int &); return true; }());
int *t12() post(r: [&] { return ++*r; }());
int *t13() post(r: [&] { return ++r; }()); // expected-error {{cannot assign to result name 'r' because it is considered 'const' inside of a contract}}
struct S { int m; void nc(); void c() const; }; // expected-note 2 {{'nc' declared here}}
S t14() post(r: [&] { return r.m; }());
S t15() post(r: [&] { r.nc(); return true; }()); // expected-error {{'this' argument to member function 'nc' has type 'const S', but function is not marked const}}
S t16() post(r: [=]() mutable { r.nc(); return true; }());
S t17() post(r: [=]() { r.nc(); return true; }()); // expected-error {{'this' argument to member function 'nc' has type 'const S', but function is not marked const}}
S t18() post(r: [=]() { r.c(); return true; }());

// Multiple postconditions each introduce their own result binding.
int m1() post(r: [&] { return r > 0; }()) post(s: [=] { return s > 0; }()) post(r: r > 0) { return 1; }

// Templates and deduced return types.
template <class T> T g1(const T x) post(r: [&] { return r == x; }()) post(r: [r, x] { return r == x; }()) { return x; }
template <class T> auto g2(const T x) post(r: [&] { return r == x; }()) post(r: [r, x] { SAME(decltype(r), T); return r == x; }()) { return x; }
template <class T> T &g3(T &x) post(r: [&] { SAME(decltype((r)), const T &); return &r == &x; }()) post(r: [=]() mutable { ++r; return true; }()) { return x; }
template <class T> auto g4(T x) post(r: [&] { return r == x; }()) { return x; } // expected-error {{parameter 'x' referenced in contract postcondition must be declared const}}
// expected-note@-1 {{parameter of type 'int' is declared here}}
template <class T> auto g5(T x) post(r: [&] { return r > 0; }()) {} // expected-error {{result name 'r' cannot be used with a void return type}}
void use_templates() {
  g1(1);
  g2(1);
  int i = 0;
  g3(i);
  g4(1); // expected-note {{in instantiation of function template specialization 'g4<int>' requested here}}
  g5(1); // expected-note {{in instantiation of function template specialization 'g5<int>' requested here}}
}
auto a1(const int x) post(r: [&] { return r == x; }()) { return x; }
auto a2(const int x) post(r: [r, x] { int j = r; return j == x; }()) { return x; }
auto a3(const int x) post(r: [x] { struct L { static int h(int v) { static int s = 0; return ++s + v; } }; return L::h(x) > 0; }() && r > 0) { return x; }
auto a4(const int x) post(r: [=]() mutable { ++r; SAME(decltype((r)), int &); return r > x; }()) { return x; }
auto a5() post(r: [&] { return r > 0; }()) {} // expected-error {{result name 'r' cannot be used with a void return type}}
decltype(auto) a6(const int &x) post(r: [&] { SAME(decltype(r), const int &); return &r == &x; }()) { return (x); }
struct M {
  int m = 1;
  auto get() post(r: [&] { return r == m; }()) post(r: [=, this] { return r == m; }()) { return m; }
  auto ref() -> const int & post(r: [&] { return &r == &m; }()) { return m; }
  auto deduced() post(r: [this, r] { return r == m; }()) { return m; }
};
void lambdas() {
  auto l1 = [](const int x) post(r: [&] { return r == x; }()) { return x; };
  auto l2 = [](const int x) post(r: [r, x] { int j = r; return j == x; }()) { return x; };
  auto l3 = [](auto x) post(r: [r, x] { return r == x; }()) { return x; }; // expected-error {{parameter 'x' referenced in contract postcondition must be declared const}}
  // expected-note@-1 {{parameter of type 'int' is declared here}}
  auto l4 = [](const auto x) post(r: [r, x] { SAME(decltype(r), int); return r == x; }()) { return x; };
  auto l5 = [](const int x) -> int post(r: [=]() mutable { ++r; return r > x; }()) { return x; };
  auto l6 = [](int x) post(r: [&] { return r > 0; }()) { }; // expected-error {{result name 'r' cannot be used with a void return type}}
  auto l7 = [](const int x) post(r: [&] { return [&] { return r == x; }(); }()) { return x; };
  l1(1);
  l2(1);
  l3(1); // expected-note {{in instantiation of function template specialization 'lambdas()::(lambda)::operator()<int>' requested here}}
  l4(1);
  l5(1);
  l7(1);
}

// A result binding cannot be captured by a block.
int b1() post(r: ^{ return r > 0; }()) { return 1; } // expected-error {{reference to result name 'r' declared in enclosing function 'b1'}} expected-note {{'r' declared here}}

// A lambda without a capture-default cannot capture it implicitly.
int n1() post(r: [] { return r > 0; }()) { return 1; } // expected-error {{result name 'r' cannot be implicitly captured in a lambda with no capture-default specified}}
// expected-note@-1 {{'r' declared here}} expected-note@-1 {{lambda expression begins here}} expected-note@-1 2 {{capture 'r' by}} expected-note@-1 2 {{default capture by}}
int n2() post(r: [] { return sizeof(decltype((r))) > 0; }()) { return 1; } // OK, not odr-used.
int n3() post(r: [] { return sizeof(r) > 0; }()) { return 1; } // OK, not odr-used.
