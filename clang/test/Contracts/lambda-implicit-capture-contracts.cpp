// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [expr.prim.lambda.closure]p10: If all potential references to a local
// entity implicitly captured by a lambda-expression L occur within the
// function contract assertions of the call operator or operator template of L
// or within assertion-statements within the body of L, the program is
// ill-formed.

static int si = 0;
void test() {
  auto f1 = [=] pre(si > 0) {}; // OK, no local entities are captured.
  int i = 1;
  auto f2 = [=] pre(i > 0) {}; // expected-error {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'i' is required here}}
  auto f3 = [i] pre(i > 0) {}; // OK, i is captured explicitly.
  auto f4 = [=] { // expected-error {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
    contract_assert(i > 0); // expected-note {{within contract context introduced here}} expected-note {{capture of local entity 'i' is required here}}
  };
  auto f5 = [=] {
    contract_assert(i > 0); // OK, i is referenced elsewhere.
    (void)i;
  };
  auto f6 = [=] pre([] {
    bool x = true;
    return [=] { return x; }(); // OK, #1 captures nothing.
  }()) {};
  bool y = true;
  auto f7 = [=] pre([=] { return y; }()) {}; // expected-error {{implicit capture of local entity 'y' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'y' is required here}}
  auto f8 = [&] { contract_assert([&] { return i > 0; }()); }; // expected-error {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'i' is required here}}
  auto f9 = [&] { contract_assert([i] { return i > 0; }()); }; // expected-error {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'i' is required here}}
  auto f10 = [&] { (void)[=] pre(i > 0) {}; }; // OK for the outer lambda, the nested lambda refers to i outside of a contract assertion of the outer lambda.
  // expected-error@-1 {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
  // expected-note@-2 {{within contract context introduced here}} expected-note@-2 {{capture of local entity 'i' is required here}}
  auto f11 = [&] { (void)[i] pre(i > 0) {}; }; // OK
  auto f12 = [&](int p) pre(p > 0) { contract_assert(p > 0 && i > 0); return i; }; // OK, i is referenced in the body.
  auto f13 = [&]() pre(i > 0) { return 1; }; // expected-error {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'i' is required here}}
  auto f14 = [&]() post(r: r > i) { return 1; }; // expected-error {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'i' is required here}}
  auto f15 = [&]() post(r: [&] { return r > i; }()) { return 1; }; // expected-error {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'i' is required here}}
  // A reference in an unevaluated operand is not a potential reference which requires a capture.
  auto f16 = [=] { contract_assert(i > 0); (void)sizeof(i); }; // expected-error {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'i' is required here}}
  // Init-captures are explicit.
  auto f17 = [j = i] pre(j > 0) {};
  // Captures required by a reference outside of a contract, but first referenced within one.
  auto f18 = [=] { contract_assert(i > 0); return i; };
  auto f19 = [=] { return [&] { contract_assert(i > 0); return i; }(); };
}

// Generic lambdas and templates diagnose the same way.
void generic(int x) {
  auto a = [=](auto y) { contract_assert(x > 0); return y; }; // expected-error {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'x' is required here}}
  auto b = [&](auto y) { contract_assert(x > y); return y; }; // expected-error {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'x' is required here}}
  auto c = [=](auto y) pre(x > 0) { return y; }; // expected-error {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'x' is required here}}
  auto d = [=](auto y) pre(x > y) { return y; }; // expected-error {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'x' is required here}}
  auto e = [=](auto y) { contract_assert(x > y); return x; }; // OK
  // Both the outer and (once instantiated) the inner lambda are ill-formed.
  auto f = [&](auto y) { return [&] { contract_assert(x > y); return y; }(); }; // expected-error 2 {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 2 {{within contract context introduced here}} expected-note@-1 2 {{capture of local entity 'x' is required here}}
  a(1);
  b(1);
  c(1);
  d(1);
  e(1);
  f(1); // expected-note {{in instantiation of function template specialization 'generic(int)::(lambda)::operator()<int>' requested here}}
}
template <class T> void tmpl(T x) {
  int local = 202;
  auto a = [=](auto y) { contract_assert(x > 0); return y; }; // expected-error {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'x' is required here}}
  auto b = [&](int y) { contract_assert(x > 0 && local > 0); return y; }; // expected-error {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-error@-1 {{implicit capture of local entity 'local' is not allowed when used exclusively in contract assertions}}
  // expected-note@-2 2 {{within contract context introduced here}} expected-note@-2 {{capture of local entity 'x' is required here}} expected-note@-2 {{capture of local entity 'local' is required here}}
  auto c = [=](auto y) pre(x > y) { return y; }; // expected-error {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'x' is required here}}
  auto d = [&](auto y) { contract_assert(x > y); return local; }; // expected-error {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of local entity 'x' is required here}}
  a(1);
  c(1);
  d(1);
}
template void tmpl<int>(int); // expected-note {{in instantiation of function template specialization 'tmpl<int>' requested here}}

// 'this'.
struct S {
  int m;
  void f() {
    auto a = [=] { contract_assert(m > 0); }; // expected-error {{implicit capture of 'this' is not allowed when used exclusively in contract assertions}}
    // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of 'this' is required here}}
    // expected-warning@-2 {{implicit capture of 'this' with a capture default of '=' is deprecated}} expected-note@-2 {{add an explicit capture of 'this' to capture '*this' by reference}}
    auto b = [&] { contract_assert(m > 0); }; // expected-error {{implicit capture of 'this' is not allowed when used exclusively in contract assertions}}
    // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of 'this' is required here}}
    auto c = [&] pre(this->m > 0) {}; // expected-error {{implicit capture of 'this' is not allowed when used exclusively in contract assertions}}
    // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of 'this' is required here}}
    auto d = [this] pre(m > 0) {}; // OK
    auto e = [*this] pre(m > 0) {}; // OK
    auto f = [&] { contract_assert(m > 0); return m; }; // OK
    auto g = [&] { contract_assert([&] { return m > 0; }()); }; // expected-error {{implicit capture of 'this' is not allowed when used exclusively in contract assertions}}
    // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of 'this' is required here}}
    auto h = [&](auto x) { contract_assert(m > x); }; // expected-error {{implicit capture of 'this' is not allowed when used exclusively in contract assertions}}
    // expected-note@-1 {{within contract context introduced here}} expected-note@-1 {{capture of 'this' is required here}}
    h(1);
  }
};
