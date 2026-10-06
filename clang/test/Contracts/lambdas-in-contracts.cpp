// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// Lambdas in the contracts of a non-member function can capture its
// parameters.
int f1(const int x) pre([x] { return x > 0; }()) { return x; }
int f2(const int x) pre([&] { return x > 0; }()) post([=] { return x > 0; }()) { return x; }
int f3(int x) pre([x] { return x > 0; }());
int f3(int x) { return x; }
constexpr int f4(const int x) pre([&] { return x > 0; }()) { return x; }
static_assert(f4(1) == 1);
constexpr int bad4 = f4(-1); // expected-error {{constexpr variable 'bad4' must be initialized by a constant expression}}
// expected-note@-1 {{in call to}}
// expected-note@9 {{contract violation with the 'enforce' evaluation semantic: precondition '[&] { return x > 0; }()' evaluated to false}}

// Lambdas in contracts can't implicitly capture without a capture-default.
void f5(int x) pre([] { return x > 0; }()); // expected-error {{variable 'x' cannot be implicitly captured in a lambda with no capture-default specified}} \
  // expected-note {{lambda expression begins here}} expected-note 4 {{capture}}
// expected-note@-2 {{'x' declared here}}

// Contracts of lambdas within templates are instantiated with the lambda.
template <class T> constexpr int t(T v) {
  auto l = [](T x) pre(x > 0) { return x; }; // expected-note {{contract violation with the 'enforce' evaluation semantic: precondition 'x > 0' evaluated to false}}
  return l(v); // expected-note {{in call to}}
}
static_assert(t(1) == 1);
constexpr int badt = t(-1); // expected-error {{constexpr variable 'badt' must be initialized by a constant expression}}
// expected-note@-1 {{in call to}}

// FIXME: Capturing the result binding is not supported yet.
int r1() post(r: [r] { return r > 0; }()) { return 1; } // expected-error 2 {{capturing result name 'r' in a lambda expression or block is not supported yet}}
int r2() post(r: [&] { return r > 0; }()) { return 1; } // expected-error {{capturing result name 'r' in a lambda expression or block is not supported yet}}
int r3() post(r: [] { return sizeof(decltype((r))) > 0; }()) { return 1; } // OK, not odr-used.

// [expr.prim.lambda.closure]: An entity implicitly captured only because of
// uses within contract assertions makes the program ill-formed, also when the
// lambda is instantiated.
namespace ImplicitCaptureInTemplates {
extern int yy;
void foo(auto p) {
  int local = 202 + p;
  [&](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert(p2 && p && local); // expected-note 2 {{contract context}} expected-note 2 {{required here}}
    return p2;
  }(p);
}
int x = (foo(42), 1); // expected-note {{here}}

auto tmpl_lambda = [](auto p) {
  int local = 202 + p;
  return [&](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert(p2 && p && local); // expected-note 2 {{contract context}} expected-note 2 {{required here}}
    return p2;
  }(p);
}(yy); // expected-note {{here}}
} // namespace ImplicitCaptureInTemplates
