// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// An immediate-escalating expression ([expr.const]) in a contract assertion of
// an immediate-escalating function (the instantiation of a constexpr function
// template, or a lambda) makes the function an immediate function, just like
// one in its body; the note explaining why points into the contract assertion.

consteval int ce(int x) { return x; }

template <class T> constexpr int in_pre(T x) pre(ce(x) > 0) { return x; } // #in_pre
template <class T> constexpr int in_post(T x) post(r: ce(r) > 0) { return x; } // #in_post
template <class T> constexpr int in_assert(T x) { contract_assert(ce(x) > 0); return x; } // #in_assert
template <class T> constexpr int declared(T x) pre(ce(x) > 0); // #declared
template <class T> constexpr int declared(T x) { return x; }
template <class T> struct S {
  constexpr int m(T x) pre(ce(x) > 0) { return x; } // #m
};
template <class T> struct MT {
  template <class U> constexpr int m(U u) pre(ce(u) > 0) { return u; } // #mt
};
// The call to the lambda, which is an immediate function because of the call
// to 'ce' in its body, is the immediate-escalating expression.
template <class T> constexpr int lam(T x) pre([&] { return ce(x) > 0; }()) { return x; } // #lam
// The contracts of a specialization which is odr-used before its definition
// can be instantiated are instantiated at the odr-use, which makes the
// specialization an immediate function right there.
template <class T> constexpr int fwd(T x) pre(ce(x) > 0); // #fwd

static_assert(in_pre(1) == 1);
static_assert(in_post(1) == 1);
static_assert(in_assert(1) == 1);
static_assert(declared(1) == 1);
static_assert(S<int>().m(1) == 1);
static_assert(MT<int>().m(1) == 1);
static_assert(lam(1) == 1);

int runtime(int v) { // #runtime
  int a = in_pre(v); // expected-error {{call to immediate function 'in_pre<int>' is not a constant expression}} \
                     // expected-note@#in_pre {{'in_pre<int>' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}} \
                     // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                     // expected-note@#runtime {{declared here}}
  int b = in_post(v); // expected-error {{call to immediate function 'in_post<int>' is not a constant expression}} \
                      // expected-note@#in_post {{'in_post<int>' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}} \
                      // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                      // expected-note@#runtime {{declared here}}
  int c = in_assert(v); // expected-error {{call to immediate function 'in_assert<int>' is not a constant expression}} \
                        // expected-note@#in_assert {{'in_assert<int>' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}} \
                        // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                        // expected-note@#runtime {{declared here}}
  int d = declared(v); // expected-error {{call to immediate function 'declared<int>' is not a constant expression}} \
                       // expected-note@#declared {{'declared<int>' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}} \
                       // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                       // expected-note@#runtime {{declared here}}
  int e = S<int>().m(v); // expected-error {{call to immediate function 'S<int>::m' is not a constant expression}} \
                         // expected-note@#m {{'m' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}} \
                         // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                         // expected-note@#runtime {{declared here}}
  int f = [](int x) pre(ce(x) > 0) { return x; }(v); // expected-error {{call to immediate function 'runtime(int)::(lambda)::operator()' is not a constant expression}} \
                                                      // expected-note {{'operator()' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}} \
                                                      // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                                                      // expected-note@#runtime {{declared here}}
  int g = MT<int>().m(v); // expected-error {{call to immediate function 'MT<int>::m<int>' is not a constant expression}} \
                          // expected-note@#mt {{'m<int>' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}} \
                          // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                          // expected-note@#runtime {{declared here}}
  int h = lam(v); // expected-error {{call to immediate function 'lam<int>' is not a constant expression}} \
                  // expected-note@#lam {{'lam<int>' is an immediate function because its body evaluates the address of an immediate function 'operator()'}} \
                  // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                  // expected-note@#runtime {{declared here}}
  int i = fwd(v); // expected-error {{call to immediate function 'fwd<int>' is not a constant expression}} \
                  // expected-note@#fwd {{'fwd<int>' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}} \
                  // expected-note {{function parameter 'v' with unknown value cannot be used in a constant expression}} \
                  // expected-note@#runtime {{declared here}}
  return a + b + c + d + e + f + g + h + i;
}
template <class T> constexpr int fwd(T x) { return x; }
static_assert(fwd(2) == 2);

int (*fp)(int) = in_pre<int>; // expected-error {{cannot take address of immediate function 'in_pre<int>' outside of an immediate invocation}} \
                              // expected-note@#in_pre {{declared here}} \
                              // expected-note@#in_pre {{'in_pre<int>' is an immediate function because its body contains a call to a consteval function 'ce' and that call is not a constant expression}}

// A non-template constexpr function is not immediate-escalating: the immediate
// invocation must be a constant expression.
constexpr int plain(int x) pre(ce(x) > 0) { return x; } // expected-error {{call to consteval function 'ce' is not a constant expression}} \
                                                        // expected-note {{function parameter 'x' with unknown value cannot be used in a constant expression}} \
                                                        // expected-note {{declared here}}
