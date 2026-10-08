// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [basic.scope.contract]p2: the result-name-introducer of a postcondition
// shall not potentially conflict with a declaration whose target scope is the
// function parameter scope of the function or, for a lambda-expression, its
// lambda scope -- that is, with its parameters or (for a lambda) its
// init-captures. The latter used to be accepted, the result name hiding the
// init-capture.

auto a = [r = 1]() post(r: r > 0) { return r; }; // expected-error {{declaration of result name 'r' conflicts with init-capture}} \
                                                 // expected-note {{previous declaration is here}}
auto b = [&r = a]() post(r: true) { return 1; }; // expected-error {{declaration of result name 'r' conflicts with init-capture}} \
                                                 // expected-note {{previous declaration is here}}
auto c = [x = 1, r = 2](const int y) post(r: r > y) { return r; }; // expected-error {{declaration of result name 'r' conflicts with init-capture}} \
                                                                   // expected-note {{previous declaration is here}}
auto d = [r = 1](const auto x) post(r: r > x) { return r; }; // expected-error {{declaration of result name 'r' conflicts with init-capture}} \
                                                             // expected-note {{previous declaration is here}}
auto e = [r = 1](const int x) -> int post(r: r > x) { return r; }; // expected-error {{declaration of result name 'r' conflicts with init-capture}} \
                                                                   // expected-note {{previous declaration is here}}
auto p = [](int r) post(r: r > 0) { return r; }; // expected-error {{declaration of result name 'r' shadows parameter}} \
                                                 // expected-note {{previous declaration is here}}

// A simple-capture does not declare anything in the lambda scope, and the
// init-captures and parameters of an enclosing lambda or function are in
// other scopes: the result name merely hides them.
void f(int x) {
  auto l = [x]() post(r: r > x) { return x; };
  (void)l;
}
auto g = [r = 1]() { return [](const int y) post(r: r > y) { return y; }(1) + r; };
auto h = [r = 1]() { return [r](const int y) post(r: r > y) { return y; }(1); };
auto i = [r = 1]() { return [&r](const int y) post(r: r > y) { return y; }(1); };
void j(int r) {
  auto l = [](const int y) post(r: r > y) { return y; };
  auto m = [r](const int y) post(r: r > y) { return y; };
  (void)l; (void)m;
}
auto k = [](const int y) post(r: [r = 2] { return r > 1; }()) { return y; };
