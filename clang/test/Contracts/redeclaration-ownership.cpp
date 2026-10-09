// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -Wall -Wextra %s
//
// The function contract assertions of a function are introduced by its first
// declaration ([dcl.contract.func]p3); a redeclaration either omits them or
// repeats them ([dcl.contract.func]p4). The predicates belong to the first
// declaration and refer to its parameters; a definition which omits the
// contracts does not get a copy of them.

namespace NoDuplicateDiagnostics {
// The postcondition of the first declaration is checked once: the definition
// which omits it is not re-diagnosed.
int f(int v) post(v > 0); // expected-error {{parameter 'v' referenced in contract postcondition must be declared const}}
// expected-note@-1 {{parameter of type 'int' is declared here}}
int f(int v) { return v; }

// A definition which repeats the (invalid) postcondition is not diagnosed
// again either.
int g(int v) post(v > 0); // expected-error {{parameter 'v' referenced in contract postcondition must be declared const}}
// expected-note@-1 {{parameter of type 'int' is declared here}}
int g(int v) post(v > 0) { return v; }

// A redeclaration which repeats a valid postcondition is checked as written.
int k(const int v) post(v > 0);
int k(const int v) post(v > 0) { return v; }

// [dcl.contract.func]p7: the parameter shall be const on all declarations,
// including those that omit the postcondition.
int h(const int v) post(v > 0); // expected-note {{parameter referenced in postcondition of first declaration here}}
int h(int v) { return v; } // expected-error {{parameter 'v' of type 'int' must be declared const because it is referenced in a postcondition of the function}}
} // namespace NoDuplicateDiagnostics

namespace RenamedParameters {
// The definition may rename the parameters and the result binding; the
// contracts of the first declaration apply to it, and the parameters they
// name count as used by the definition (no -Wunused-parameter).
int f(const int x, int unused) pre(x > 0) post(r : r > x);
int f(const int y, int) { return y + 1; }

struct S {
  int m;
  int g(const int x) pre(x > m) post(r : r > m);
};
int S::g(const int y) { return y; }

template <class T> T t(const T x) pre(x > T{}) post(r : r > x);
template <class U> U t(const U y) { return y + 1; }
int use_t = t(1);
} // namespace RenamedParameters

namespace LambdaInContract {
template <class F> bool call(F f, int v) { return f(v); }

// A lambda-expression within the predicate of the first declaration: its
// closure type is the one the definition evaluates, and the only one that
// is passed to the template.
int f(int x) pre(call([](int v) { return v > 0; }, x));
int f(int y) { return y; }

// The definition may repeat the lambda-expression; the closure type of the
// repetition is never used.
int g(int x) pre(call([](int v) { return v > 0; }, x));
int g(int y) pre(call([](int v) { return v > 0; }, y)) { return y; }

int use = f(1) + g(2);
} // namespace LambdaInContract

namespace std {
template <class... Args> struct void_t_imp { using type = void; };
template <class... Args> using void_t = typename void_t_imp<Args...>::type;
template <class T, class = void> struct traits_sfinae_base {};
template <class T> struct traits_sfinae_base<T, void_t<typename T::promise_type>> {
  using promise_type = typename T::promise_type;
};
template <class Ret, class... Args> struct coroutine_traits : public traits_sfinae_base<Ret> {};
template <class PromiseType = void> struct coroutine_handle {
  static coroutine_handle from_address(void *) noexcept;
  static coroutine_handle from_promise(PromiseType &promise);
};
template <> struct coroutine_handle<void> {
  template <class PromiseType> coroutine_handle(coroutine_handle<PromiseType>) noexcept;
  static coroutine_handle from_address(void *) noexcept;
  template <class PromiseType> static coroutine_handle from_promise(PromiseType &promise);
};
} // namespace std

namespace Coroutines {
struct suspend_always {
  bool await_ready() noexcept { return false; }
  template <typename F> void await_suspend(F) noexcept;
  void await_resume() noexcept {}
};
struct task {
  struct promise_type {
    task get_return_object();
    suspend_always initial_suspend() noexcept { return {}; }
    suspend_always final_suspend() noexcept { return {}; }
    void unhandled_exception() {}
    void return_value(int);
  };
};

// The postcondition of the first declaration odr-uses a non-reference
// parameter, which the definition of the coroutine is diagnosed for.
task f(const int x) post(r [[maybe_unused]] : x > 0); // expected-error {{postcondition of a coroutine cannot odr-use non-reference parameter 'x'}}
task f(const int y) { co_return y; } // expected-note {{function 'f' is a coroutine}}

task g(const int &x) post(r [[maybe_unused]] : x > 0);
task g(const int &y) { co_return y; }
} // namespace Coroutines
