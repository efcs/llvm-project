// RUN: %clang_cc1  -fcontracts -fcoroutines -std=c++26 -fsyntax-only %s -verify=expected

// Note: There's a ton of boilerplate code here to make the test compile.

namespace std {
template <class... Args>
struct void_t_imp {
  using type = void;
};
template <class... Args>
using void_t = typename void_t_imp<Args...>::type;

template <class T, class = void>
struct traits_sfinae_base {};

template <class T>
struct traits_sfinae_base<T, void_t<typename T::promise_type>> {
  using promise_type = typename T::promise_type;
};

template <class Ret, class... Args>
struct coroutine_traits : public traits_sfinae_base<Ret> {};
} // end of namespace std

template<typename Promise> struct coro {};
template <typename Promise, typename... Ps>
struct std::coroutine_traits<coro<Promise>, Ps...> {
  using promise_type = Promise;
};

struct awaitable {
  bool await_ready() noexcept;
  template <typename F>
  void await_suspend(F) noexcept;
  void await_resume() noexcept;
} a;

struct suspend_always {
  bool await_ready() noexcept { return false; }
  template <typename F>
  void await_suspend(F) noexcept;
  void await_resume() noexcept {}
};

struct suspend_never {
  bool await_ready() noexcept { return true; }
  template <typename F>
  void await_suspend(F) noexcept;
  void await_resume() noexcept {}
};

struct auto_await_suspend {
  bool await_ready();
  template <typename F> auto await_suspend(F) {}
  void await_resume();
};

namespace std {
template <class PromiseType = void>
struct coroutine_handle {
  static coroutine_handle from_address(void *) noexcept;
  static coroutine_handle from_promise(PromiseType &promise);
};
template <>
struct coroutine_handle<void> {
  template <class PromiseType>
  coroutine_handle(coroutine_handle<PromiseType>) noexcept;
  static coroutine_handle from_address(void *) noexcept;
  template <class PromiseType>
  static coroutine_handle from_promise(PromiseType &promise);
};
} // namespace std


struct promise {
  void get_return_object();
  suspend_always initial_suspend();
  suspend_always final_suspend() noexcept;
  awaitable yield_value(int);
  void return_value(int);
  void unhandled_exception();
};


template <typename... T>
struct std::coroutine_traits<void, T...> { using promise_type = promise; };

void coreturn(int n) {
  contract_assert((co_await a, true)); // expected-error {{'co_await' cannot be used inside a contract}}
  contract_assert((co_yield 3, true)); // expected-error {{'co_yield' cannot be used inside a contract}}
}

template <class T>
void dependent_coawait(T n) {
  contract_assert((co_await n, true)); // expected-error {{'co_await' cannot be used inside a contract}}
  contract_assert((co_yield n, true)); // expected-error {{'co_yield' cannot be used inside a contract}}
}
