// RUN: %clang_cc1 -fcontracts -fcoroutines -std=c++26 -fsyntax-only %s -verify=expected

// An odr-use of a non-reference parameter in a postcondition of a coroutine
// is ill-formed.
//
// [dcl.contract.func] requires such a parameter to be const.
// [dcl.fct.def.coroutine]/5 creates the coroutine's parameter copies "at the
// beginning of the replacement body", and says the copy of a non-reference
// parameter of type CV T is direct-initialized from an xvalue of type T --
// the UNQUALIFIED type, which cannot be formed from a const parameter.  The
// two requirements cannot both be met, and the standard states the
// consequence outright, as a note on [dcl.fct.def.coroutine]:
//
//   "An odr-use of a non-reference parameter in a postcondition assertion of
//    a coroutine is ill-formed."
//
// So the non-const spelling is rejected by the const rule and the const
// spelling by this one; there is no third.  Use a reference parameter, whose
// frame copy [dcl.fct.def.coroutine]/5 binds to the same object.
//
// Mirror of gcc/testsuite/g++.dg/contracts/cpp26/coroutine-postcondition-param.C.
// The const spelling is the one easily missed: it satisfies
// [dcl.contract.func] and fails [dcl.fct.def.coroutine]/5, so a check that
// looks only at the postcondition rule lets it through.

// Note: boilerplate to make coroutines compile, matching coroutine-compat.cpp.

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
} // end of namespace std

template<typename Promise> struct coro {};
template <typename Promise, typename... Ps>
struct std::coroutine_traits<coro<Promise>, Ps...> {
  using promise_type = Promise;
};

struct suspend_always {
  bool await_ready() noexcept { return false; }
  template <typename F>
  void await_suspend(F) noexcept;
  void await_resume() noexcept {}
};

struct promise_base {
  suspend_always initial_suspend() noexcept { return {}; }
  suspend_always final_suspend() noexcept { return {}; }
  void unhandled_exception() {}
};

struct promise : promise_base {
  coro<promise> get_return_object() { return {}; }
  void return_void() {}
};

using Task = coro<promise>;

// Non-const by value gets BOTH diagnostics, and the pair is the useful
// answer: the general rule says it must be const, and this one says const
// would not rescue it either because the function is a coroutine.
Task non_const(int x) // expected-note {{parameter of type 'int' is declared here}} \
                       // expected-note {{function 'non_const' is a coroutine}}
    post(x > 0) {      // expected-error {{parameter 'x' referenced in contract postcondition must be declared const}} \
                       // expected-error {{postcondition of a coroutine cannot odr-use non-reference parameter 'x'}}
  co_return;
}

// Const by value: rejected because this is a coroutine.
Task by_value(const int x) // expected-note {{function 'by_value' is a coroutine}}
    post(x > 0) { // expected-error {{postcondition of a coroutine cannot odr-use non-reference parameter 'x'}}
  co_return;
}

// A reference parameter is fine -- its copy is bound to the same object.
Task by_ref(const int &x) post(x > 0) { co_return; }
Task by_mutable_ref(int &x) post(x > 0) { co_return; }

// A PREcondition may name a by-value parameter: the const rule, and so this
// restriction, are postcondition rules.
Task in_pre(int x) pre(x > 0) { co_return; }

// A const by-value parameter with no postcondition naming it is unaffected;
// the restriction is on the odr-use, not on the parameter.
Task const_unused(const int x) post(true) { co_return; }

// Naming the result binding rather than a parameter is fine.
Task result_only(int x) post(r : true) { co_return; }

// A non-coroutine with the same signature keeps working -- this is what
// makes the restriction coroutine-specific rather than a general tightening
// of the const rule.
int not_a_coroutine(const int x) post(x > 0) { return x; }

// An UNEVALUATED naming of a parameter is not an odr-use, so it does not
// trip this restriction either -- the same boundary the const rule has.  GCC
// rejected all three of these, because one flag drives both rules there and a
// decltype or requires-expression was wrongly marking the parameter as used.
// See
// Contracts/Sema/postcondition-unevaluated-operand.cpp for the rest of that
// family.
template <typename T> bool pred();
Task unevaluated_decltype(int x) post(pred<decltype(x)>()) { co_return; }
Task unevaluated_sizeof(int x) post(sizeof(x) > 0) { co_return; }
Task unevaluated_requires(int x) post(requires { +x; }) { co_return; }

// The same restriction inside a template, where the function is only known
// to be a coroutine once its body is parsed.  Deliberately not instantiated:
// the GCC mirror found that an instantiation repeats the same diagnostic at
// the same location, which is a diagnostic-quality wrinkle, not a second
// property worth pinning here.
template <class T>
Task templated(const T x) // expected-note {{function 'templated' is a coroutine}}
    post(x > 0) { // expected-error {{postcondition of a coroutine cannot odr-use non-reference parameter 'x'}}
  co_return;
}
