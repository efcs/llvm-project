// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected -verify-ignore-unexpected=warning %s -fcontracts

// expected-no-diagnostics

template <bool B>
struct BoolConstant {
  enum { value = B };
  constexpr BoolConstant() = default;
  constexpr operator bool() const { return value; }
};
using TrueT = BoolConstant<true>;
using FalseT = BoolConstant<false>;
template <class T>
struct BoomT {
  using BIG_BANG = typename T::THIS_DOES_NOT_EXIST;
};
template <class T>
using Boom = typename BoomT<T>::LIGHT_FUSE;

namespace Traits {

template <class ...Args>
concept f_callable = requires(Args... args) {
  { f(args...) };
};

template <class T, class ...Args>
concept mem_f_callable = requires(T t, Args... args) {
  {  t.f(args...) };
};

template <class T, class ...Args>
concept obj_callable = requires(T t, Args... args) {
  { t(args...) };
};

template <class T, class U>
concept same_as_helper = __is_same(T, U);

template <class T, class U>
concept same_as = same_as_helper<T, U> && same_as_helper<U, T>;


} // end namespace traits

#define DEF_OVL_FALSE(name) FalseT name(...);
#define ASSERT_OVL_TRUE(...) static_assert(decltype(__VA_ARGS__)::value);
#define ASSERT_OVL_TRUE(...) static_assert(decltype(__VA_ARGS__)::value);
#define OVL_RETURNS(...) decltype(__VA_ARGS__)::value
template <template <class ...> class C, class ...Ts>
concept satisfies_trait = requires {
  typename C<Ts...>;
};

namespace basic_test {
  FalseT f(...);

  static_assert(OVL_RETURNS(f(TrueT{})) == false);

  template <class T>
  T f(T x) pre(Boom<T>::value) post(Boom<T>::value) {
    return x;
  }

  static_assert(OVL_RETURNS(f(TrueT{})) == true); // doesn't cause instantiation
  // doesn't evaluate contracts
}

namespace on_member_test {
struct A {
  template <class T>
  T f(T x) const pre(Boom<T>::value) post(Boom<T>::value) {
    return x;
  }

  FalseT f(...) const;
};
template <class U>
struct B {
  FalseT f(...) const;
  FalseT g(...) const;

  template <class T>
  T f(T x) const pre(Boom<T>::value) post(Boom<T>::value) {
    return x;
  }

  U g(U x) const pre(Boom<U>::value) post(Boom<U>::value);
};

static_assert(OVL_RETURNS(A{}.f(TrueT{})) == true);
static_assert(OVL_RETURNS(B<int>{}.f(TrueT{})) == true);

}