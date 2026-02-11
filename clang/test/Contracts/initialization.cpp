
// RUN: %clang_cc1 -std=c++26 -fcontracts  %s -fcolor-diagnostics -verify=expected

#define UNIQUE1() __COUNTER__
#define UNIQUE UNIQUE1()
template <class T, class U>
struct FirstTypeT {
  using type = T;
};
template <class T, class U>
using FirstType = typename FirstTypeT<T, U>::type;

template <bool B, class T = void, int N = 0>
struct EnableIf { static_assert(B);  using type = T; };
template <class T, int N>
struct EnableIf<false, T, N> {};

template <bool B, class T = void>
using EnableIfT = typename EnableIf<B, T>::type;

template <class Ret, class T>
constexpr T f(T x)
  pre(T::type) { // expected-error 1+ {{'::'}}
  return 0;
}
template<typename T> int not_constexpr() pre(T::error) { return T::error; }
template<typename T> constexpr int is_constexpr() pre(T::error) { return T::error; } // expected-error 2 {{'::'}}


template<typename T, class Boom = EnableIf<false, T, UNIQUE >> int not_constexpr_var = f<Boom>(0);
template<typename T, class Boom = EnableIf<false, T, UNIQUE >> constexpr int is_constexpr_var = f<Boom>(0); // expected-note {{here}}

template<typename T, class Boom = EnableIf<false, T, UNIQUE >> const int is_const_var = f<Boom>(0); // expected-note {{here}}
template<typename T, class Boom = EnableIf<false, T, UNIQUE >> const volatile int is_const_volatile_var = f<Boom>(0);
template<typename T, class Boom = EnableIf<false, T, UNIQUE >> T is_dependent_var = f<Boom>(T{0}); // expected-note {{here}}
int var = 0;
template<typename T, class Boom = EnableIf<false, T, UNIQUE >> int &is_reference_var = (f<Boom>(0), var); // expected-note {{here}}
template<typename T, class Boom = EnableIf<false, T, UNIQUE >> float is_float_var = f<Boom>(0.0);

void test() {
  // Do not instantiate functions referenced in unevaluated operands...
  (void)sizeof(not_constexpr<long>());
  (void)sizeof(is_constexpr<long>());
  (void)sizeof(not_constexpr_var<long>);
  (void)sizeof(is_constexpr_var<long>);
  (void)sizeof(is_const_var<long>);
  (void)sizeof(is_const_volatile_var<long>);
  (void)sizeof(is_dependent_var<long>);
  (void)sizeof(is_dependent_var<const long>);
  (void)sizeof(is_reference_var<long>);
  (void)sizeof(is_float_var<long>);

  // ... but do if they are potentially constant evaluated, and refer to
  // constexpr functions or to variables usable in constant expressions.
  (void)sizeof(int{not_constexpr<int>()});
  (void)sizeof(int{is_constexpr<int>()}); // expected-note {{here}}
  (void)sizeof(int{not_constexpr_var<int>});
  (void)sizeof(int{is_constexpr_var<int>}); // expected-note 1+ {{here}}
  (void)sizeof(int{is_const_var<int>}); // expected-note {{here}}
  (void)sizeof(int{is_const_volatile_var<int>});
  (void)sizeof(int{is_dependent_var<int>});
  (void)sizeof(int{is_dependent_var<const int>}); // expected-note {{here}}
  (void)sizeof(int{is_reference_var<int>}); // expected-note {{here}}
  (void)sizeof(int{is_float_var<int>}); // expected-error {{cannot be narrowed}} expected-note {{cast}}
}
