// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts -fexperimental-new-constant-interpreter



constexpr int f(int x) post(r : r != 0) {
  return x;
}

constexpr int do_test() {
  f(1); // OK
  f(0); // Not OK
  return 42;
}

int foo() {
  int x = 42;
  contract_assert(++x);
}

template <class T, class U>
constexpr inline bool is_same_v = false;

template <class T>
constexpr inline bool is_same_v<T, T> = true;


template <class T, class U>
constexpr void assert_same() {
  static_assert(is_same_v<T, U>);
}


template <typename T>
int foo(T v) {
  int v2 = v;
  const int v3 = v;
  contract_assert((
  ++v,
  ++v2,
  ++v3
  ));
  contract_assert((assert_same<decltype(v), T>(), true));
  contract_assert((assert_same<decltype(v2), int>(), true));
  contract_assert((assert_same<decltype(v3), const int>(), true));

}
template int foo(int);

constexpr int test = do_test();
