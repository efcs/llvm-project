// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts -fno-late-parsed-contracts
// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts -flate-parsed-contracts




constexpr int f(int x) post(r : r != 0) { // expected-error {{contract failed}}
  return x;
}

constexpr int do_test() {
  f(1); // OK
  f(0); // expected-note {{in call}}
  return 42;
}

constexpr int anchor = do_test(); // expected-error {{must be initialized}}
// expected-note@-1 {{in call}}

// not constified.
int *y = nullptr;

int foo() {
  int x = 42;
  contract_assert(
    ++x && // expected-error {{read-only variable}}
    (y = &x)); // expected-error {{discards qualifiers}}

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
  const int v3 = v; // expected-note {{declared const here}}
  contract_assert((
  ++v, // expected-error {{read-only variable}}
  ++v2, // expected-error {{read-only variable}}
  ++v3  // expected-error {{const-qualified type 'const int'}}
  ));
  contract_assert((assert_same<decltype(v), T>(), true));
  contract_assert((assert_same<decltype(v2), int>(), true));
  contract_assert((assert_same<decltype(v3), const int>(), true));
}
template int foo(int); // expected-note {{requested here}}
