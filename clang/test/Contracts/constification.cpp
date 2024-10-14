// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts

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
    ++x && // expected-error {{it is considered 'const'}}
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
  ++v, // expected-error {{it is considered 'const'}}
  ++v2, // expected-error {{it is considered 'const' inside of a contract}}
  ++v3  // expected-error {{const-qualified type 'const int'}}
  ));
  contract_assert((assert_same<decltype(v), T>(), true));
  contract_assert((assert_same<decltype(v2), int>(), true));
  contract_assert((assert_same<decltype(v3), const int>(), true));
}
template int foo(int); // expected-note {{requested here}}

namespace DecompDecl {
struct A {
  int a = 101;
  int b = 42;
};
void f(A& x) {
  auto [a, b] = x;
  contract_assert(++a);
}

}

namespace ModifiedIntReturnValue {

template <class T>
auto bar(T x) post(r : ++r != 42) { // expected-error {{cannot assign to variable 'r' because it is considered 'const' inside of a contract}}
return x;
}
template auto bar(int); // expected-note {{requested here}}
}

namespace NonStaticMembers {

bool eat(int&);
struct S {
  bool f(); // expected-note 1+ {{'f' declared here}}
  void g()
  pre(this->f()) // expected-error {{'this' argument to member function 'f' has type 'const NonStaticMembers::S', but function is not marked const}}
  // expected-note@-1 {{'this' is 'const' within contract introduced here}}
  pre(++m) // expected-error {{considered 'const'}}
  post(this->f()) // expected-error {{'this' argument to member function 'f' has type 'const NonStaticMembers::S', but function is not marked const}}
  // expected-note@-1 {{'this' is 'const' within contract introduced here}}
  post(++m) // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
  {
    contract_assert(this->f()); // expected-error {{'this' argument to member function 'f' has type 'const NonStaticMembers::S', but function is not marked const}}
    contract_assert(++m); // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
  }
  int m;
};
template <class T>
struct ST {
  bool f(); // expected-note 1+ {{'f' declared here}}
  void g()
  pre(this->f()) // expected-error {{'this' argument to member function 'f' has type 'const NonStaticMembers::ST<int>', but function is not marked const}}
  pre(++m) // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
  post(this->f()) // expected-error {{'this' argument to member function 'f' has type 'const NonStaticMembers::ST<int>', but function is not marked const}}
  post(++m) // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
  {
    contract_assert(this->f()); // expected-error {{'this' argument to member function 'f' has type 'const NonStaticMembers::ST<int>', but function is not marked const}}
    contract_assert(++m); // expected-error {{cannot assign to non-static data member because it is considered 'const' inside of a contract}}
  }
  T m;
};
template struct ST<int>; // expected-note {{in instantiation of member function 'NonStaticMembers::ST<int>::g' requested here}}

}

namespace DeclTypeHasConst {
  template <class T, class U>
  struct AssertSame;

  template <class T>
  struct AssertSame<T, T> {
    enum { value = 1 };
  };

void f(int p) {
  contract_assert(AssertSame<decltype(p), int>::value);
  contract_assert(AssertSame<decltype((p)), const int&>::value);

}
}