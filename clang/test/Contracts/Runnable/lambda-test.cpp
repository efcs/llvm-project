// RUN: %clangxx -std=c++26 %s -fcontracts -o %t -fcontract-evaluation-semantic=observe
// RUN: %t
// RUN: %clangxx -std=c++26 %s -fcontracts -o %t -fcontract-evaluation-semantic=enforce
// RUN: %t

#include "my_assert.h"
#include "contracts-runtime.h"

const int *fz = nullptr;
constexpr int f(int x) pre([x=x](int y) { static int z(0);  z = x; fz = &z; return y > x; }(1000)) {
  return x;
}

template <class T>
const T* gz = nullptr;

template <class T>
constexpr T g(T x) pre([x=x](T y) { static T z(0); z = x;  gz<T> = &z; return y > x; }(1000)) {
  return x;
}
template int g(int);
template long g(long);

struct A {
  constexpr A() : z(0) {}

  int f(int x) pre([=,this](int y) { static A a; gz<A> = &a; a.z = z; return y > x; }(1000)) {
    return x;
  }

  int z = 0;
};


struct B {
  constexpr B() : z(0) {}

  int f(int x) pre([z=z]() { static B a; gz<B> = &a; a.z = z; return true; }()) {
    return x;
  }

  int z = 0;
};

int main() {
  int i = 101;
  long l = 42;
  assert(fz == nullptr);
  assert(f(i) == 101);
  assert(fz != nullptr);
  assert(gz<int> == nullptr);
  assert(gz<long> == nullptr);
  g(42);
  assert(gz<int> != nullptr);
  assert(gz<long> == nullptr);
  assert(*gz<int> == 42);

  A a;
  assert(gz<A> == nullptr);
  a.f(42);
  assert(gz<A> != nullptr);
  assert(gz<A>->z == 0);


}