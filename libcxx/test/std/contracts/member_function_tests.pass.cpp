//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// UNSUPPORTED: c++03, c++11, c++14, c++17, c++20, c++23
// REQUIRES: clang, has-fcontracts
// XFAIL: availability-contracts-missing

// ADDITIONAL_COMPILE_FLAGS: -fcontracts -fcontract-evaluation-semantic=observe

// Check that function contract assertions on constructors, destructors, member
// functions, member function templates and members of class templates are
// evaluated the expected number of times, in the expected order, and at the
// expected point of the lifetime of the object.

#include <cassert>
#include <contracts>
#include <type_traits>
#include <vector>

#include "contract_violation_recorder.h"

enum { Pre, Post, Assert, Int, NotInt, Base, Mem, Param, Local };

template <class T>
constexpr int type_id = std::is_same_v<T, int> ? Int : NotInt;

// Increments counter `Id` while alive.
template <int Id>
struct AliveCounter {
  AliveCounter() { ++counters()[Id]; }
  AliveCounter(const AliveCounter&) { ++counters()[Id]; }
  ~AliveCounter() { --counters()[Id]; }
};

inline bool alive(int base, int mem, int param, int local = 0) {
  return count(Base) == base && count(Mem) == mem && count(Param) == param && count(Local) == local;
}

inline bool counters_eq(int pre, int post, int assertion) {
  return count(Pre) == pre && count(Post) == post && count(Assert) == assertion;
}

namespace basic_member_test {

struct S {
  S() pre(hit(Pre)) post(hit(Post)) { contract_assert(hit(Assert)); }

  void f() pre(hit(Pre)) post(hit(Post)) { contract_assert(hit(Assert)); }

  template <class T>
  T tf(T v) pre(hit(Pre)) post(hit(Post)) pre(hit(type_id<T>)) {
    contract_assert(hit(Assert));
    return v;
  }

  ~S() pre(hit(Pre)) post(hit(Post)) { contract_assert(hit(Assert)); }
};

template <class T>
struct C {
  C() pre(hit(Pre)) post(hit(Post)) { contract_assert(hit(Assert)); }

  void f() pre(hit(Pre)) post(hit(Post)) pre(hit(type_id<T>)) { contract_assert(hit(Assert)); }

  template <class U>
  U tf(U v) pre(hit(Pre)) post(hit(Post)) pre(hit(type_id<T>)) pre(hit(type_id<U>)) {
    contract_assert(hit(Assert));
    return v;
  }

  ~C() pre(hit(Pre)) post(hit(Post)) pre(hit(type_id<T>)) { contract_assert(hit(Assert)); }
};

void test() {
  reset_counters();
  {
    S s;
    assert(counters_eq(1, 1, 1));
    s.f();
    assert(counters_eq(2, 2, 2));
    assert(count(Int) == 0 && count(NotInt) == 0);
    s.tf(42);
    assert(count(Int) == 1 && count(NotInt) == 0);
    assert(counters_eq(3, 3, 3));
    s.tf("abc");
    assert(count(Int) == 1 && count(NotInt) == 1);
    assert(counters_eq(4, 4, 4));
  }
  assert(counters_eq(5, 5, 5));

  reset_counters();
  {
    C<int> c;
    assert(counters_eq(1, 1, 1));
    c.f();
    assert(counters_eq(2, 2, 2));
    assert(count(Int) == 1 && count(NotInt) == 0);
    c.tf(42);
    assert(counters_eq(3, 3, 3));
    assert(count(Int) == 3 && count(NotInt) == 0);
    c.tf(42LL);
    assert(counters_eq(4, 4, 4));
    assert(count(Int) == 4 && count(NotInt) == 1);
  }
  assert(counters_eq(5, 5, 5));
  assert(count(Int) == 5 && count(NotInt) == 1);

  reset_counters();
  {
    C<long> c;
    assert(count(Int) == 0 && count(NotInt) == 0);
    c.f();
    assert(count(Int) == 0 && count(NotInt) == 1);
    c.tf(42);
    assert(count(Int) == 1 && count(NotInt) == 2);
    c.tf(42LL);
    assert(count(Int) == 1 && count(NotInt) == 4);
  }
  assert(count(Int) == 1 && count(NotInt) == 5);
  assert(counters_eq(5, 5, 5));
}

} // namespace basic_member_test

namespace member_lifetime_test {

// Preconditions of a constructor are evaluated before the bases and members are
// initialized, and its postconditions after the constructor body. Preconditions
// of a destructor are evaluated before its body, and its postconditions after
// the bases and members have been destroyed.
struct S : AliveCounter<Base> {
  explicit S(AliveCounter<Param>) pre(alive(0, 0, 1)) post(alive(1, 1, 1)) { assert(alive(1, 1, 1)); }

  void f(AliveCounter<Param>, AliveCounter<Param>) pre(alive(1, 1, 2)) post(alive(1, 1, 2)) {
    assert(alive(1, 1, 2));
    AliveCounter<Local> l1;
    assert(alive(1, 1, 2, 1));
    {
      AliveCounter<Local> l2;
      assert(alive(1, 1, 2, 2));
    }
    assert(alive(1, 1, 2, 1));
    contract_assert(alive(1, 1, 2, 1));
  }

  ~S() pre(alive(1, 1, 0)) post(alive(0, 0, 0)) { assert(alive(1, 1, 0)); }

  AliveCounter<Mem> m;
};

void test() {
  reset_counters();
  violation_log().clear();
  {
    S s{AliveCounter<Param>{}};
    assert(alive(1, 1, 0));
    s.f({}, {});
    assert(alive(1, 1, 0));
  }
  assert(alive(0, 0, 0));
  assert(violation_log().size() == 0);
}

} // namespace member_lifetime_test

namespace order_test {

void foo() pre(seq(0)) pre(seq(1)) post(seq(4)) pre(seq(2)) post(seq(5)) { contract_assert(seq(3)); }

template <class T>
void ft(T) pre(seq(0)) pre(seq(1)) post(seq(4)) pre(seq(2)) post(seq(5)) {
  contract_assert(seq(3));
}

struct M {
  void mf() pre(seq(0)) pre(seq(1)) post(seq(4)) pre(seq(2)) post(seq(5)) { contract_assert(seq(3)); }
};

void test() {
  const std::vector<int> expected = {0, 1, 2, 3, 4, 5};

  sequence().clear();
  foo();
  assert(sequence() == expected);

  sequence().clear();
  ft(42);
  assert(sequence() == expected);

  sequence().clear();
  M().mf();
  assert(sequence() == expected);
}

} // namespace order_test

int main(int, char**) {
  violation_log().clear();
  basic_member_test::test();
  member_lifetime_test::test();
  order_test::test();
  assert(violation_log().size() == 0);
  return 0;
}
