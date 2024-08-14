// ADDITIONAL_COMPILE_FLAGS: -std=c++26 -Xclang -fcontracts -fcontract-evaluation-semantic=enforce -fcontract-group-evaluation-semantic=observe=observe,enforce=enforce -g

#include "nttp_string.h"
#include "contracts_support.h"
#include "test_register.h"
#include "contracts_handler.h"


namespace fn_template_test {
template <class T>
void f(T v) pre(++KV<"Pre">&& v != 1024) post(++KV<"Post">) pre(++KV<std::is_same_v<T, int> ? "Int" : "NotInt">) {
  contract_assert(++KV<"CS">);
}

REGISTER_TEST(fn_template_test) {
  auto CounterList = CounterGroup<"Pre", "Post", "CS", "Int", "NotInt">;
  reset(CounterList);
  f(42);
  COUNTERS_EQ(CounterList, 1, 1, 1, 1, 0);
  f(42ll);
  COUNTERS_EQ(CounterList, 2, 2, 2, 1, 1);
  f(42);
  COUNTERS_EQ(CounterList, 3, 3, 3, 2, 1);
};

} // namespace fn_template_test

namespace basic_member_test {

struct S {
  S() pre(++KV<"Pre">) post(++KV<"Post">) { contract_assert(++KV<"CS">); }

  void f() pre(++KV<"Pre">) post(++KV<"Post">) { contract_assert(++KV<"CS">); }

  template <class T>
  T tf(T v) pre(++KV<"Pre">) post(++KV<"Post">) pre(++KV<std::is_same_v<T, int> ? "Int" : "NotInt">) {
    contract_assert(++KV<"CS">);
    return v;
  }

  ~S() pre(++KV<"Pre">) post(++KV<"Post">) { contract_assert(++KV<"CS">); }
};

template <class T>
struct C {
  C() pre(++KV<"Pre">) post(++KV<"Post">) { contract_assert(++KV<"CS">); }

  void f() pre(++KV<"Pre">) post(++KV<"Post">) pre(++KV<std::is_same_v<T, int> ? "Int" : "NotInt">) {
    contract_assert(++KV<"CS">);
  }

  template <class U>
  U tf(U v) pre(++KV<"Pre">) post(++KV<"Post">) pre(++KV<std::is_same_v<T, int> ? "Int" : "NotInt">)
      pre(++KV<std::is_same_v<U, int> ? "Int" : "NotInt">) {
    contract_assert(++KV<"CS">);
    return v;
  }

  ~C() pre(++KV<"Pre">) post(++KV<"Post">) pre(++KV<std::is_same_v<T, int> ? "Int" : "NotInt">) {
    contract_assert(++KV<"CS">);
  }
};

REGISTER_TEST(basic_test) {
  auto CounterList  = CounterGroup<"Pre", "Post", "CS">;
  auto TypeCounters = CounterGroup<"Int", "NotInt">;
  reset(CounterList);
  reset(TypeCounters);

  static_assert(std::is_same_v<decltype(CounterList), std::tuple<int&, int&, int&>>);
  {
    S s;
    COUNTERS_EQ(CounterList, 1, 1, 1);
    s.f();
    COUNTERS_EQ(CounterList, 2, 2, 2);
    COUNTERS_EQ(TypeCounters, 0, 0);
    s.tf(42);
    COUNTERS_EQ(TypeCounters, 1, 0);
    COUNTERS_EQ(CounterList, 3, 3, 3);
    reset(TypeCounters);
    s.tf("abc");
    COUNTERS_EQ(TypeCounters, 0, 1);
    COUNTERS_EQ(CounterList, 4, 4, 4);
  }
  COUNTERS_EQ(CounterList, 5, 5, 5);
};

REGISTER_TEST(template_test) {
  auto CounterList  = CounterGroup<"Pre", "Post", "CS">;
  auto TypeCounters = CounterGroup<"Int", "NotInt">;

  reset(CounterList);
  reset(TypeCounters);
  COUNTERS_EQ(CounterList, 0, 0, 0);
  {
    C<int> c;
    COUNTERS_EQ(CounterList, 1, 1, 1);
    c.f();
    COUNTERS_EQ(CounterList, 2, 2, 2);
    COUNTERS_EQ(TypeCounters, 1, 0);
    c.tf(42);
    COUNTERS_EQ(CounterList, 3, 3, 3);
    COUNTERS_EQ(TypeCounters, 3, 0);
    reset(TypeCounters);
    c.tf(42ll);
    COUNTERS_EQ(TypeCounters, 1, 1);
    COUNTERS_EQ(CounterList, 4, 4, 4);
  }
  COUNTERS_EQ(CounterList, 5, 5, 5);
  reset(CounterList);
  reset(TypeCounters);
  {
    C<long> c;
    COUNTERS_EQ(TypeCounters, 0, 0);
    c.f();
    COUNTERS_EQ(TypeCounters, 0, 1);
    reset(TypeCounters);
    c.tf(42);
    COUNTERS_EQ(TypeCounters, 1, 1);
    reset(TypeCounters);
    c.tf(42ll);
    COUNTERS_EQ(TypeCounters, 0, 2);
  }
};

} // namespace basic_member_test

namespace member_lifetime_test {

struct S : CAliveCounter<"Base"> {
  using Base = CAliveCounter<"Base">;

  S(CAliveCounter<"Param">)
  pre(eq(CounterGroup<"Base", "Mem", "Param">, {0, 0, 1})) post(eq(CounterGroup<"Base", "Mem", "Param">, {1, 1, 1})) {
    auto CounterList = CounterGroup<"Base", "Mem", "Param">;
    assert(eq(CounterList, {1, 1, 1}));
  }

  void f(CAliveCounter<"Param">, CAliveCounter<"Param">)
      pre(eq(CounterGroup<"Base", "Mem", "Param", "Local">, {1, 1, 2, 0}))
          post(eq(CounterGroup<"Base", "Mem", "Param", "Local">, {1, 1, 2, 0})) {
    auto CounterList = CounterGroup<"Mem", "Base", "Param", "Local">;
    assert(eq(CounterList, {1, 1, 2, 0}));
    CAliveCounter<"Local"> Local;

    assert(eq(CounterList, {1, 1, 2, 1}));

    {
      CAliveCounter<"Local"> Local2;
      assert(eq(CounterList, {1, 1, 2, 2}));
    }
    assert(eq(CounterList, {1, 1, 2, 1}));
  }

  ~S() pre(eq(CounterGroup<"Base", "Mem">, {1, 1})) post(eq(CounterGroup<"Base", "Mem">, {0, 0})) {
    auto CounterList = CounterGroup<"Base", "Mem">;
    assert(eq(CounterList, {1, 1}));
  }

  CAliveCounter<"Mem"> Mem;
};

REGISTER_TEST(lifetime_test) {
  auto CounterList = CounterGroup<"Base", "Mem", "Param", "Local">;
  reset(CounterList);
  COUNTERS_EQ(CounterList, 0, 0, 0, 0);
  {
    assert(KV<"Param"> == 0);
    S s({});

    COUNTERS_EQ(CounterList, 1, 1, 0, 0);
    s.f({}, {});
    COUNTERS_EQ(CounterList, 1, 1, 0, 0);
  }
  COUNTERS_EQ(CounterList, 0, 0, 0, 0);
};

} // namespace member_lifetime_test

namespace order_test {
auto& OrdC = KV<"OrderCounter">;

void foo() pre(OrdC++ == 0) pre(OrdC++ == 1) post(OrdC++ == 4) pre(OrdC++ == 2) post(OrdC++ == 5) {
  contract_assert(OrdC++ == 3);
}
template <class T>
void ft(T) pre(OrdC++ == 0) pre(OrdC++ == 1) post(OrdC++ == 4) pre(OrdC++ == 2) post(OrdC++ == 5) {
  contract_assert(OrdC++ == 3);
}

REGISTER_TEST(order_test) {
  OrdC = 0;
  foo();
  assert(OrdC == 6);
  OrdC = 0;
  ft(42);
  assert(OrdC == 6);
  OrdC = 0;
};

} // namespace order_test
