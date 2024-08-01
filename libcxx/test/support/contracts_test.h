#include <map>
#include <string>
#include <cassert>
#include <tuple>
#include <vector>
#include <exception>
#include <format>
#include <functional>
#include <iostream>

#include "test_macros.h"
#include "nttp_string.h"

#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused"
#pragma GCC diagnostic ignored "-Wunused-template"
#pragma GCC diagnostic ignored "-Wunused-parameter"

#define STR2(x) #x
#define STR(x) STR2(x)
#define CONCAT1(x, y) x##y
#define CONCAT(x, y) CONCAT1(x, y)

#define REGISTER_TEST(test) static ::TestRegistrar ANON_VAR(test) = []()

#define ANON_VAR(id) CONCAT(anon_var_, CONCAT(id, __LINE__))

#define COUNTERS_EQ(list, ...) assert(eq(list, {__VA_ARGS__}))

std::map<std::string, int>& GetCounterStore() {
  static std::map<std::string, int> CounterStore;
  return CounterStore;
}

template <TStr Key>
auto& KV = GetCounterStore()[Key.str()];

inline bool count(bool value) {
  KV<"AssertCounter"> += 1;
  return value;
}

template <class T, size_t... N>
auto array_to_tuple_impl(const T* arr, std::index_sequence<N...>) {
  return std::tuple{arr[N]...};
}

template <class T, size_t N>
auto array_to_tuple(T (&arr)[N]) {
  return array_to_tuple_impl(arr, std::make_index_sequence<N>{});
}

template <class T, size_t N>
auto array_to_tuple(const std::array<T, N>& arr) {
  return array_to_tuple_impl(arr.data(), std::make_index_sequence<N>{});
}
template <class... Args>
struct first_type;

template <class T, class... Args>
struct first_type<T, Args...> {
  using type = T;
  static_assert((std::is_same_v<T, Args> && ...));
};
template <class T>
struct first_type<T> {
  using type = T;
};
template <class... Args>
using first_type_t = typename first_type<Args...>::type;

template <class... Args>
std::vector<first_type_t<Args...>> tuple_to_vector(std::tuple<Args...> const& tuple) {
  return std::apply(
      [](auto... values) {
        std::vector<first_type_t<Args...>> result;
        (result.push_back(values), ...);
        return result;
      },
      tuple);
}

template <size_t N, class T>
auto initlist_to_tuple(std::initializer_list<T> il) {
  assert(il.size() == N);
  std::array<T, N> arr = {};
  std::copy(il.begin(), il.end(), arr.begin());
  return array_to_tuple(arr);
}

template <class T>
std::string vector_to_str(std::vector<T> const& V) {
  std::string tmp = "[";
  bool first      = true;
  for (auto vv : V) {
    if (!first) {
      tmp += ", ";
    }
    first = false;
    tmp += std::to_string(vv);
  }
  return tmp + "]";
}

template <class... Args>
inline bool eq(std::tuple<Args...>& list, std::initializer_list<int> il) {
  auto tup2           = initlist_to_tuple<sizeof...(Args)>(il);
  decltype(tup2) tup3 = list;
  bool result         = (list == tup2);
  if (!result) {
    std::string expect_str = vector_to_str(tuple_to_vector(tup2));
    std::string actual_str = vector_to_str(tuple_to_vector(tup3));

    std::cout << "Expected: " << expect_str << "\n";
    std::cout << "Actual:   " << actual_str << "\n";
    std::cout << std::endl;
  }
  return result;
}

template <class... Args>
inline void reset(std::tuple<Args&...> const& list) {
  int values[sizeof...(Args)] = {};
  list                        = array_to_tuple(values);
}

struct RegisteredTest {
  template <class Func>
  RegisteredTest(Func xtest) {
    Test = xtest;
  }

  void operator()() {
    assert(!executed);
    Test();
    executed = true;
  }

  RegisteredTest(RegisteredTest const&) = delete;
  RegisteredTest(RegisteredTest&&)      = delete;
  ~RegisteredTest() { assert(executed); }

  std::function<void()> Test;
  bool executed = false;
};

struct TestRegistrar {
  static std::vector<RegisteredTest*>& GetTests() {
    static std::vector<RegisteredTest*> TestRegistrars;
    return TestRegistrars;
  }

  static void RunTests() {
    std::cout << "Running " << GetTests().size() << " tests\n";
    assert(GetTests().size() > 0);
    for (auto* Reg : GetTests()) {
      (*Reg)();
    }
  }

  template <class Func>
  TestRegistrar(Func&& f) {
    test = new RegisteredTest(std::forward<Func>(f));
    GetTests().push_back(test);
  }

  TestRegistrar(TestRegistrar const&) = delete;

  ~TestRegistrar() { assert(test->executed); }

  RegisteredTest* test = nullptr;
};

struct CounterStoreT {
  decltype(auto) get() { return GetCounterStore(); }

  decltype(auto) operator[](std::string_view key) { return get()[std::string(key)]; }

  decltype(auto) at(std::string_view key) { return get().at(std::string(key)); }

  std::map<std::string, int>* operator->() { return &get(); }
};
constinit CounterStoreT CounterStore;

template <auto, class T>
using AsType = T;

template <TStr Key>
auto Counter = std::ref(CounterStore[Key.str()]);

template <TStr... Key>
auto CounterGroup = std::tuple<AsType<Key, int&>...>{GetCounterStore()[Key.str()]...};

struct AliveCounter {
  explicit AliveCounter(const char* key) : Counter(&CounterStore[key]) {
    assert(Counter && *Counter >= 0);
    *Counter += 1;
  }

  AliveCounter(nullptr_t) = delete;
  AliveCounter(void*)     = delete;

  constexpr AliveCounter(int* dest) : Counter(dest) {
    assert(Counter && *Counter >= 0);
    *Counter += 1;
  }

  constexpr AliveCounter(AliveCounter const& RHS) : Counter(RHS.Counter) {
    assert(Counter && *Counter >= 0);
    *Counter += 1;
  }

  ~AliveCounter() {
    assert(*Counter >= 1);
    *Counter -= 1;
  }

  int* Counter;
};

template <TStr Key>
struct CAliveCounter : private AliveCounter {
  CAliveCounter() : AliveCounter(&KV<Key>) {}
  CAliveCounter(CAliveCounter const& RHS) : AliveCounter(RHS) {}

  ~CAliveCounter() = default;
};
