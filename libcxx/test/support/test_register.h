#ifndef TEST_REGISTER_H
#define TEST_REGISTER_H

#include <iostream>
#include <vector>
#include <functional>
#include <cassert>


#define STR2(x) #x
#define STR(x) STR2(x)
#define CONCAT1(x, y) x##y
#define CONCAT(x, y) CONCAT1(x, y)
#define REGISTER_TEST(test) static ::TestRegistrar ANON_VAR(test) = []()
#define ANON_VAR(id) CONCAT(anon_var_, CONCAT(id, __LINE__))


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

int main() {
  TestRegistrar::RunTests();
}

#endif