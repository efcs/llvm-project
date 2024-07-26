// ADDITIONAL_COMPILE_FLAGS: -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -Xclang -fcontract-group-evaluation-semantic=enforce=enforce -fcontract-group-evaluation-semantic=quick_enforce=quick_enforce -fcontract-group-evaluation-semantic=ignore=ignore -fcontract-group-evaluation-semantic=observe=observe
#include <cassert>
#include <contracts>
#include <iostream>
#include <vector>
#include <compare>
#include <source_location>
#include <utility>
#include <tuple>
#include <fstream>
#include <unordered_map>
#include <format>
#include <vector>
#include <map>
#include <variant>
#include <regex>
#include <string>
#include <stdexcept>
#include <set>
#include "check_assertion.h"
#include "dump_struct.h"
#include "contracts_support.h"

namespace self_test {
  void do_self_test() {
    ObjectInfo *info = AT::get_info();
    info->massert({.alive = 0, .constructed = 0, .destroyed = 0});
    {
    AT a;
    info->massert({.alive = 1, .constructed = 1, .destroyed = 0});
    }
    info->massert({.alive = 0, .constructed = 1, .destroyed = 1});

  }
}
#endif

int main() {
  Checker->expect()
  ObjectInfo info;

  foo(0, &info);
  c1.massert({.evaluated = 2, .violated = 1});

  test_post(&info);
}