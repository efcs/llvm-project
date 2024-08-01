#include <map>
#include <string>
#include <cassert>
#include <tuple>
#include <vector>
#include <exception>
#include <format>
#include <functional>
#include <iostream>
#include <contracts>
#include <source_location>
#include <set>

#include "test_macros.h"
#include "nttp_string.h"
#include "contracts_support.h"

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

