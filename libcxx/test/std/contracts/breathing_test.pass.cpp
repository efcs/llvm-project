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

struct ObjectInfoBase {
  int alive       = 0;
  int constructed = 0;
  int destroyed   = 0;
};

struct ObjectInfoArg {
  int alive       = -1;
  int constructed = -1;
  int destroyed   = -1;
};

struct ObjectInfo : ObjectInfoBase {
  std::source_location created_loc;

  ObjectInfo(std::source_location loc = std::source_location()) : created_loc(loc) {}

  ObjectInfo* reset() {
    alive       = 0;
    constructed = 0;
    destroyed   = 0;
    return this;
  }

  ObjectInfo* construct() {
    ++alive;
    ++constructed;
    return this;
  }

  ObjectInfo* destroy() {
    --alive;
    ++destroyed;
    return this;
  }

  std::string to_string() const {
    std::string tmp = dump_struct(static_cast<const ObjectInfoBase*>(this));
    if (created_loc.file_name() != nullptr) {
      tmp += std::format(
          "    location: {}:{} in {}\n", created_loc.file_name(), created_loc.line(), created_loc.function_name());
    }
    return tmp;
  }

  void do_assertion(std::string msg, std::source_location loc, bool should_exit = true) const {
    std::string tmp;
    if (loc.file_name()) {
      tmp += std::format("{}:{} in {}\n", loc.file_name(), loc.line(), loc.function_name());
    }
    tmp += std::format("Assertion failed: {}\n", msg);
    tmp += to_string();
    std::cerr << tmp << std::endl;
    if (should_exit)
      std::exit(1);
  }

  const ObjectInfo* assert_alive(int N, SLOC(loc)) const {
    if (alive != N) {
      do_assertion(std::format("assert_alive({})", N), loc);
    }
    return this;
  }

  const ObjectInfo* assert_constructed(int N, SLOC(loc)) const {
    if (constructed != N) {
      do_assertion(std::format("assert_constructed({})", N), loc);
    }
    return this;
  }
  const ObjectInfo* assert_destroyed(int N, SLOC(loc)) const {
    if (destroyed != N) {
      do_assertion(std::format("assert_destroyed({})", N), loc);
    }
    return this;
  }

  const ObjectInfo* massert(ObjectInfoArg info, SLOC(loc)) const {
    return massert(info.alive, info.constructed, info.destroyed, loc);
  }

  bool check(ObjectInfoArg info, SLOC(loc)) const { return check(info.alive, info.constructed, info.destroyed, loc); }

  bool check(int xalive, int xconstructed = -1, int xdestroyed = -1, SLOC(loc)) const {
    bool result = true;
    if (xalive != -1 && alive != xalive) {
      result = false;
    }
    if (xconstructed != -1 && constructed != xconstructed) {
      result = false;
    }
    if (xdestroyed != -1 && destroyed != xdestroyed) {
      result = false;
    }
    if (!result) {
      do_assertion("check() failed", loc, false);
    }
    return result;
  }

  const ObjectInfo* massert(int xalive, int xconstructed = -1, int xdestroyed = -1, SLOC(loc)) const {
    if (xalive != -1) {
      assert_alive(xalive, loc);
    }
    if (xconstructed != -1) {
      assert_constructed(xconstructed, loc);
    }
    if (xdestroyed != -1) {
      assert_destroyed(xdestroyed, loc);
    }
    return this;
  }
};

struct TrackedObject {
  TrackedObject() : info(nullptr) {}

  explicit TrackedObject(ObjectInfo* xinfo) : info(xinfo) {
    if (info)
      info->construct();
  }

  TrackedObject(TrackedObject const& RHS) : info(RHS.info) {
    if (info)
      info->construct();
  }

  TrackedObject(TrackedObject&& RHS) : info(RHS.info) {
    if (info)
      info->construct();
  }

  ~TrackedObject() { info->destroy(); }

  ObjectInfo* info;
};

template <class BaseT>
struct TrackedType : TrackedObject {
  static ObjectInfo* get_info() {
    static ObjectInfo info_obj;
    return &info_obj;
  }

  TrackedType() : TrackedObject(get_info()) {}
  TrackedType(TrackedType const&)            = default;
  TrackedType(TrackedType&&)                 = default;
  TrackedType& operator=(TrackedType const&) = default;
  TrackedType& operator=(TrackedType&&)      = default;
  ~TrackedType()                             = default;
};

struct AT : TrackedType<AT> {};

#define FMT_FIELD(name) ::std::format("{}: {}", #name, name)
#if 0
template <int N = 4, class ...Args>
std::string join_and_indent(Args&&... args) {
  std::string indent(N, ' ');
  return (std::format("{}{}\n", indent, args) + ...);
}


inline void hash_combine_impl(size_t& seed, size_t value)
{
  seed ^= value + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

template <class ...Args>
inline size_t hash_combine(size_t first, Args... args) {
  size_t seed = first;
  (hash_combine_impl(seed, args), ...);
  return seed;
}

struct ObjectDatabaseKey {
  int version;
  void* key;

  auto operator<=>(const ObjectDatabaseKey& other) const {
    return std::tie(version, key) <=> std::tie(other.version, other.key);
  }
};

template <>
struct std::hash<::ObjectDatabaseKey> {
  size_t operator()(::ObjectDatabaseKey const& Obj) {
    return hash_combine(std::hash<int>{}(Obj.version), std::hash<void*>{}(Obj.key));
  }
};

struct ObjectDatabase {
private:
  ObjectDatabase() = default;
  ObjectDatabase(ObjectDatabase const&) = delete;
  ObjectDatabase(ObjectDatabase&&) = delete;
  ObjectDatabase& operator=(ObjectDatabase const&) = delete;

public:
  static ObjectDatabase *instance() {
    static ObjectDatabase *db = new ObjectDatabase();
    return db;
  }

public:
  int getCurrentVersion(void* key) {
    if (auto pos = version_map.find(key); pos != version_map.end()) {
      return pos->second;
    }
    return -1;
  }

  int incrementVersion(void* key) {
    int version      = getCurrentVersion(key);
    version_map[key] = version + 1;
    return version + 1;
  }

  ObjectDatabaseKey getCurrentKey(void* key) { return {getCurrentVersion(key), key}; }

  ObjectInfo* getObjectInfo(void* key, int version = -1) {
    if (version == -1)
      version = getCurrentVersion(key);
    assert(version >= 0);
    ObjectDatabaseKey k = {version, key};

    if (auto pos = object_map.find(k); pos != object_map.end()) {
      return &pos->second;
    }
    return nullptr;
  }

  void assertObjectDead(void* k, int version) {
    assert(version >= 0);
    ObjectDatabaseKey key = {version, k};
    if (auto pos = object_map.find(key); pos != object_map.end()) {
      assert(pos->second.alive == 0);
    }
  }

  ObjectInfo* createObject(void* key) {
    int version         = incrementVersion(key);
    ObjectDatabaseKey k = {version, key};
    assert(object_map.find(k) == object_map.end());
    return &object_map[k];
  }

  void clear() {
    version_map.clear();
    for (auto const & [k, v] : object_map) {
      if (v.alive != 0) {
        std::cerr << "Object "
      }
    }
    object_map.clear();
  }


private:
  std::unordered_map<void*, int> version_map;
  std::map<ObjectDatabaseKey, ObjectInfo> object_map;
};

struct ObjectDatabaseInstance {

  static ObjectDatabase *instance() {
    static ObjectDatabase db;
    return &db;
  }

  ObjectDatabase* operator->() {
    return instance();
  }
};


struct ObjectTrackerBase {
  ObjectTrackerBase() : info(info ? info->construct() : nullptr) {}
  ObjectTrackerBase(ObjectTrackerBase const& RHS) : info(RHS.info ? RHS.info->construct() : nullptr) {}
  ObjectTrackerBase(ObjectTrackerBase&& RHS) : info(RHS.info ? info->construct() : ) { RHS.info = nullptr; }

  ~ObjectTrackerBase() { if (info) info->destroy(); }

  bool enable = true;
};

#endif

ContractTester c1, c2, c3;

void foo(int x, ObjectInfo* inf) {
  inf->reset();
  TrackedObject obj(inf);
  auto OnFailure = [inf = inf]() { inf->assert_alive(1)->assert_destroyed(0); };
  c1.on_failure(OnFailure);
  contract_assert(c1(x != 0));
}

void test_post(ObjectInfo* inf) post((inf->massert({.alive = 0, .destroyed = 1}), true)) {
  inf->reset();
  TrackedObject obj(inf);
  inf->massert({.alive = 1, .destroyed = 0});
}

struct TrackedInt : TrackedObject {
  TrackedInt(int x, ObjectInfo* inf) : TrackedObject(inf), value(x) {}
  int value;
};

#if 0
namespace handler_tests {
auto test_loc = ContractLoc() + 1;
void test(int x) pre(x != 1) post(x != 1) { contract_assert(x > 1); }

void test_with_handler() {
  const ExpectedViolation locs[3] = {
    {pre, test_loc + 1},
    {cassert, test_loc + 3},
    {post, test_loc + 2},
};
Checker->expect_range(std::begin(locs), std::end(locs));
test(1);
Checker->assert_empty();
}

} // namespace handler_tests

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
  ObjectInfo info;
  foo(0, &info);
  c1.massert({.evaluated = 2, .violated = 1});

  test_post(&info);
}