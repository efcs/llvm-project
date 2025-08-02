#ifndef LIBCXX_TEST_CONTRACTS_SUPPORT_H
#define LIBCXX_TEST_CONTRACTS_SUPPORT_H

#include "test_macros.h"
#include <contracts>
#include <variant>
#include <optional>
#include <iostream>
#include <functional>
#include <string_view>
#include <tuple>
#include <regex>
#include <source_location>
#include <unordered_map>
#include <string>
#include <format>
#include <fstream>
#include <cassert>
#include <set>
#include <map>
#include "dump_struct.h"
#include "nttp_string.h"

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused"
#pragma GCC diagnostic ignored "-Wunused-template"
#pragma GCC diagnostic ignored "-Wunused-parameter"


#define COUNTERS_EQ(list, ...) assert(eq(list, {__VA_ARGS__}))
#define SLOC(name) std::source_location name = std::source_location::current()

using std::contracts::contract_violation;
using Semantic      = std::contracts::evaluation_semantic;
using AssertKind    = std::contracts::assertion_kind;
using DetectionKind = std::contracts::detection_mode;

constexpr AssertKind pre     = AssertKind::pre;
constexpr AssertKind post    = AssertKind::post;
constexpr AssertKind contract_assertion = AssertKind::assert;

constexpr Semantic observe = Semantic::observe;
constexpr Semantic enforce = Semantic::enforce;

constexpr DetectionKind failed    = DetectionKind::predicate_false;
constexpr DetectionKind exception = DetectionKind::evaluation_exception;

constexpr std::string_view enum_to_string(AssertKind K) {
  switch (K) {
  case pre:
    return "pre";
  case post:
    return "post";
  case AssertKind::assert:
    return "contract_assert";
  case AssertKind::manual:
    return "manual";
  case AssertKind::cassert:
  return "assert";
  case AssertKind::__unknown:
    return "<unknown>";
  }
}
constexpr std::string_view enum_to_string(Semantic S) {
  switch (S) {
  case observe:
    return "observe";
  case enforce:
    return "enforce";
  case Semantic::__unknown:
    return "<unknown>";
  }
}
constexpr std::string_view enum_to_string(DetectionKind D) {
  switch (D) {
  case failed:
    return "failed";
  case exception:
    return "exception";
  case DetectionKind::unspecified:
    return "unspecified";
  }
}

struct ContractLoc;

#define OBSERVE [[clang::contract_group("observe")]]
#define ENFORCE [[clang::contract_group("enforce")]]
#define QUICK_ENFORCE [[clang::contract_group("quick_enforce")]]
#define IGNORE [[clang::contract_group("ignore")]]


std::string to_string(std::source_location loc, bool add_newline = true) {
  std::string tmp = std::format("    {}:{}: ", loc.file_name(), loc.line());
  if (loc.function_name())
    tmp += std::format("in {}", loc.function_name());
  if (add_newline)
    tmp += '\n';
  return tmp;
}

std::string to_string(contract_violation const& vio) {
  std::string tmp = "Contract Violation: \n";
  tmp += to_string(vio.location());
  tmp += std::format(
      "    assertion_kind: {}\n    detection_kind: {}\n     semantic: {}\n     comment: \"{}\"\n",
      enum_to_string(vio.kind()),
      enum_to_string(vio.detection_mode()),
      enum_to_string(vio.semantic()),
      vio.comment());
  return tmp;
}

struct ContractLoc {
  int line = 0;
  std::string_view file;
  std::string_view function;

  using key_type = std::tuple<std::string_view, std::string_view, int>;

  constexpr key_type key() const { return {file, "", line}; }

  constexpr static key_type wildcard_key() { return {"*", "", 0}; }

  constexpr bool is_wildcard() const { return file == "*" && line == 0; }

  constexpr auto operator<=>(const ContractLoc& other) const {
    if (is_wildcard())
      return key() <=> wildcard_key();
    return key() <=> other.key();
  }

  constexpr auto operator<=>(std::source_location loc) const { return *this <=> ContractLoc(loc); }

  constexpr auto operator<=>(contract_violation const& vio) const { *this <=> ContractLoc(vio); }

  bool operator==(ContractLoc const&) const = default;
  bool operator<(ContractLoc const&) const  = default;

  std::string to_string() const {
    std::string tmp = std::format("{}:{}", file, line) + (function.empty() ? "" : std::format(" in {}", function));

    return tmp + "\n";
  }

  bool diagnose_mismatch(ContractLoc const& other) const {
    if (*this == other)
      return false;
    std::cerr << "Mismatch between expected and actual contract location:\n";
    if (file != other.file) {
      std::cerr << "    File mismatch: " << file << " != " << other.file << " (actual)" << std::endl;
      return true;
    }
    if (line != other.line) {
      std::cerr << "    Line mismatch: " << line << " != " << other.line << " (actual)" << std::endl;
      return true;
    }
    if (function != other.function && !function.empty() && !other.function.empty()) {
      std::cerr << "    Function mismatch: " << function << " != " << other.function << " (actual)" << std::endl;
      return true;
    }
    assert(false);
  }

  std::string get_source_line() const {
    if (file.empty()) {
      return "";
    }
    std::ifstream file_stream;
    file_stream.open(file.data());
    if (!file_stream.is_open()) {
      return "";
    }
    std::string ln;
    assert(line >= 0);
    for (int i = 0; i < line; ++i) {
      std::getline(file_stream, ln);
    }
    return ln;
  }

  constexpr ContractLoc with_offset(int line_offset) const {
    auto cp = *this;
    cp.line += line_offset;
    return cp;
  }
  explicit ContractLoc(std::string name, int offset = 0, SLOC(loc))
      : line(loc.line() + offset), file(loc.file_name()), function(loc.function_name()) {
    with_name(name);
  }
  constexpr explicit ContractLoc(SLOC(loc)) : line(loc.line()), file(loc.file_name()), function(loc.function_name()) {}
  constexpr explicit ContractLoc(unsigned line, const char* file, const char* func = "")
      : line(line), file(file ? file : ""), function(func ? func : "") {}
  constexpr explicit ContractLoc(const contract_violation& vio) : ContractLoc(vio.location()) {}

  ContractLoc with_name(std::string name) {
    auto pos = named_locs.find(name);
    if (pos != named_locs.end()) {
      if (pos->second == *this)
        return *this;
      std::cerr << "Name already exists: " << name << std::endl;
      assert(false);
      return *this;
    }
    named_locs[name] = *this;
    return *this;
  }

  static std::optional<ContractLoc> get_named(std::string name) {
    auto pos = named_locs.find(name);
    if (pos == named_locs.end()) {
      std::cerr << "Name not found: " << name << std::endl;
      return std::nullopt;
    }
    return pos->second;
  }

public:
  static std::unordered_map<std::string, ContractLoc> named_locs;
};

constexpr ContractLoc operator+(ContractLoc const& loc, int offset) { return loc.with_offset(offset); }
constexpr ContractLoc operator-(ContractLoc const& loc, int offset) { return loc.with_offset(-offset); }

constexpr ContractLoc& operator+=(ContractLoc& loc, int offset) { return (loc = loc.with_offset(offset)); }

constexpr static ContractLoc AnyLoc = ContractLoc(0, "*", "");

struct HumanRegex : std::regex {
  std::string re_str;

  HumanRegex() = default;
  HumanRegex(std::string re_str) : std::regex(re_str), re_str(re_str) {}
};

template <class OStream>
OStream& operator<<(OStream& os, HumanRegex const& re) {
  return os << "\"" << re.re_str << "\"";
}

struct ExpectedViolation {
  ContractLoc loc = AnyLoc;
  std::optional<AssertKind> assert_kind;
  std::optional<Semantic> semantic;
  std::optional<DetectionKind> detection_kind = failed;

  std::variant<std::monostate, std::string, HumanRegex> comment_matcher;

  void assign_field(ContractLoc& xloc) { loc = xloc; }
  void assign_field(AssertKind K) { assert_kind = K; }
  void assign_field(Semantic S) { semantic = S; }
  void assign_field(DetectionKind D) { detection_kind = D; }
  void assign_field(std::string comment) { comment_matcher = HumanRegex(comment); }

  template <class... Args>
  ExpectedViolation(Args... args)
    requires(!std::disjunction_v<std::is_same<Args, ExpectedViolation>...> && sizeof...(Args) > 0)
  {
    (assign_field(args), ...);
  }

  ExpectedViolation with_loc(ContractLoc xloc) const {
    auto cp = *this;
    cp.loc  = xloc;
    return cp;
  }

  ExpectedViolation with_assert(AssertKind kind) const {
    auto cp        = *this;
    cp.assert_kind = kind;
    return cp;
  }

  ExpectedViolation with_semantic(Semantic sem) const {
    auto cp     = *this;
    cp.semantic = sem;
    return cp;
  }

  ExpectedViolation with_comment(std::string comment) const {
    auto cp            = *this;
    cp.comment_matcher = comment;
    return cp;
  }

  ExpectedViolation with_comment_re(std::string comment) const {
    auto cp            = *this;
    cp.comment_matcher = HumanRegex(comment);
    return cp;
  }

  std::string to_string() const {
    std::string tmp = "Expected Violation: \n";
    tmp += loc.to_string();
    if (assert_kind) {
      tmp += std::format("    assertion_kind: {}\n", enum_to_string(*assert_kind));
    }
    if (semantic) {
      tmp += std::format("    semantic: {}\n", enum_to_string(*semantic));
    }
    if (detection_kind) {
      tmp += std::format("    detection_kind: {}\n", enum_to_string(*detection_kind));
    }
    if (std::holds_alternative<std::string>(comment_matcher)) {
      tmp += std::format("    comment_matcher: {}\n", std::get<std::string>(comment_matcher));
    } else if (std::holds_alternative<HumanRegex>(comment_matcher)) {
      tmp += std::format("    comment_matcher_regex: {}\n", std::get<HumanRegex>(comment_matcher).re_str);
    }
    return tmp;
  }

  bool diagnose_comment_mismatch(std::string_view comment) const {
    if (std::holds_alternative<std::string>(comment_matcher)) {
      std::string cm = std::get<std::string>(comment_matcher);
      if (cm != comment) {
        std::cerr << "    Comment mismatch: " << cm << " != " << comment << std::endl;
        return true;
      }
      return false;
    }
    if (std::holds_alternative<HumanRegex>(comment_matcher)) {
      HumanRegex cm = std::get<HumanRegex>(comment_matcher);
      if (!std::regex_match(comment.begin(), comment.end(), cm)) {
        std::cerr << "    Comment mismatch: '" << comment << "' " << "didn't match regex" << std::endl;
        return true;
      }
      return false;
    }
    return false;
  }

  bool diagnose_mismatch(contract_violation const& vio) const {
    if (loc.diagnose_mismatch(ContractLoc(vio))) {
      return true;
    }
    if (semantic && vio.semantic() != *semantic) {
      std::cerr << "    Semantic mismatch: " << enum_to_string(vio.semantic()) << " != " << enum_to_string(*semantic)
                << std::endl;
      return true;
    }
    if (assert_kind && vio.kind() != *assert_kind) {
      std::cerr << "    Kind mismatch: " << enum_to_string(vio.kind()) << " != " << enum_to_string(*assert_kind)
                << std::endl;
      return true;
    }
    if (detection_kind && vio.detection_mode() != *detection_kind) {
      std::cerr << "    Detection mismatch: " << enum_to_string(vio.detection_mode())
                << " != " << enum_to_string(*detection_kind) << std::endl;
      return true;
    }
    if (diagnose_comment_mismatch(vio.comment()))
      return true;
    return false;
  }
};

#define CAPTURE_LOC(name) ::capture_loc(name)
#define CAPTURE_LOC_AT(name, loc) ::capture_loc(name, loc)

constexpr ContractLoc capture_loc(SLOC(loc)) { return ContractLoc(loc); }

constexpr ContractLoc capture_loc_at(unsigned offset, const char* file = __FILE__, unsigned line = __LINE__) {
  return ContractLoc(line + offset, file);
}

using ContractHandlerType = std::function<void(std::contracts::contract_violation const&)>;

inline ContractHandlerType *get_contract_handler() {
  static ContractHandlerType handler;
  return &handler;
}

struct ContractChecker {
  static ContractChecker* instance() {
    static ContractChecker checker;
    return &checker;
  }

  ExpectedViolation pop_front() {
    auto vio = expected_violations.front();
    expected_violations.pop_front();
    return vio;
  }

  void operator()(std::contracts::contract_violation const& V) {
    ++violation_count;
    std::cerr << to_string(V) << std::endl;

    if (expected_violations.empty()) {
      std::cerr << "Unexpected violation: " << V.comment() << std::endl;
      return;
    }
    auto next_vio     = pop_front();
    bool has_mismatch = next_vio.diagnose_mismatch(V);
    if (!has_mismatch && V.semantic() == observe)
      return;
    if (!has_mismatch && V.semantic() == enforce) {
      std::exit(0);
    }
  }

  void push_back(ExpectedViolation vio) { expected_violations.push_back(vio); }

  bool empty() const { return expected_violations.empty(); }

  unsigned count() const { return violation_count; }

  unsigned remaining() const { return expected_violations.size(); }

  void dump() {
    for (auto& vio : expected_violations) {
      std::cerr << vio.to_string() << std::endl;
    }
  }

  void assert_empty() {
    if (!expected_violations.empty()) {
      std::cerr << "Expected violations not checked: " << expected_violations.size() << std::endl;
      for (auto& vio : expected_violations) {
        std::cerr << vio.to_string() << std::endl;
      }
      std::exit(1);
    }
  }

  template <class... Args>
  ContractChecker* expect(Args&&... args) {
    ExpectedViolation ex(args...);
    push_back(ex);
    return this;
  }

  template <class It, class Sent>
  ContractChecker* expect_range(It it, Sent sent) {
    for (; it != sent; ++it) {
      push_back(*it);
    }
    return this;
  }

  std::deque<ExpectedViolation> expected_violations;
  unsigned violation_count = 0;
};
auto Checker = ContractChecker::instance();



struct ContractTester;


struct ContractTesterArgs {
  int evaluated = -1;
  int violated  = -1;
};

struct ContractTester {


  template <class Func>
  static auto make_evaluator(Func f) {
    if constexpr (std::is_invocable_v<Func>) {
      return [f = f](const ContractTester&) { f(); };
    } else {
      static_assert(std::is_invocable_r_v<void, Func, const ContractTester&>);
      return f;
    }
  }

  using EvalFuncT = std::function<void(const ContractTester&)>;

  ContractTester() = default;
  ContractTester(ContractTester*&& other) : ContractTester(*other) {}

  bool operator()(bool value) const {
    evaluation_count += 1;
    bool result = value;
    if (xon_eval) {
      xon_eval(*this);
    }
    if (xon_failure && !result) {
      xon_failure(*this);
    }
    if (xon_success && result) {
      xon_success(*this);
    }
    if (!result)
      violation_count += 1;
    return result;
  }

  template <class FuncT>
  ContractTester* on_eval(FuncT&& f) {
    xon_eval = make_evaluator(f);
    return this;
  }

  template <class FuncT>
  ContractTester* on_failure(FuncT&& f) {
    xon_failure = make_evaluator(f);
    return this;
  }

  template <class FuncT>
  ContractTester* on_success(FuncT&& f) {
    xon_success = make_evaluator(f);
    return this;
  }

  ContractTester* set_result(bool new_result) {
    default_evaluation_result = new_result;
    return this;
  }

  std::string to_string() const {
    auto dummy      = ContractTesterArgs{.evaluated = evaluation_count, .violated = violation_count};
    std::string tmp = dump_struct(&dummy);

    return tmp;
  }

  void do_assertion(std::string msg, std::source_location loc) const {
    std::string tmp = ::to_string(loc);
    tmp += std::format("Assertion failed: {}\n", msg);
    tmp += to_string();
    std::cerr << tmp << std::endl;
    std::exit(1);
  }

  ContractTester* massert(ContractTesterArgs args, SLOC(loc)) {
    if (args.evaluated != -1 && evaluation_count != args.evaluated) {
      do_assertion(std::format("evaluated != {}", args.evaluated), loc);
    }
    if (args.violated != -1 && violation_count != args.violated) {
      do_assertion(std::format("violated != {}", args.violated), loc);
    }
    return this;
  }

  std::string_view name        = "";
  mutable int evaluation_count = 0;
  mutable int violation_count  = 0;
  EvalFuncT xon_eval;
  EvalFuncT xon_failure;
  EvalFuncT xon_success;
  bool default_evaluation_result = true;
};


std::string_view get_global_string(std::string_view sv) {
  static std::set<std::string> strings;
  auto result = strings.insert(std::string(sv));
  return *result.first;
}


template <class ValueT>
std::map<std::string, ValueT>& GetCounterStore() {
  static std::map<std::string, ValueT> CounterStore;
  return CounterStore;
}

template <class ValueT = int, class ...Args>
ValueT& get_or_insert(std::string Key, Args&& ...args) {
  auto &store = GetCounterStore<ValueT>();
  auto pos = store.find(Key);
  if (pos != store.end()) {
    return pos->second;
  }

  return store.emplace(
                  std::piecewise_construct, std::forward_as_tuple(Key), std::forward_as_tuple(std::forward<Args>(args)...)).first->second;

}

template <TStr Str, class ValueT = int>
auto& KV = get_or_insert<ValueT>(Str.str());

struct NamedCounter {
  constexpr NamedCounter(const char* name, int* counter, SLOC(loc)) : Name(get_global_string(name ? name : "")), Counter(counter), LastLoc(loc) {}
  constexpr NamedCounter(const char* name, SLOC(loc)) :  Name(get_global_string( name ? name : "")),
                                                        Counter(&get_or_insert<int>(std::string(Name))), LastLoc(loc) {}
  constexpr NamedCounter(TStr name, SLOC(loc)) : Name(get_global_string(name.sv())), Counter(&get_or_insert<int>(name.str())), LastLoc(loc) {}

  NamedCounter& loc(SLOC(xloc)) {
    LastLoc = xloc;
    return *this;
  }

  friend NamedCounter operator+(NamedCounter& LHS, int RHS) {
    *LHS.Counter += RHS;
    return LHS;
  }

  friend NamedCounter operator-(NamedCounter& LHS, int RHS) {
    *LHS.Counter -= RHS;
    return LHS;
  }

  friend NamedCounter& operator++(NamedCounter& LHS) {
    *LHS.Counter += 1;
    return LHS;
  }

  friend NamedCounter& operator--(NamedCounter& LHS) {
    *LHS.Counter -= 1;
    return LHS;
  }

  friend NamedCounter operator++(NamedCounter& LHS, int) {
    *LHS.Counter += 1;
    return LHS;
  }

  friend NamedCounter operator--(NamedCounter& LHS, int) {
    *LHS.Counter -= 1;
    return LHS;
  }

  friend NamedCounter& operator+=(NamedCounter& LHS, int RHS) {
    *LHS.Counter += RHS;
    return LHS;
  }

  friend NamedCounter& operator-=(NamedCounter& LHS, int RHS) {
    *LHS.Counter -= RHS;
    return LHS;
  }

  bool operator==(int RHS) const {
    return *Counter == RHS;
  }

  auto operator<=>(int RHS) const {
    return *Counter <=> RHS;
  }

  int consume() {
    int tmp = *Counter;
    *Counter = 0;
    return tmp;
  }

  bool assert_eq(int RHS, SLOC(loc)) {
    if (*Counter == RHS)
      return true;
    report_difference(RHS, "==", loc);
    return false;
  }

  bool assert_ne(int RHS, SLOC(loc)) {
    if (*Counter != RHS)
      return true;
    report_difference(RHS, "!=", loc);
    return false;
  }

  bool assert_gt(int RHS, SLOC(loc)) {
    if (*Counter > RHS)
      return true;
    report_difference(RHS, ">", loc);
    return false;
  }

  bool assert_lt(int RHS, SLOC(loc)) {
    if (*Counter < RHS)
      return true;
    report_difference(RHS, "<", loc);
    return false;
  }

  void report_difference(int RHS, const char* op, SLOC(loc)) {
    std::cerr << to_string(loc) << ": ";
    std::cerr << "Error: Counter(" << *Counter << ") " << op << " " << RHS << " failed. " << std::endl;
    if (LastLoc.function_name()) {
      std::cerr << "Counter last seen near: " << to_string(LastLoc);
    }
  }



  std::string_view Name;
  int *Counter;
  std::source_location LastLoc;
};


template <TStr Key>
auto NC = NamedCounter{get_global_string(Key.str()).data(), &KV<Key>};


inline bool count(bool value) {
  KV<"AssertCounter"> += 1;
  return value;
}



template <class... Args, class T>
inline bool eq(std::tuple<Args...>& list, std::initializer_list<T> il) {
  auto initlist_to_tuple = [](auto il) {
    constexpr int N = sizeof...(Args);
    assert(il.size() == N);
    std::array<T, N> arr = {};
    std::copy(il.begin(), il.end(), arr.begin());
    return std::tuple_cat(arr);
  };

  auto tup2           = initlist_to_tuple(il);
  decltype(tup2) tup3 = list;
  bool result         = (list == tup2);
  if (!result) {
    auto vector_to_str = [](auto ...vargs) {
      std::string tmp = "[";
      bool first      = true;
      std::vector V = {vargs...};
      for (auto vv : V) {
        if (!first) {
          tmp += ", ";
        }
        first = false;
        tmp += std::to_string(vv);
      }
      return tmp + "]";
    };

    std::string expect_str = std::apply(vector_to_str, tup2);
    std::string actual_str = std::apply(vector_to_str, tup3);

    std::cout << "Expected: " << expect_str << "\n";
    std::cout << "Actual:   " << actual_str << "\n";
    std::cout << std::endl;
  }
  return result;
}

template <class T = int, class... Args>
inline void reset(std::tuple<Args&...> const& list) {
  auto Reseter = [](auto& ...args) { ((args = {}), ...); };
  std::apply(Reseter, list);
}


template <class T>
struct CounterStoreT {
  decltype(auto) get() { return GetCounterStore<T>(); }

  decltype(auto) operator[](std::string_view key) { return get()[std::string(key)]; }

  decltype(auto) at(std::string_view key) { return get().at(std::string(key)); }

  std::map<std::string, int>* operator->() { return &get(); }
};
constinit CounterStoreT<int> CounterStore;

template <auto, class T>
using AsType = T;

template <TStr Key>
auto Counter = std::ref(CounterStore[Key.str()]);

template <TStr... Key>
auto CounterGroup = std::tuple<AsType<Key, int&>...>{GetCounterStore<int>()[Key.str()]...};

struct AliveCounter {
  explicit AliveCounter(NamedCounter counter, SLOC(loc)) : Counter(counter) {
    assert(Counter >= 0);
    Counter += 1;
  }
  explicit AliveCounter(const char* key, SLOC(loc)) : Counter(key, loc) {
    assert(Counter >= 0);
    Counter += 1;
  }

  AliveCounter(nullptr_t) = delete;
  AliveCounter(void*)     = delete;

  constexpr AliveCounter(int* dest, SLOC(loc)) : Counter("", dest, loc) {
    assert(Counter >= 0);
    Counter += 1;
  }

  constexpr AliveCounter(AliveCounter const& RHS) : Counter(RHS.Counter) {
    assert(Counter >= 0);
    Counter += 1;
  }

  ~AliveCounter() {
    assert(Counter >= 1);
    Counter -= 1;
  }

  NamedCounter Counter;
};

template <TStr Key>
struct CAliveCounter : private AliveCounter {
  CAliveCounter(SLOC(loc)) : AliveCounter(Key.sv().data(), loc) {}
  CAliveCounter(CAliveCounter const& RHS) : AliveCounter(RHS) {}

  ~CAliveCounter() = default;
};

#pragma clang diagnostic pop

#endif // LIBCXX_TEST_CONTRACTS_SUPPORT_H