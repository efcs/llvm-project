#ifndef LIBCXX_TEST_CONTRACTS_SUPPORT_H
#define LIBCXX_TEST_CONTRACTS_SUPPORT_H

#include "test_macros.h"
#include <contracts>
#include <variant>
#include <optional>
#include <iostream>
#include <string_view>
#include <tuple>
#include <regex>
#include <source_location>
#include <unordered_map>
#include <string>
#include <format>
#include "dump_struct.h"

#define SLOC(name) std::source_location name = std::source_location::current()

using std::contracts::contract_violation;
using Semantic      = std::contracts::evaluation_semantic;
using AssertKind    = std::contracts::assertion_kind;
using DetectionKind = std::contracts::detection_mode;

constexpr AssertKind pre     = AssertKind::pre;
constexpr AssertKind post    = AssertKind::post;
constexpr AssertKind cassert = AssertKind::assert;

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
  case cassert:
    return "contract_assert";
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
  case DetectionKind::__unknown:
    return "<unknown>";
  }
}

struct ContractLoc;

#define OBSERVE [[clang::contract_group("observe")]]
#define ENFORCE [[clang::contract_group("enforce")]]
#define QUICK_ENFORCE [[clang::contract_group("quick_enforce")]]
#define IGNORE [[clang::contract_group("ignore")]]

std::string to_string(contract_violation const& vio) {
  std::string tmp = "Contract Violation: \n";
  tmp += std::format("    {}:{}: \n", vio.file_name(), vio.line());
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
  constexpr explicit ContractLoc(const contract_violation& vio) : line(vio.line()), file(vio.file_name()) {}

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

  void handle_contract_violation(contract_violation const& V) {
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

  std::deque<ExpectedViolation> expected_violations;
  unsigned violation_count = 0;
};
auto Checker = ContractChecker::instance();

int violation_count = 0;
void handle_contract_violation(contract_violation const& V) {
  violation_count += 1;
  std::cout << "Here with " << V.comment() << std::endl;
  Checker->handle_contract_violation(V);
}

struct ContractTester;

template <class Func>
auto make_evaluator(Func f) {
  if constexpr (std::is_invocable_v<Func>) {
    return [f = f](const ContractTester&) { f(); };
  } else {
    static_assert(std::is_invocable_r_v<void, Func, const ContractTester&>);
    return f;
  }
}

template <class Func>
constexpr bool is_contract_eval_pred() {
  return std::is_invocable_r_v<void, Func, const ContractTester&> || std::is_invocable_r_v<void, Func>;
}

struct ContractTesterArgs {
  int evaluated = -1;
  int violated  = -1;
};

struct ContractTester {
  using EvalFuncT = std::function<void(const ContractTester&)>;

  constexpr ContractTester() = default;
  constexpr ContractTester(ContractTester*&& other) : ContractTester(*other) {}

  bool operator()(bool value) const {
    evaluation_count += 1;
    bool result = value;
    if (xon_eval) {
      xon_eval.value()(*this);
    }
    if (xon_failure && !result) {
      xon_failure.value()(*this);
    }
    if (xon_success && result) {
      xon_success.value()(*this);
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
    std::string tmp;
    if (loc.file_name()) {
      tmp += std::format("{}:{} in {}\n", loc.file_name(), loc.line(), loc.function_name());
    }
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
  std::optional<EvalFuncT> xon_eval;
  std::optional<EvalFuncT> xon_failure;
  std::optional<EvalFuncT> xon_success;
  bool default_evaluation_result = true;
};

#endif // LIBCXX_TEST_CONTRACTS_SUPPORT_H