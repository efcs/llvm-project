#include "../include/contracts"
#include <__config>
#include <exception>
#include <iostream>

using namespace std::contracts;

struct std::contracts::_ContractViolationImpl {
  _AssertKind kind              = _AssertKind::assert;
  _EvaluationSemantic semantic  = _EvaluationSemantic::enforce;
  _DetectionMode mode           = _DetectionMode::predicate_false;
  const char* comment           = nullptr;
  std::source_location location = std::source_location::current();

  contract_violation __create_violation() const { return contract_violation{this}; }
};

namespace {

static void __default_violation_handler(const contract_violation& violation) {
  using namespace std::contracts;
  std::cerr << violation.location().file_name() << ":" << violation.location().line() << ": ";
  auto assert_str = [&]() -> std::pair<const char*, const char*> {
    switch (violation.kind()) {
    case _AssertKind::pre:
      return {"pre(", ")"};
    case _AssertKind::post:
      return {"post(", ")"};
    case _AssertKind::assert:
      return {"contract_assert(", ")"};
    case _AssertKind::cassert:
      return {"assert(", ")"};
    case _AssertKind::manual:
    case _AssertKind::__unknown:
      return {"", ""};
    }
  }();
  std::cerr << assert_str.first << violation.comment() << assert_str.second;
  if (violation.detection_mode() == _DetectionMode::predicate_false) {
    std::cerr << " failed" << std::endl;
  } else {
    std::cerr << " exited via exception" << std::endl;
  }
}

template <int N>
struct _BuiltinContractStruct;

template <>
struct _BuiltinContractStruct<0> {
  enum { VERSION = 1 };
  int version;
};

template <>
struct _BuiltinContractStruct<1> {
  enum { VERSION = 1 };
  unsigned version; // the version of the struct
  const char* comment;
  const char* file;
  const char* function;
  unsigned lineno = 0;
  unsigned contract_kind;
  unsigned eval_semantic;
  unsigned detection_mode;
};

template <>
struct _BuiltinContractStruct<2> {
  enum { VERSION = 2 };
  unsigned version; // the version of the struct
  const char* comment;
  const char* file;
  const char* function;
  unsigned lineno = 0;
  unsigned contract_kind;
  unsigned eval_semantic;
};

template <>
struct _BuiltinContractStruct<3> {
  enum { VERSION = 3 };
  unsigned version; // the version of the struct
  const char* file;
  const char* function;
  unsigned lineno = 0;
  unsigned column = 0;
  const char* comment;
  unsigned contract_kind;
};

union _BuiltinContractStructUnion {
  _BuiltinContractStruct<0> v0;
  _BuiltinContractStruct<1> v1;
  _BuiltinContractStruct<2> v2;
  _BuiltinContractStruct<3> v3;
};

_ContractViolationImpl create_impl(void* data, _EvaluationSemantic* sem = nullptr, _DetectionMode* mode = nullptr) {
  _BuiltinContractStructUnion* data_union = static_cast<_BuiltinContractStructUnion*>(data);
  switch (data_union->v0.version) {
  case 1:
    return _ContractViolationImpl{
        .kind     = static_cast<_AssertKind>(data_union->v1.contract_kind),
        .semantic = static_cast<_EvaluationSemantic>(data_union->v1.eval_semantic),
        .mode     = static_cast<_DetectionMode>(data_union->v1.detection_mode),
        .comment  = data_union->v1.comment,
        .location = std::source_location::current()};
  case 2:
    return _ContractViolationImpl{
        .kind     = static_cast<_AssertKind>(data_union->v2.contract_kind),
        .semantic = static_cast<_EvaluationSemantic>(data_union->v2.eval_semantic),
        .mode     = mode ? *mode : _DetectionMode::predicate_false,
        .comment  = data_union->v2.comment,
        .location = std::source_location::current()};
  case 3:
    return _ContractViolationImpl{
        .kind     = static_cast<_AssertKind>(data_union->v3.contract_kind),
        .semantic = sem ? *sem : _EvaluationSemantic::enforce,
        .mode     = mode ? *mode : _DetectionMode::predicate_false,
        .comment  = data_union->v3.comment,
        .location = std::source_location::__create_from_pointer(
            reinterpret_cast<const char*>(data) + offsetof(_BuiltinContractStruct<3>, file))};
  default:
    return _ContractViolationImpl{
        .kind     = _AssertKind::assert,
        .semantic = sem ? *sem : _EvaluationSemantic::enforce,
        .mode     = mode ? *mode : _DetectionMode::predicate_false,
        .comment  = "<unknown contract violation>",
        .location = std::source_location()};
  }
}

} // end namespace

void std::contracts::invoke_default_contract_violation_handler(const contract_violation& violation) noexcept {
  __default_violation_handler(violation);
}

std::source_location contract_violation::location() const noexcept { return __pimpl_->location; }
const char* contract_violation::comment() const noexcept { return __pimpl_->comment; }
_DetectionMode contract_violation::detection_mode() const noexcept { return __pimpl_->mode; }
assertion_kind contract_violation::kind() const noexcept { return __pimpl_->kind; }
evaluation_semantic contract_violation::semantic() const noexcept { return __pimpl_->semantic; }

extern "C" _LIBCPP_EXPORTED_FROM_ABI void
__handle_contract_violation_v3(_EvaluationSemantic __semantic, _DetectionMode __mode, void* dataptr) {
  using namespace std::contracts;
  _ContractViolationImpl impl  = create_impl(dataptr, &__semantic, &__mode);
  contract_violation violation = impl.__create_violation();

  if (::handle_contract_violation)
    ::handle_contract_violation(violation);
  else
    invoke_default_contract_violation_handler(violation);

  if (violation.semantic() == _EvaluationSemantic::enforce) {
    std::terminate();
  }
}

namespace {

void __run_violation_handler(const std::contracts::contract_violation& violation) {
  if (::handle_contract_violation)
    ::handle_contract_violation(violation);
  else
    invoke_default_contract_violation_handler(violation);

  if (violation.semantic() == evaluation_semantic::enforce)
    std::terminate();
}

void __run_nothrow_violation_handler(const std::contracts::contract_violation& violation) noexcept {
#if _LIBCPP_HAS_EXCEPTIONS
try {
#endif
  __run_violation_handler(violation);
#if _LIBCPP_HAS_EXCEPTIONS
  } catch (...) {
    std::terminate();
  }
#endif
}

} // end namespace

void std::contracts::__handle_manual_contract_violation(
  assertion_kind __kind,
  evaluation_semantic __semantic,
  detection_mode __mode,
  const char* __comment,
  std::source_location __loc,
  bool __can_throw
) {
  _ContractViolationImpl __impl{
      .kind     = __kind,
      .semantic = __semantic,
      .mode     = __mode,
      .comment  = __comment ? __comment : "<unknown contract violation>",
      .location = __loc};

  auto violation = __impl.__create_violation();
  if (!__can_throw)
    __run_nothrow_violation_handler(violation);
  else
    __run_violation_handler(violation);

}
