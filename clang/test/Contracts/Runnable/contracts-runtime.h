#ifndef CONTRACTS_CONTRACTS_RUNTIME_H
#define CONTRACTS_CONTRACTS_RUNTIME_H

#include "contracts.h"

namespace __cxxabi {
namespace {

using namespace std::contracts;

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
  unsigned long version; // the version of the struct
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


_ContractViolationImpl create_impl(void* data, evaluation_semantic* sem = nullptr, _DetectionMode* mode = nullptr) {
  _BuiltinContractStructUnion* data_union = static_cast<_BuiltinContractStructUnion*>(data);
  switch (data_union->v0.version) {
  case 1:
    return _ContractViolationImpl{
        .kind     = static_cast<assertion_kind>(data_union->v1.contract_kind),
        .semantic = static_cast<evaluation_semantic>(data_union->v1.eval_semantic),
        .mode     = static_cast<_DetectionMode>(data_union->v1.detection_mode),
        .comment  = data_union->v1.comment,
        .location = std::source_location::current()};
  case 2:
    return _ContractViolationImpl{
        .kind     = static_cast<assertion_kind>(data_union->v2.contract_kind),
        .semantic = static_cast<evaluation_semantic>(data_union->v2.eval_semantic),
        .mode     = mode ? *mode : _DetectionMode::predicate_false,
        .comment  = data_union->v2.comment,
        .location = std::source_location::current()};
  case 3:
    return _ContractViolationImpl{
        .kind     = static_cast<assertion_kind>(data_union->v3.contract_kind),
        .semantic = sem ? *sem : evaluation_semantic::enforce,
        .mode     = mode ? *mode : _DetectionMode::predicate_false,
        .comment  = data_union->v3.comment,
        .location = std::source_location::__create_from_pointer(
            reinterpret_cast<const char*>(data) + __builtin_offsetof(_BuiltinContractStruct<3>, file))};
  default:
    return _ContractViolationImpl{
        .kind     = assertion_kind::assert,
        .semantic = sem ? *sem : evaluation_semantic::enforce,
        .mode     = mode ? *mode : _DetectionMode::predicate_false,
        .comment  = "<unknown contract violation>",
        .location = std::source_location()};
  }
}

} // namespace

} // namespace __cxxabi

extern "C" inline void
__handle_contract_violation_v3(unsigned __semantic, unsigned __mode, void* dataptr) {
  using namespace std::contracts;
  evaluation_semantic sem = static_cast<evaluation_semantic>(__semantic);
  _DetectionMode mode = static_cast<_DetectionMode>(__mode);

  // Impl needs to outlive the violation
  _ContractViolationImpl impl  = __cxxabi::create_impl(dataptr, &sem, &mode);
  std::contracts::__handle_contract_violation_internal(impl.__create_violation());
}

#endif // CONTRACTS_CONTRACTS_RUNTIME_H