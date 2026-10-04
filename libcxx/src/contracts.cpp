//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <__config>
#include <contracts>
#include <cstddef>
#include <cstdio>
#include <exception>
#include <source_location>

_LIBCPP_BEGIN_NAMESPACE_STD

namespace contracts {

struct __contract_violation_impl {
  assertion_kind __kind_;
  evaluation_semantic __semantic_;
  contracts::detection_mode __mode_;
  const char* __comment_;
  source_location __location_;
  exception_ptr __exception_;

  contract_violation __make_violation() const noexcept { return contract_violation(this); }
};

const char* contract_violation::comment() const noexcept { return __impl_->__comment_; }

contracts::detection_mode contract_violation::detection_mode() const noexcept { return __impl_->__mode_; }

exception_ptr contract_violation::evaluation_exception() const noexcept { return __impl_->__exception_; }

bool contract_violation::is_terminating() const noexcept {
  return __impl_->__semantic_ == evaluation_semantic::enforce ||
         __impl_->__semantic_ == evaluation_semantic::quick_enforce;
}

assertion_kind contract_violation::kind() const noexcept { return __impl_->__kind_; }

source_location contract_violation::location() const noexcept { return __impl_->__location_; }

evaluation_semantic contract_violation::semantic() const noexcept { return __impl_->__semantic_; }

namespace {

const char* __kind_to_string(assertion_kind __kind) noexcept {
  switch (__kind) {
  case assertion_kind::pre:
    return "pre";
  case assertion_kind::post:
    return "post";
  case assertion_kind::assert:
    return "contract_assert";
  }
  return "<unknown assertion kind>";
}

const char* __semantic_to_string(evaluation_semantic __semantic) noexcept {
  switch (__semantic) {
  case evaluation_semantic::ignore:
    return "ignore";
  case evaluation_semantic::observe:
    return "observe";
  case evaluation_semantic::enforce:
    return "enforce";
  case evaluation_semantic::quick_enforce:
    return "quick_enforce";
  }
  return "<unknown semantic>";
}

} // namespace

void invoke_default_contract_violation_handler(const contract_violation& __violation) {
  source_location __loc  = __violation.location();
  const char* __file     = __loc.file_name();
  const char* __function = __loc.function_name();
  const char* __comment  = __violation.comment();
  std::fprintf(
      stderr,
      "%s:%u: %s%scontract violation in %s(%s): %s (semantic: %s)\n",
      __file && *__file ? __file : "<unknown file>",
      static_cast<unsigned>(__loc.line()),
      __function ? __function : "",
      __function && *__function ? ": " : "",
      __kind_to_string(__violation.kind()),
      __comment ? __comment : "",
      __violation.detection_mode() == contracts::detection_mode::evaluation_exception
          ? "predicate evaluation exited via an exception"
          : "predicate evaluated to false",
      __semantic_to_string(__violation.semantic()));
  std::fflush(stderr);
}

} // namespace contracts

_LIBCPP_END_NAMESPACE_STD

// The contract-violation handler, ::handle_contract_violation, is replaceable
// ([basic.contract.handler]). Following the same approach as
// std::__libcpp_verbose_abort, the library provides a weak default definition
// that a strong definition in the program overrides. A user definition may be
// declared noexcept: noexcept is not part of the mangled name of a function, so
// it replaces this one as well.
_LIBCPP_OVERRIDABLE_FUNC_VIS void handle_contract_violation(const std::contracts::contract_violation&);

[[gnu::weak]] void handle_contract_violation(const std::contracts::contract_violation& __violation) {
  std::contracts::invoke_default_contract_violation_handler(__violation);
}

namespace {

// The layout of the data emitted by Clang (version 3) and passed to
// __handle_contract_violation_v3. The {file, function, line, column} members
// must stay contiguous and in this order: they have the same layout as the data
// referred to by std::source_location.
struct __contract_violation_data_v3 {
  unsigned __version_;
  const char* __file_;
  const char* __function_;
  unsigned __line_;
  unsigned __column_;
  const char* __comment_;
  unsigned __contract_kind_;
};

// Clang's internal encoding of the evaluation semantic (clang::ContractEvaluationSemantic).
// Note that it differs from std::contracts::evaluation_semantic.
enum : unsigned char {
  __clang_semantic_ignore        = 0,
  __clang_semantic_enforce       = 1,
  __clang_semantic_observe       = 2,
  __clang_semantic_quick_enforce = 3,
};

std::contracts::evaluation_semantic __translate_semantic(unsigned char __sem) noexcept {
  using std::contracts::evaluation_semantic;
  switch (__sem) {
  case __clang_semantic_ignore:
    return evaluation_semantic::ignore;
  case __clang_semantic_enforce:
    return evaluation_semantic::enforce;
  case __clang_semantic_observe:
    return evaluation_semantic::observe;
  case __clang_semantic_quick_enforce:
    return evaluation_semantic::quick_enforce;
  }
  // Unknown values are treated conservatively.
  return evaluation_semantic::enforce;
}

std::contracts::detection_mode __translate_detection_mode(unsigned char __mode) noexcept {
  // Clang's encoding (clang::ContractDetectionMode) matches the standard enumerators.
  return __mode == static_cast<unsigned char>(std::contracts::detection_mode::evaluation_exception)
           ? std::contracts::detection_mode::evaluation_exception
           : std::contracts::detection_mode::predicate_false;
}

std::contracts::assertion_kind __translate_kind(unsigned __kind) noexcept {
  switch (__kind) {
  case 1:
    return std::contracts::assertion_kind::pre;
  case 2:
    return std::contracts::assertion_kind::post;
  default:
    return std::contracts::assertion_kind::assert;
  }
}

} // namespace

// Entry point called by Clang-generated code when a contract is violated.
//
//   __semantic: Clang's internal evaluation semantic (enforce=1, observe=2, ...)
//   __mode:     the detection mode (predicate_false=1, evaluation_exception=2)
//   __data:     pointer to the compiler-generated __contract_violation_data_v3
//
// When __mode is evaluation_exception, this is called from within the handler
// that caught the exception, so std::current_exception() refers to it.
extern "C" _LIBCPP_EXPORTED_FROM_ABI void
__handle_contract_violation_v3(unsigned char __semantic, unsigned char __mode, void* __data);

extern "C" void __handle_contract_violation_v3(unsigned char __semantic, unsigned char __mode, void* __data) {
  using namespace std::contracts;
  const __contract_violation_data_v3* __info = static_cast<const __contract_violation_data_v3*>(__data);

  __contract_violation_impl __impl;
  __impl.__semantic_ = __translate_semantic(__semantic);
  __impl.__mode_     = __translate_detection_mode(__mode);
  if (__info && __info->__version_ == 3) {
    __impl.__kind_     = __translate_kind(__info->__contract_kind_);
    __impl.__comment_  = __info->__comment_ ? __info->__comment_ : "";
    __impl.__location_ = std::source_location::__create_from_pointer(&__info->__file_);
  } else {
    __impl.__kind_    = assertion_kind::assert;
    __impl.__comment_ = "<unknown contract violation>";
  }
  if (__impl.__mode_ == detection_mode::evaluation_exception)
    __impl.__exception_ = std::current_exception();

  const contract_violation __violation = __impl.__make_violation();
  ::handle_contract_violation(__violation);

  if (__violation.is_terminating())
    std::terminate();
}
