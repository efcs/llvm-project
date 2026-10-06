//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <__config>
#include <contracts>
#include <exception>
#include <source_location>

// The contract-violation runtime of the Itanium C++ Contracts ABI (the entry
// points called by the compiler, the dispatch to the handler and the default
// handler) is provided by libc++abi. This file provides the C++ interface to it
// from [support.contract]: the accessors of std::contracts::contract_violation,
// which read the chain of data blocks the runtime passes to the handler, and
// std::contracts::invoke_default_contract_violation_handler.
//
// FIXME: When libc++ is built on top of another ABI library, nothing provides
// the runtime: the entry points would need to be hosted by libc++ itself (as
// is done for operator new), and the handler replacement mechanism of the ABI
// depends on weak symbols, which Windows does not support across DLLs.
#if defined(LIBCXX_BUILDING_LIBCXXABI)

#  include <__contracts/abi.h>

using namespace __cxxabiv1;

_LIBCPP_BEGIN_UNVERSIONED_NAMESPACE_STD

namespace contracts {

_LIBCPP_BEGIN_EXPLICIT_ABI_ANNOTATIONS

// The values of the ABI enumerations are those of the standard enumerators, so
// the known values are translated by a cast. Unknown values (for instance the
// kinds of assertions that other implementations or future standards may
// produce) are passed through unchanged, as [support.contract.enum] allows
// implementation-defined values.
static_assert(static_cast<int>(assertion_kind::pre) == CXA_AK_PRE);
static_assert(static_cast<int>(assertion_kind::post) == CXA_AK_POST);
static_assert(static_cast<int>(assertion_kind::assert) == CXA_AK_ASSERT);
static_assert(static_cast<int>(evaluation_semantic::ignore) == CXA_ES_IGNORE);
static_assert(static_cast<int>(evaluation_semantic::observe) == CXA_ES_OBSERVE);
static_assert(static_cast<int>(evaluation_semantic::enforce) == CXA_ES_ENFORCE);
static_assert(static_cast<int>(evaluation_semantic::quick_enforce) == CXA_ES_QUICK_ENFORCE);
static_assert(static_cast<int>(detection_mode::predicate_false) == CXA_DM_PREDICATE_FALSE);
static_assert(static_cast<int>(detection_mode::evaluation_exception) == CXA_DM_EVALUATION_EXCEPTION);

assertion_kind contract_violation::kind() const noexcept {
  return static_cast<assertion_kind>(
      __cxa_find_field_value<__UINT8_TYPE__>(__chain_, CXA_FIELD_ASSERTION_KIND, CXA_AK_UNSPECIFIED));
}

evaluation_semantic contract_violation::semantic() const noexcept {
  return static_cast<evaluation_semantic>(
      __cxa_find_field_value<__UINT8_TYPE__>(__chain_, CXA_FIELD_EVALUATION_SEMANTIC, CXA_ES_UNSPECIFIED));
}

contracts::detection_mode contract_violation::detection_mode() const noexcept {
  return static_cast<contracts::detection_mode>(
      __cxa_find_field_value<__UINT8_TYPE__>(__chain_, CXA_FIELD_DETECTION_MODE, CXA_DM_UNSPECIFIED));
}

bool contract_violation::is_terminating() const noexcept {
  evaluation_semantic __semantic = semantic();
  return __semantic == evaluation_semantic::enforce || __semantic == evaluation_semantic::quick_enforce;
}

const char* contract_violation::comment() const noexcept {
  const char* const* __comment = __cxa_find_field_ptr<const char*>(__chain_, CXA_FIELD_COMMENT);
  return __comment && *__comment ? *__comment : "";
}

source_location contract_violation::location() const noexcept {
  // The source location in the chain has the layout of the data a
  // source_location refers to, and lives at least as long as the handler.
  if (const __cxa_source_location* __loc =
          __cxa_find_field_ptr<__cxa_source_location>(__chain_, CXA_FIELD_SOURCE_LOCATION))
    return source_location::__create_from_pointer(__loc);
  return source_location();
}

exception_ptr contract_violation::evaluation_exception() const noexcept {
  if (detection_mode() != detection_mode::evaluation_exception)
    return exception_ptr();
#  if !defined(_LIBCPP_ABI_MICROSOFT)
  // The runtime records the exception the predicate exited with (the exception
  // object, as std::exception_ptr refers to it) when it dispatches the
  // violation, so that the result does not depend on which exception is
  // currently being handled when this is called.
  if (void* __exception = __cxa_find_field_value<void*>(__chain_, CXA_FIELD_EXCEPTION_PTR, nullptr))
    return exception_ptr::__from_native_exception_pointer(__exception);
#  endif
  // The handler is invoked from within an active handler for the exception.
  return current_exception();
}

void invoke_default_contract_violation_handler(const contract_violation& __violation) {
  __contract_invoke_default_handler(&__violation);
}

_LIBCPP_END_EXPLICIT_ABI_ANNOTATIONS

} // namespace contracts

_LIBCPP_END_UNVERSIONED_NAMESPACE_STD

// The default definition of the replaceable contract-violation handler
// ([basic.contract.handler]). The runtime does not call this symbol: it calls
// the program's replacement through the C-linkage symbol
// __handle_contract_violation, which the compiler defines as an alias of a
// replacement it compiles. This definition is what a program that declares and
// calls ::handle_contract_violation itself gets, and it is weak so that a
// replacement can be linked statically.
//
// Declaring the C-linkage symbol here keeps the compiler from emitting it as an
// alias of this default definition.
extern "C" void __handle_contract_violation(const std::contracts::contract_violation&);

#  if !defined(_LIBCPP_OBJECT_FORMAT_COFF)
_LIBCPP_OVERRIDABLE_FUNC_VIS void handle_contract_violation(const std::contracts::contract_violation&);

[[gnu::weak]] void handle_contract_violation(const std::contracts::contract_violation& __violation) {
  std::contracts::invoke_default_contract_violation_handler(__violation);
}
#  endif

#endif // LIBCXX_BUILDING_LIBCXXABI
