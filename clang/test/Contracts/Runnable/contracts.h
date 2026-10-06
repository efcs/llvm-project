// A minimal <contracts> for the runnable tests, so that they do not depend on
// the standard library's: the std::contracts interface of [support.contract],
// with a contract_violation that wraps the chain of data blocks the
// contract-violation runtime of the Itanium C++ Contracts ABI passes to the
// handler (see contracts-runtime.h, which provides the runtime and the
// definitions of the accessors).

#ifndef CONTRACTS_CONTRACTS_H
#define CONTRACTS_CONTRACTS_H

#include "source-location.h"
#include <stdio.h>

namespace __cxxabiv1 {
struct __cxa_contract_data_block;
}

namespace std::contracts {
  class contract_violation;
}

__attribute__((weak)) void handle_contract_violation(const std::contracts::contract_violation&);

namespace std::contracts {

enum class assertion_kind : unsigned char {
  pre = 1,
  post = 2,
  assert = 3
};
enum class evaluation_semantic : unsigned char {
  ignore = 1,
  observe = 2,
  enforce = 3,
  quick_enforce = 4
};
enum class detection_mode : unsigned char {
  predicate_false = 1,
  evaluation_exception = 2
};

class contract_violation {
public:
  ~contract_violation() = default;
  contract_violation(const contract_violation&) = delete;
  contract_violation& operator=(const contract_violation&) = delete;

  std::source_location location() const noexcept;
  const char* comment() const noexcept;
  // Not part of [support.contract]: the message of the assertion, or null if
  // it has none.
  const char* message() const noexcept;
  contracts::detection_mode detection_mode() const noexcept;
  assertion_kind kind() const noexcept;
  evaluation_semantic semantic() const noexcept;
  bool is_terminating() const noexcept;

private:
  friend struct __contract_violation_access;
  explicit contract_violation(const __cxxabiv1::__cxa_contract_data_block* chain) noexcept : __chain_(chain) {}

  // The head of the chain, which the runtime's entry points build.
  const __cxxabiv1::__cxa_contract_data_block* __chain_;
};

inline void invoke_default_contract_violation_handler(const contract_violation& violation) noexcept {
  ::fprintf(stderr, "Contract violation: %s\n", violation.comment());
  __builtin_abort();
}

} // namespace std::contracts

#endif  // CONTRACTS_CONTRACTS_H
