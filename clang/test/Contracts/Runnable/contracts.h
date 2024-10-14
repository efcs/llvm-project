#ifndef CONTRACTS_CONTRACTS_H
#define CONTRACTS_CONTRACTS_H

#include "source-location.h"
#include <stdio.h>

namespace std::contracts {
  class contract_violation;
}

__attribute__((weak)) void handle_contract_violation(const std::contracts::contract_violation&);

namespace std::contracts {

enum class assertion_kind : unsigned {
  pre = 1,
  post = 2,
  assert = 3
};
enum class evaluation_semantic : unsigned {
  __unspecified = 0,
  enforce = 1,
  observe = 2
};
enum class detection_mode : unsigned {
  predicate_false = 1,
  evaluation_exception = 2
};
using _DetectionMode = detection_mode;

struct _ContractViolationImpl;

class contract_violation {
  friend struct _ContractViolationImpl;
public:
  ~contract_violation() = default;
  contract_violation(const contract_violation&) = delete;
  contract_violation& operator=(const contract_violation&) = delete;

   std::source_location location() const noexcept;
   const char* comment() const noexcept;
   _DetectionMode detection_mode() const noexcept;
   assertion_kind kind() const noexcept;
   evaluation_semantic semantic() const noexcept;

private:
  explicit contract_violation(_ContractViolationImpl &__impl) : __impl_(&__impl) {}

  _ContractViolationImpl *__impl_;
};


inline void __default_contract_violation_handler(const contract_violation& violation) noexcept {
  ::fprintf(stderr, "Contract violation: %s\n", violation.comment());
  __builtin_abort();
}


inline void invoke_default_contract_violation_handler(const contract_violation& __violation) noexcept {
  return __default_contract_violation_handler(__violation);
}

void __handle_contract_violation_internal(const contract_violation& __violation) {
  if (::handle_contract_violation)
    ::handle_contract_violation(__violation);
  else
    invoke_default_contract_violation_handler(__violation);

  if (__violation.semantic() == evaluation_semantic::enforce)
    __builtin_abort();
}

struct _ContractViolationImpl {
  assertion_kind kind           = assertion_kind::assert;
  evaluation_semantic semantic  = evaluation_semantic::enforce;
  _DetectionMode mode           = _DetectionMode::predicate_false;
  const char* comment           = nullptr;
  std::source_location location = std::source_location::current();

  contract_violation __create_violation()  {
    return contract_violation(*this);
  }
};

std::source_location contract_violation::location() const noexcept { return __impl_->location; }
const char* contract_violation::comment() const noexcept { return __impl_->comment; }
_DetectionMode contract_violation::detection_mode() const noexcept { return __impl_->mode; }
assertion_kind contract_violation::kind() const noexcept { return __impl_->kind; }
evaluation_semantic contract_violation::semantic() const noexcept { return __impl_->semantic; }


} // namespace std::contracts


//

#endif  // CONTRACTS_CONTRACTS_H