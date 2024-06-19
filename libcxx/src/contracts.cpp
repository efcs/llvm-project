#include <__config>
#include <contracts>

_LIBCPP_WEAK void handle_contract_violation() noexcept {
  __builtin_abort();
}
_LIBCPP_WEAK void handle_contract_violation(const std::contracts::contract_violation&) noexcept {
  __builtin_abort();
}

namespace std::contracts {

void invoke_default_contract_violation_handler(const contract_violation& violation) noexcept {
  ::handle_contract_violation(violation);
}

void invoke_default_contract_violation_handler() {
  invoke_default_contract_violation_handler(contract_violation());
}

} // namespace std::contracts


