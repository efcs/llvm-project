#ifndef TEST_CONTRACTS_HANDLER_H_
#define TEST_CONTRACTS_HANDLER_H_

#include <contracts>
#include <functional>


#include "contracts_support.h"


void handle_contract_violation(std::contracts::contract_violation const& violation) {
  auto &handler = *get_contract_handler();
  if (!handler)
    std::contracts::invoke_default_contract_violation_handler(violation);
  else
    handler(violation);
}

struct ContractHandlerInstaller {
  ContractHandlerInstaller() = default;

  template <class Func>
  ContractHandlerInstaller(Func&& F, bool UninstallOnDestruction = true) : uninstall_on_destruction(UninstallOnDestruction), have_old_handler(true), old_handler(*get_contract_handler()) {
    do_install(std::forward<Func>(F));
  }

  template <class Func>
  void do_install(Func&&  f) {
    if constexpr (std::is_invocable_r_v<void, Func, std::contracts::contract_violation const&>) {
      *get_contract_handler() = std::forward<Func>(f);
    } else {
      *get_contract_handler() = [f=std::forward<Func>(f)](std::contracts::contract_violation const& violation) {
        f();
        ((void)violation);
      };
    }
  }

  template <class Func>
  void install(Func&& f, bool UninstallOnDestruction = true) {

    if (!have_old_handler) {
      old_handler = *get_contract_handler();
      have_old_handler = true;
    }
    uninstall_on_destruction = UninstallOnDestruction;

    do_install(std::forward<Func>( f));


  }

  void uninstall() {
    *get_contract_handler() = old_handler;
    uninstall_on_destruction = false;
    old_handler = nullptr;
  }

  ~ContractHandlerInstaller() {
    if (uninstall_on_destruction)
      *get_contract_handler() = old_handler;
    old_handler = nullptr;
  }

  bool uninstall_on_destruction = false;
  bool have_old_handler = false;
  ContractHandlerType old_handler;
};


#endif //