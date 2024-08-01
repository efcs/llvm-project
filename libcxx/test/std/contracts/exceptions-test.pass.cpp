#include <contracts>
#include "contracts_test.h"
#include "nttp_string.h"
#include "contracts_support.h"
#include "contracts_handler.h"

struct A : CAliveCounter<"A"> {

};

int main() {
  ContractHandlerInstaller installer;
  installer.install([&]() {
    installer.uninstall();

  })
  {
    A a;
    {
      A a2;

    }
  }
}
