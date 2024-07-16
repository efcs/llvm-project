// ADDITIONAL_COMPILE_FLAGS: -std=c++17 -fcontracts -fcontract-evaluation-semantic=observe
#include <cassert>
#include <contracts>
#include <iostream>

using std::contracts::contract_violation;


int violation_count = 0;
void handle_contract_violation(contract_violation const& V) {
  violation_count += 1;
  std::cout << "Here with " << V.comment() << std::endl;
  std::terminate();
}

void test(int x) pre(x != 1) {
  contract_assert(x != 0);
}


int main() {
  test(2);
  test(1);
}
