// ADDITIONAL_COMPILE_FLAGS: -std=c++17 -fcontracts -fcontract-evaluation-semantic=observe
#include <cassert>
#include <contracts>
#include <iostream>
#include <vector>

using std::contracts::contract_violation;

struct ContractLoc {
  unsigned line = 0;
  const char* file = nullptr;
  const char* comment = nullptr;

  bool operator==(const ContractLoc& other) const {
    return line == other.line && file == other.file && comment == other.comment;
  }
  bool operator!=(const ContractLoc& other) const { return !(*this == other); }
};


std::vector<ContractLoc> contract_locs;


bool capture_contract(const char* file, unsigned line, const char* comment, bool value) {
  contract_locs.push_back({line, file, comment});
  return value;
}


#define CONTRACT_CAP()

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
