// Without -fcontracts the facility is inactive: __cpp_contracts is undefined and
// pre/post/contract_assert are ordinary identifiers, not keywords.
// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s
// expected-no-diagnostics

#if defined(__cpp_contracts)
#error "__cpp_contracts must be undefined without -fcontracts"
#endif

int pre = 1;
int post = 2;
int contract_assert = 3;
int use() { return pre + post + contract_assert; }
