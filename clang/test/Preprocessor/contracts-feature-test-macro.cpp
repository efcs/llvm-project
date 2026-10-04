// RUN: %clang_cc1 -std=c++26 -E -dM %s | FileCheck --check-prefix=NONE %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fno-contracts -E -dM %s | FileCheck --check-prefix=NONE %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -E -dM %s | FileCheck --check-prefix=CONTRACTS %s
// RUN: %clang_cc1 -std=c++20 -fcontracts -E -dM %s | FileCheck --check-prefix=CONTRACTS %s
// RUN: %clang_cc1 -std=c++11 -fcontracts -E -dM %s | FileCheck --check-prefix=CONTRACTS %s

// NONE-NOT: #define __cpp_contracts
// CONTRACTS: #define __cpp_contracts 202502L
