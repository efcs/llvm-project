// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts -fexperimental-new-constant-interpreter



constexpr int f(int x) post(r : r != 0) {
  return x;
}

constexpr int do_test() {
  f(1); // OK
  f(0); // Not OK
  return 42;
}

constexpr int test = do_test();
