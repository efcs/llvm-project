// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts

// A lambda within a contract within a lambda within a function template.



template <class T>
void foo () {
  (void)+[](int p, int pppp) {
    int local = 202;

    contract_assert(
        [&]
        (int p2) { return

        local; }
        (
            p)
            );
    return 0;

  };
}
template void foo<int>();
// expected-no-diagnostics
