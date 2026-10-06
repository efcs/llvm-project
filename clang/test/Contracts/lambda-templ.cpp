// RUN: %clang_cc1 -std=c++26 -fsyntax-only -fcolor-diagnostics %s -fcontracts



template <class T>
void foo () {
  +[](int p, int pppp) { // expected-note {{while substituting}}
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