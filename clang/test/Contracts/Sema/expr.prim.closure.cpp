  // RUN: %clang_cc1 -std=c++2a -fcontracts  -fsyntax-only -fcolor-diagnostics -Wno-c++23-extensions -verify  %s
// XFAIL: *
// expected-no-diagnostics
static int i = 0;
void test() {
  auto f1 = [=] pre(i > 0) { // OK, no local entities are captured.
  };
  int i = 1;
  auto f2 = [=] pre(i > 0) { // expected-foo {{cannot implicitly capture i here}}
  };
  auto f3 = [i] pre(i > 0) { // OK, i is captured explicitly.
  };
  auto f4 = [=] {
    contract_assert(i > 0); // error: cannot implicitly capture i here
  };
  auto f5 = [=] {
    contract_assert(i > 0); // OK, i is referenced elsewhere.
    (void)i;
  };
  auto f6 = [=] pre([] {
    bool x = true;
    return [=] { return x; }(); // OK, x is captured implicitly.
  }()) {};
}