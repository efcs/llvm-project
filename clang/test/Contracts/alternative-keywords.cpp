// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts

// expected-no-diagnostics

// The `__pre`, `__post` and `__contract_assert` spellings are aliases of the
// standard keywords (TokenKinds.def), so they must be accepted everywhere the
// standard spellings are.

int f(const int x) __pre(x) __post(x > 0) {
  __contract_assert(x);
  return x;
}

// The cases below reach contract handling by other paths.  They are worth
// covering because the equivalent gap on GCC hid an ICE in its own handling
// of these spellings: recognising the keyword is not the same as routing it
// through the contract machinery correctly.

// In a template, where the contracts are handled once for the pattern and
// again for each instantiation.
template <typename T>
T tmpl(const T x) __pre(x > 0) __post(r : r > 0) {
  __contract_assert(x > 0);
  return x;
}

template int tmpl<int>(const int);

// On a member function, whose contracts are late-parsed.
struct S {
  int m = 1;
  int mem(const int x) __pre(x > 0) __post(x > 0) {
    __contract_assert(m > 0);
    return x;
  }
};

// On a lambda's own declarator.
void in_lambda(const int x) {
  auto l = [](const int y) __pre(y > 0) __post(y > 0) {
    __contract_assert(y > 0);
    return y;
  };
  (void)l(x);
}

// Inside a lambda written in a contract predicate that captures NOTHING,
// naming a global instead. Mirrors the same pair in GCC's
// contract-assert-alt-spelling.C.
int g_alt_spelling_n = 0;

void in_predicate_lambda_no_capture(const int x)
    __pre([] { __contract_assert(g_alt_spelling_n >= 0); return true; }()) {
  (void)x;
}

// Inside a lambda written in a contract predicate, the most indirect route.
// The lambda captures, and the nested assert names the capture -- a shape
// that has ICEd GCC, so pinning that Clang handles it is worth a row of its
// own.
void in_predicate_lambda(const int x)
    __pre([x] { __contract_assert(x >= 0); return x > 0; }()) {}

// Mixed spellings on one declaration, to be sure recognising one does not
// disturb the other.
int mixed(const int x) pre(x > 0) __post(r : r > 0) {
  contract_assert(x > 0);
  __contract_assert(x != 0);
  return x;
}
