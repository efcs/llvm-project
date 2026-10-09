// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wall -Wextra -Wno-unused -fcontract-evaluation-semantic=ignore -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wall -Wextra -Wno-unused -fcontract-evaluation-semantic=observe -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wall -Wextra -Wno-unused -fcontract-evaluation-semantic=enforce -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wall -Wextra -Wno-unused -fcontract-evaluation-semantic=quick_enforce -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wall -Wextra -Wno-unused -fcontract-evaluation-semantic=observe -verify %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wall -Wextra -Wno-unused -fcontract-evaluation-semantic=enforce -verify %s -fexperimental-new-constant-interpreter

// expected-no-diagnostics

// A contract violation is only diagnosed at compile time if it occurs in a
// manifestly constant-evaluated context. Evaluating an expression in order to
// fold it or to analyze it for warnings never diagnoses contract violations:
// the contract assertion is going to be checked at run time, if at all.

constexpr int f(int x) pre(x > 0) post(r : r > 0) {
  contract_assert(x > 0);
  return x;
}
int runtime(int);
void use(int);

void potentially_evaluated(int n) {
  // Initializers of automatic variables that are not usable in constant
  // expressions.
  int a = f(0);
  long b = f(0);
  const long not_integral_constant = f(runtime(0));
  auto [c, d] = (int[2]){f(0), f(0)};

  // Operands that are evaluated in order to diagnose things like division by
  // zero, out of bounds accesses, overflow, or tautological comparisons.
  int array[4] = {};
  use(array[f(0)]);
  use(1 / (f(0) - 1));
  use(f(0) * 100000 * 100000);
  use(1 << f(0));
  if (f(0) == 0)
    use(1);
  if (f(0))
    use(2);
  while (f(0))
    break;
  use(n ? f(0) : 1);
  use(f(0) ?: 3);
  switch (f(0)) {
  default:
    break;
  }
  unsigned char narrow = f(0);
  bool boolean = f(0);
  use(__builtin_constant_p(f(0)));
  use(__builtin_expect(f(0), 1));
  (void)(f(0) && n);
  enum E { e } en = static_cast<E>(f(0));
  char buffer[8];
  __builtin_memset(buffer, 0, f(0));
}

// Unevaluated operands.
int size = sizeof(f(0));
bool nothrow = noexcept(f(0));
decltype(f(0)) declared = 1;
template <class T> auto sfinae(T t) -> decltype(f(0), t);
int substituted = sizeof(sfinae(1));
template <class T> concept C = requires(T t) { f(0); t + f(0); { f(0) } noexcept; };
static_assert(!C<int>);

// Default arguments and default member initializers are evaluated where they
// are used.
int default_argument(int x = f(0));
struct DefaultMemberInit {
  int m = f(0);
};

// Attribute arguments that are evaluated speculatively.
void enable_if(int x) __attribute__((enable_if(x > 0, "")));
void diagnose_if(int x) __attribute__((diagnose_if(f(x), "", "warning")));
void call_attributes() {
  enable_if(f(1));
  diagnose_if(0);
}

// Whether a constexpr function can ever be used in a constant expression
// does not depend on its contract assertions.
constexpr int never_true(int x) pre(false) post(false) {
  contract_assert(false);
  return x;
}
constexpr int not_constant_predicate(int x) pre(runtime(x)) { return x; }
int call_never() { return never_true(1) + not_constant_predicate(1); }
