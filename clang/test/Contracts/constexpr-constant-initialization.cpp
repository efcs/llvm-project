// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=observe -verify=expected,observe %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=ignore -verify=expected,ignore %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=observe -verify=expected,observe %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=enforce -verify=expected,terminating %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -Wno-division-by-zero -fcontract-evaluation-semantic=quick_enforce -verify=expected,terminating %s -fexperimental-new-constant-interpreter

// C++26 [expr.const.init]p4: Whether a variable is constant-initialized is
// determined with all contract assertions using the ignore semantic, so that
// a contract assertion never silently turns constant initialization into
// dynamic initialization or vice versa. If the variable is
// constant-initialized, its initializer is manifestly constant-evaluated and
// its contract assertions are checked.

int runtime(int); // #runtime

constexpr int l(int c) pre(c >= 2) { // #l
  return (c % 2 == 0) ? c / 0 : c; // #l-div
}

namespace standard_example {
// [expr.const.imm] example
const int i0 = l(0); // #i0 dynamic initialization; contract violation or undefined behavior at run time
const int i1 = l(1); // static initialization; value of 1 or contract violation at compile time
// observe-warning@#l {{contract violation with the 'observe' evaluation semantic: precondition 'c >= 2' evaluated to false}}
// observe-note@-2 {{in call to 'l(1)'}}
// terminating-error@-3 {{constant initialization of 'i1' failed when checking its contract assertions}}
// terminating-note-re@#l {{contract violation with the '{{(quick_)?enforce}}' evaluation semantic: precondition 'c >= 2' evaluated to false}}
// terminating-note@-5 {{in call to 'l(1)'}}
// terminating-note@-6 {{'i1' is constant-initialized because its initializer is a constant expression when all contract assertions are ignored}}
const int i2 = l(2); // #i2 dynamic initialization; undefined behavior
const int i3 = l(3); // static initialization; value of 3

// i0 and i2 are not usable in constant expressions; i1 and i3 are.
int a0[i0]; // expected-warning {{variable length arrays in C++ are a Clang extension}} \
            // expected-note {{initializer of 'i0' is not a constant expression}} \
            // expected-note@#i0 {{declared here}} \
            // expected-error {{variable length array declaration not allowed at file scope}}
static_assert(i1 == 1);
int a2[i2]; // expected-warning {{variable length arrays in C++ are a Clang extension}} \
            // expected-note {{initializer of 'i2' is not a constant expression}} \
            // expected-note@#i2 {{declared here}} \
            // expected-error {{variable length array declaration not allowed at file scope}}
static_assert(i3 == 3);
} // namespace standard_example

namespace storage_duration {
// Variables with static or thread storage duration are constant-initialized
// if possible, whether or not they are const.
int g = l(1);
// observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
// observe-note@-2 {{in call to 'l(1)'}}
// terminating-error@-3 {{constant initialization of 'g' failed when checking its contract assertions}}
// terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
// terminating-note@-5 {{in call to 'l(1)'}}
// terminating-note@-6 {{'g' is constant-initialized because}}

thread_local int tl = l(1);
// observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
// observe-note@-2 {{in call to 'l(1)'}}
// terminating-error@-3 {{constant initialization of 'tl' failed when checking its contract assertions}}
// terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
// terminating-note@-5 {{in call to 'l(1)'}}
// terminating-note@-6 {{'tl' is constant-initialized because}}

struct S {
  static inline int member = l(1);
  // observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
  // observe-note@-2 {{in call to 'l(1)'}}
  // terminating-error@-3 {{constant initialization of 'member' failed when checking its contract assertions}}
  // terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
  // terminating-note@-5 {{in call to 'l(1)'}}
  // terminating-note@-6 {{'member' is constant-initialized because}}
};

void f() {
  static int static_local = l(1);
  // observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
  // observe-note@-2 {{in call to 'l(1)'}}
  // terminating-error@-3 {{constant initialization of 'static_local' failed when checking its contract assertions}}
  // terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
  // terminating-note@-5 {{in call to 'l(1)'}}
  // terminating-note@-6 {{'static_local' is constant-initialized because}}

  // A const-qualified variable of integral type is usable in constant
  // expressions if it is constant-initialized.
  const int const_local = l(1);
  // observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
  // observe-note@-2 {{in call to 'l(1)'}}
  // terminating-error@-3 {{constant initialization of 'const_local' failed when checking its contract assertions}}
  // terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
  // terminating-note@-5 {{in call to 'l(1)'}}
  // terminating-note@-6 {{'const_local' is constant-initialized because}}

  // The contract violation was diagnosed where the variable is initialized.
  // Uses of the variable in constant expressions don't diagnose it again.
  static_assert(const_local == 1);
  int array[const_local];

  // The initialization of other automatic variables is not manifestly
  // constant-evaluated. Their contract assertions are checked at run time.
  int local = l(1);
  const long not_constant_initialized = l(0);
  int from_runtime = l(runtime(1));
}
} // namespace storage_duration

namespace required_constant_initialization {
// A variable that is required to be constant-initialized is ill-formed in
// any case if the evaluation of its initializer fails.
constinit int ci = l(1);
// observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
// observe-note@-2 {{in call to 'l(1)'}}
// terminating-error@-3 {{variable does not have a constant initializer}}
// terminating-note@-4 {{required by 'constinit' specifier here}}
// terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
// terminating-note@-6 {{in call to 'l(1)'}}

constexpr int ce = l(1);
// observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
// observe-note@-2 {{in call to 'l(1)'}}
// terminating-error@-3 {{constexpr variable 'ce' must be initialized by a constant expression}}
// terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
// terminating-note@-5 {{in call to 'l(1)'}}

// The contract violation is found before the undefined behavior if the
// contract assertion is checked.
constexpr int ub = l(0);
// ignore-error@-1 {{constexpr variable 'ub' must be initialized by a constant expression}}
// ignore-note@#l-div {{division by zero}}
// ignore-note@-3 {{in call to 'l(0)'}}
// observe-error@-4 {{constexpr variable 'ub' must be initialized by a constant expression}}
// observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
// observe-note@-6 {{in call to 'l(0)'}}
// observe-note@#l-div {{division by zero}}
// observe-note@-8 {{in call to 'l(0)'}}
// terminating-error@-9 {{constexpr variable 'ub' must be initialized by a constant expression}}
// terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
// terminating-note@-11 {{in call to 'l(0)'}}
} // namespace required_constant_initialization

namespace no_trial_evaluation_difference {
// P2900R14 [3.5.12]: A contract assertion must not cause an initializer that
// is evaluated at run time to be evaluated at compile time instead.
constexpr int compute(int n) pre(n == 0 || !__builtin_is_constant_evaluated()) {
  return n == 0 ? 42 : runtime(n);
}
void f() {
  const int i = compute(0); // constant initialization
  const int j = compute(1); // dynamic initialization: the precondition holds at run time
  static_assert(i == 42);
  static int k = compute(1);
}
int g = compute(1);
} // namespace no_trial_evaluation_difference

namespace predicate_changes_the_initializer {
// P2900R14 [3.5.12]: The program is ill-formed if the initializer is a
// constant expression when its contract assertions are ignored, but
// checking the contract assertions makes it a non-constant expression.
constexpr int foo(int i) { return i == 0 ? 0 : runtime(i); } // #foo
constexpr int bar(int *p) pre((*p = 1)) { return foo(*p); }  // #bar
constexpr int baz(int i) { return bar(&i); }                 // #baz
static int x = baz(0);
// observe-error@-1 {{constant initialization of 'x' failed when checking its contract assertions}}
// observe-note@#foo {{non-constexpr function 'runtime' cannot be used in a constant expression}}
// observe-note@#bar {{in call to 'foo(1)'}}
// observe-note@#baz {{in call to 'bar(&i)'}}
// observe-note-re@-5 {{in call to 'baz({{[01]}})'}}
// observe-note@-6 {{'x' is constant-initialized because}}
// observe-note@#runtime {{declared here}}
// terminating-error@-8 {{constant initialization of 'x' failed when checking its contract assertions}}
// terminating-note@#foo {{non-constexpr function 'runtime' cannot be used in a constant expression}}
// terminating-note@#bar {{in call to 'foo(1)'}}
// terminating-note@#baz {{in call to 'bar(&i)'}}
// terminating-note-re@-12 {{in call to 'baz({{[01]}})'}}
// terminating-note@-13 {{'x' is constant-initialized because}}
// terminating-note@#runtime {{declared here}}

// [basic.contract.eval]p8 note 6: The side effects of a predicate can alter
// the value of a constant expression.
constexpr int increment(int i) {
  contract_assert((++const_cast<int &>(i), true));
  return i;
}
const int k = increment(1);
int array[k];
static_assert(sizeof(array) / sizeof(int) == 1); // observe-error {{static assertion failed}} terminating-error {{static assertion failed}} \
                                                 // observe-note {{expression evaluates to '2 == 1'}} terminating-note {{expression evaluates to '2 == 1'}}
} // namespace predicate_changes_the_initializer

namespace class_types {
// The value computed with the contract assertions checked is the value of
// the variable, also if it needs non-trivial cleanup in the AST.
struct Buf {
  int data[4];
  constexpr Buf(int v) pre(v > 0) : data{v, v, v, v} {} // #Buf
};
Buf b1(1);
Buf b0(0);
// observe-warning@#Buf {{precondition 'v > 0' evaluated to false}}
// observe-note@-2 {{in call to 'Buf(0)'}}
// terminating-error@-3 {{constant initialization of 'b0' failed when checking its contract assertions}}
// terminating-note@#Buf {{precondition 'v > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'Buf(0)'}}
// terminating-note@-6 {{'b0' is constant-initialized because}}
const Buf &ref = Buf(0);
// observe-warning@#Buf {{precondition 'v > 0' evaluated to false}}
// observe-note@-2 {{in call to 'Buf(0)'}}
// terminating-error@-3 {{constant initialization of 'ref' failed when checking its contract assertions}}
// terminating-note@#Buf {{precondition 'v > 0' evaluated to false}}
// terminating-note@-5 {{in call to 'Buf(0)'}}
// terminating-note@-6 {{'ref' is constant-initialized because}}
} // namespace class_types

namespace templates {
template <class T> const int tv = l(sizeof(T));
// observe-warning@#l {{precondition 'c >= 2' evaluated to false}}
// observe-note@-2 {{in call to 'l(1)'}}
// terminating-error@-3 {{constant initialization of 'tv<char>' failed when checking its contract assertions}}
// terminating-note@#l {{precondition 'c >= 2' evaluated to false}}
// terminating-note@-5 {{in call to 'l(1)'}}
// terminating-note@-6 {{'tv<char>' is constant-initialized because}}
int use = tv<char> + tv<char[3]>;
// observe-note@-1 {{in instantiation of variable template specialization 'templates::tv<char>' requested here}}
// terminating-note@-2 {{in instantiation of variable template specialization 'templates::tv<char>' requested here}}
} // namespace templates
