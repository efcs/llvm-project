// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify -fcontract-evaluation-semantic=observe %s

// The attribute-specifier-seq which follows 'pre', 'post' or 'contract_assert'
// appertains to the contract assertion. Only the attributes which are meaningful
// for a contract assertion are accepted there.

namespace Accepted {
int f(int x)
  pre [[clang::contract_group("a")]] (x > 0)
  pre [[clang::contract_message("positive")]] (x > 0)
  pre [[clang::contract_message("positive", false)]] (x > 0)
  pre [[clang::contract_message("positive", true)]] (x > 0)
  pre [[clang::contract_group("a.b"), clang::contract_message("m")]] (x > 0)
  pre [[clang::contract_group("a")]] [[clang::contract_message("m")]] [[]] (x > 0)
  post [[clang::contract_group("a")]] (r : r > 0) {
  contract_assert [[clang::contract_group("a")]] (x > 0);
  contract_assert [[clang::contract_message("m", 1 + 1 == 2)]] (x > 0);
  return x;
}

template <class T> T t(T x) pre [[clang::contract_group("a")]] (x > T{}) {
  contract_assert [[clang::contract_message("m")]] (x > T{});
  return x;
}
int use = t(1);

// Unknown attributes are ignored, as elsewhere.
int unknown(int x) pre [[foo::bar]] (x > 0); // expected-warning {{unknown attribute 'foo::bar' ignored}}
} // namespace Accepted

namespace Rejected {
// Statement attributes do not appertain to a contract assertion; a function
// contract assertion is not even a statement.
void f1(int x) pre [[clang::nomerge]] (x > 0); // expected-error {{'clang::nomerge' attribute cannot be applied to a contract assertion}}
void f2(int x) pre [[clang::noinline]] (x > 0); // expected-error {{'clang::noinline' attribute cannot be applied to a contract assertion}}
void f3(int x) pre [[clang::always_inline]] (x > 0); // expected-error {{'clang::always_inline' attribute cannot be applied to a contract assertion}}
void f4(int x) pre [[likely]] (x > 0); // expected-error {{'likely' attribute cannot be applied to a contract assertion}}
void f5(int x) pre [[unlikely]] (x > 0); // expected-error {{'unlikely' attribute cannot be applied to a contract assertion}}
void f6(int x) pre [[clang::annotate("a")]] (x > 0); // expected-error {{'clang::annotate' attribute cannot be applied to a contract assertion}}
void f7(int x) pre [[clang::suppress]] (x > 0); // expected-error {{'clang::suppress' attribute cannot be applied to a contract assertion}}
void f8(int x) pre [[assume(x > 0)]] (x > 0); // expected-error {{'assume' attribute cannot be applied to a contract assertion}}
void f9(int x) pre [[fallthrough]] (x > 0); // expected-error {{'fallthrough' attribute cannot be applied to a contract assertion}}
// Nor do declaration attributes.
void f10(int x) pre [[maybe_unused]] (x > 0); // expected-error {{'maybe_unused' attribute cannot be applied to a contract assertion}}
void f11(int x) pre [[deprecated]] (x > 0); // expected-error {{'deprecated' attribute cannot be applied to a contract assertion}}
void f12(int x) pre [[nodiscard]] (x > 0); // expected-error {{'nodiscard' attribute cannot be applied to a contract assertion}}
void f13(int x) pre [[gnu::unused]] (x > 0); // expected-error {{'gnu::unused' attribute cannot be applied to a contract assertion}}
int f14(int x) post [[noreturn]] (r : r > 0); // expected-error {{'noreturn' attribute cannot be applied to a contract assertion}}

void g(int x) {
  contract_assert [[clang::nomerge]] (x > 0); // expected-error {{'clang::nomerge' attribute cannot be applied to a contract assertion}}
  contract_assert [[likely]] (x > 0); // expected-error {{'likely' attribute cannot be applied to a contract assertion}}
  contract_assert [[maybe_unused]] (x > 0); // expected-error {{'maybe_unused' attribute cannot be applied to a contract assertion}}
}

// Several errors in one attribute-specifier-seq are all diagnosed.
void h(int x) pre [[likely, clang::nomerge]] (x > 0); // expected-error {{'likely' attribute cannot be applied to a contract assertion}} \
                                                      // expected-error {{'clang::nomerge' attribute cannot be applied to a contract assertion}}

template <class T> T t(T x) pre [[likely]] (x > T{}); // expected-error {{'likely' attribute cannot be applied to a contract assertion}}
} // namespace Rejected

namespace Arguments {
void a1(int x) pre [[clang::contract_group]] (x > 0); // expected-error {{'clang::contract_group' attribute takes one argument}}
void a2(int x) pre [[clang::contract_group("a", "b")]] (x > 0); // expected-error {{'clang::contract_group' attribute takes one argument}}
void a3(int x) pre [[clang::contract_group(1)]] (x > 0); // expected-error {{expected string literal as argument of 'contract_group' attribute}}
void a4(int x) pre [[clang::contract_message]] (x > 0); // expected-error {{'clang::contract_message' attribute takes at least 1 argument}}
void a5(int x) pre [[clang::contract_message("a", 1, 2)]] (x > 0); // expected-error {{'clang::contract_message' attribute takes no more than 2 arguments}}
void a6(int x) pre [[clang::contract_message(1)]] (x > 0); // expected-error {{expected string literal as argument of 'contract_message' attribute}}
void a7(int x) pre [[clang::contract_message("a", x)]] (x > 0); // expected-error {{argument to 'clang::contract_message' attribute must be a boolean constant expression}}
void a8(int x) pre [[clang::contract_message("a", "b")]] (x > 0); // expected-error {{argument to 'clang::contract_message' attribute must be a boolean constant expression}}
template <class T> void a9(T x) pre [[clang::contract_message("a", sizeof(T) > 1)]] (x > 0); // expected-error {{argument to 'clang::contract_message' attribute must be a boolean constant expression}}

void d1(int x) pre [[clang::contract_group("a")]] // expected-note {{previous attribute is here}}
                   [[clang::contract_group("b")]] (x > 0); // expected-error {{attribute 'clang::contract_group' cannot appear more than once on a contract specifier}}
void d2(int x) pre [[clang::contract_message("a"), clang::contract_message("b")]] (x > 0); // expected-error {{attribute 'clang::contract_message' cannot appear more than once on a contract specifier}} \
                                                                                           // expected-note {{previous attribute is here}}
} // namespace Arguments

namespace Position {
void f(int x) {
  // The attribute-specifier-seq of a statement appertains to the statement.
  [[likely]] contract_assert(x > 0);
  [[clang::nomerge]] contract_assert(x > 0); // expected-warning {{'clang::nomerge' attribute is ignored because there exists no call expression inside the statement}}
  [[clang::suppress]] contract_assert(x > 0);
  // The attributes of a contract assertion cannot appear there.
  [[clang::contract_group("a")]] contract_assert(x > 0); // expected-error {{'clang::contract_group' attribute cannot appear here; it appertains to a contract assertion and must follow the 'contract_assert' keyword}}
  [[clang::contract_message("a")]] contract_assert(x > 0); // expected-error {{'clang::contract_message' attribute cannot appear here; it appertains to a contract assertion and must follow the 'contract_assert' keyword}}
  // ... nor on other statements.
  [[clang::contract_group("a")]] return; // expected-error {{'clang::contract_group' attribute only applies to contract assertions}}
}
// ... nor on declarations.
[[clang::contract_message("a")]] void g(); // expected-error {{'clang::contract_message' attribute cannot be applied to a declaration}}
} // namespace Position
