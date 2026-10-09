// RUN: %clang_cc1 -std=c++26 -verify -fcontracts -fsyntax-only %s

// [dcl.contract.func]'s restrictions on a parameter odr-used by a
// postcondition -- it must be const, and must not have array or function type
// -- apply to a lambda's call operator like any other function.  Nothing ran
// them for one: the checks hang off ActOnFunctionDeclarator, and a lambda's
// operator() is built by SemaLambda and never reaches it.
//
// A *generic* lambda was diagnosed anyway, because the instantiation path runs
// the same checks, so only the non-generic case went unchecked.  That is why
// this first looked like a missing const rule rather than a missing check --
// the array and function-type rows were equally missing.
//
// GCC diagnoses every row below.

namespace non_generic {

auto non_const = [](int b)         // expected-note {{parameter of type 'int' is declared here}}
    post(b > 0) { return b; };     // expected-error {{parameter 'b' referenced in contract postcondition must be declared const}}

auto array = [](const int a[4])    // expected-note {{parameter of type 'const int[4]' is declared here}}
    post(a[0] > 0) { return a[0]; }; // expected-error {{parameter 'a' referenced in contract postcondition cannot have an array type}}

auto function = [](int (*f)())     // expected-note {{parameter of type 'int (*)()' is declared here}}
    post(f() > 0) { return f(); }; // expected-error {{parameter 'f' referenced in contract postcondition cannot have a function type}}

// A reference parameter is exempt -- the rule is about non-reference
// parameters -- and so is a const one.
auto by_ref = [](int &b) post(b > 0) { return b; };
auto by_const = [](const int b) post(b > 0) { return b; };

// The result binding alone is always fine.
auto result_only = [](int b) post(r : r > 0) { return b; };

// A precondition may name a non-const value parameter: the rule is a
// postcondition rule.
auto in_pre = [](int b) pre(b > 0) { return b; };

} // namespace non_generic

namespace generic {

// This one is diagnosed via the instantiation path rather than the
// primary-parse path.  Kept so the two cannot diverge.
auto non_const = [](auto b)        // expected-note {{parameter of type 'int' is declared here}}
    post(b > 0) { return b; };     // expected-error {{parameter 'b' referenced in contract postcondition must be declared const}}

int use() { return non_const(1); } // expected-note {{in instantiation of}}

} // namespace generic

namespace nested {

// A lambda inside a lambda: the inner call operator is completed by the same
// path, so it is checked too.
auto outer = [] {
  auto inner = [](int b)         // expected-note {{parameter of type 'int' is declared here}}
      post(b > 0) { return b; }; // expected-error {{parameter 'b' referenced in contract postcondition must be declared const}}
  return inner(1);
};

} // namespace nested
