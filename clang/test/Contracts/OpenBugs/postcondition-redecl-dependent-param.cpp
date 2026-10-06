// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// XFAIL: *
//
// OPEN BUG (sema-templates): [dcl.contract.func]/7 requires the odr-used
// parameter to be const on "all declarations of f".  When the offending
// declaration's parameter type is dependent the check only looks at the merged
// (surviving) parameter list, so a `T a` declaration carrying the postcondition
// is not diagnosed once a `T const a` definition follows it.
//   Expected (P2900R14 [dcl.contract.func]/7): errors in namespaces Reported
//     and ThreeDeclarations.
//   Actual: no diagnostic (NonDependent, BothConst, ConstTemplateArgument,
//     Reference, NotOdrUsed behave correctly).

// [dcl.contract.func]: a parameter a postcondition odr-uses must have const
// type on that declaration "and the corresponding parameter on all
// declarations of f".
//
// A declaration that violates it can be gone by the time the question can be
// answered.  Redeclarations are merged and one set of parameters survives, and
// with a DEPENDENT parameter type there is nothing to judge until the template
// arguments are known -- by which point the offending declaration no longer
// exists.  So the check consults the pattern's other declarations and
// substitutes their written parameter types.
//
// Mirror of gcc/testsuite/g++.dg/contracts/cpp26/pr127196.C (GCC PR127196,
// fixed there).

// The report's own case: the contract is on the non-const declaration, the
// definition is const.  This is the shape that is accepted if the check reads
// only the surviving merged parameters.
namespace Reported {
template <typename T>
void f(T a)  // expected-note {{parameter of type 'int' is declared here}}
    post(a); // expected-error {{parameter 'a' referenced in contract postcondition must be declared const}}
template <typename T> void f(T const a) {}
template void f<int>(int);
// expected-note@-1 {{in instantiation of function template specialization 'Reported::f<int>' requested here}}
}

// CONTROL, non-dependent: the same shape with concrete types was always
// caught, on the contract's own declaration, when it was written.  This is
// what says the gap was specific to dependence.
namespace NonDependent {
void f(int a)  // expected-note {{parameter of type 'int' is declared here}}
    post(a);   // expected-error {{parameter 'a' referenced in contract postcondition must be declared const}}
void f(const int a) {}
}

// Both const: WELL-FORMED, and the check must not over-reject it.
namespace BothConst {
template <typename T> void f(T const a) post(a);
template <typename T> void f(T const a) {}
template void f<int>(int);
}

// The corner that rules out deciding this by comparing written qualifiers.
// The declarations disagree about writing `const`, but T is deduced as a const
// type, so BOTH parameters are const after substitution and the program is
// well-formed.  Only substituting answers this.
namespace ConstTemplateArgument {
template <typename T> void f(T a) post(a);
template <typename T> void f(T const a) {}
template void f<const int>(const int);
}

// Never instantiated: nothing to substitute, so nothing is diagnosed.
namespace NeverInstantiated {
template <typename T> void f(T a) post(a);
template <typename T> void f(T const a) {}
}

// A reference parameter is outside the rule however the declarations differ.
namespace Reference {
template <typename T> void f(T &a) post(a);
template <typename T> void f(T &a) {}
template void f<int>(int &);
}

// Three declarations with the offending one in the middle, so the check has to
// look past more than a single merge.
namespace ThreeDeclarations {
template <typename T>
void f(T const a) post(a); // expected-error {{parameter 'a' referenced in contract postcondition must be declared const}}
template <typename T>
void f(T a); // expected-note {{parameter of type 'int' is declared here}}
template <typename T> void f(T const a) {}
template void f<int>(int);
// expected-note@-1 {{in instantiation of function template specialization 'ThreeDeclarations::f<int>' requested here}}
}

// A parameter the predicate does NOT odr-use is unconstrained, however the
// declarations differ.  Here `a` differs and is not named; `b` is named and
// is const on both.
namespace NotOdrUsed {
template <typename T> void f(T a, T const b) post(b);
template <typename T> void f(T const a, T const b) {}
template void f<int>(int, int);
}
