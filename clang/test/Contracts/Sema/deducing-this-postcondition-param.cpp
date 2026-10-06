// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts

// [dcl.contract.func]/7 applied to an EXPLICIT OBJECT PARAMETER.
//
// An explicit object parameter ([dcl.fct]) is a parameter of the function, so
// the rule "if the predicate of a postcondition assertion of a function f
// odr-uses a non-reference parameter of f, that parameter ... shall have
// const type" governs it exactly as it governs any other by-value parameter.
//
// These are the NON-DEPENDENT shapes, which Clang gets right.  The dependent
// ones are in deducing-this-postcondition-param-dependent.cpp, which is
// XFAILed.
//
// GCC mirror: g++.dg/contracts/cpp26/deducing-this-postcondition-param.C

struct S {
  int x = 0;
};

// A by-value explicit object parameter is subject to the rule.
struct ByValue : S {
  // expected-note@+2 {{parameter of type 'ByValue' is declared here}}
  // expected-error@+1 {{parameter 'self' referenced in contract postcondition must be declared const}}
  void f(this ByValue self) post(self.x == 0);
};

// ... and satisfies it when declared const.
struct ByValueConst : S {
  void f(this const ByValueConst self) post(self.x == 0);
};

// Reference explicit object parameters are exempt, in all three spellings.
struct ByRef : S {
  void f(this ByRef &self) post(self.x == 0);
};

struct ByConstRef : S {
  void f(this const ByConstRef &self) post(self.x == 0);
};

struct ByRvalueRef : S {
  void f(this ByRvalueRef &&self) post(self.x == 0);
};

// The rule is about POSTconditions only.
struct InPrecondition : S {
  void f(this InPrecondition self) pre(self.x == 0);
};

// ... and only about a parameter the predicate actually odr-uses.
struct NotNamed : S {
  void f(this NotNamed self) post(true);
};

// With a result-name-introducer, the explicit object parameter is still
// caught.
struct WithResultName : S {
  // expected-note@+2 {{parameter of type 'WithResultName' is declared here}}
  // expected-error@+1 {{parameter 'self' referenced in contract postcondition must be declared const}}
  int f(this WithResultName self) post(r : r == self.x);
};

// An ordinary parameter alongside a conforming explicit object parameter is
// still checked on its own account, and the diagnostic names that parameter.
struct OrdinaryParameter : S {
  // expected-note@+2 {{parameter of type 'int' is declared here}}
  // expected-error@+1 {{parameter 'n' referenced in contract postcondition must be declared const}}
  void f(this const OrdinaryParameter self, int n) post(self.x == n);
};

// A deduced forwarding-reference explicit object parameter is exempt.
struct DeducedByRef : S {
  template <class Self> void f(this Self &&self) post(self.x == 0);
};

void use_deduced_by_ref() {
  DeducedByRef a;
  a.f();
}

// Const on the parameter itself: Self deduces to the unqualified type and the
// parameter is const.
struct DeducedByValueConst : S {
  template <class Self> void f(this const Self self) post(self.x == 0);
};

void use_deduced_by_value_const() {
  DeducedByValueConst a;
  a.f();
}

// Never instantiated: a valid specialization exists (Self = const T), so the
// template itself is well-formed and no diagnostic is required.
struct NeverInstantiated : S {
  template <class Self> void f(this Self self) post(self.x == 0);
};
