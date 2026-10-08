// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts
//
// The function contract assertions of a function template that is DECLARED
// BUT NEVER DEFINED are instantiated when the specialization is odr-used, so
// [dcl.contract.func]/7 (a non-reference parameter odr-used by a postcondition
// shall have const type) is checked for a dependent parameter type.
// See also contracts-needed-on-odr-use.cpp.

// [dcl.contract.func]/7 applied to a DEPENDENT parameter type.
//
// The rule is about a function: a specialization whose non-reference parameter
// deduces to a non-const type and is odr-used by a postcondition predicate is
// ill-formed, even though the template itself is fine, a valid specialization
// existing.
//
// Every function below is DECLARED but not DEFINED, which is the whole point.
// Contract assertions are needed "when the function is odr-used
// ([basic.def.odr]) or the function is defined" ([dcl.contract.func]/9), so a
// contract specifier must be instantiated along BOTH paths.  Instantiating
// only on definition leaves a declaration-only template that is merely called
// holding a dependent predicate forever, and /7 -- which correctly declines
// to judge a dependent parameter type -- never gets a concrete one to judge.
// The same templates WITH bodies are diagnosed correctly either way, which is
// what makes the gap look like a missing check rather than a missing
// instantiation.
//
// This is not specific to deducing this: deduced_ordinary and InClassTemplate
// below have no explicit object parameter at all.
//
// GCC mirror: g++.dg/contracts/cpp26/deducing-this-postcondition-param.C

struct S {
  int x = 0;
};

// A deduced explicit object parameter, by value.  Deduction from a by-value
// parameter drops the argument's top-level cv-qualification, so Self deduces
// to the unqualified class type and the parameter is non-const.
struct DeducedByValue : S {
  // expected-note@+2 {{parameter of type 'DeducedByValue' is declared here}}
  // expected-error@+1 {{parameter 'self' referenced in contract postcondition must be declared const}}
  template <class Self> void f(this Self self) post(self.x == 0);
};

void use_deduced_by_value() {
  DeducedByValue a;
  a.f(); // expected-note {{in instantiation of function template specialization 'DeducedByValue::f<DeducedByValue>' requested here}}
}

// The same, called on a const object: deduction still drops the const, so this
// is the SAME specialization, already diagnosed above, and says nothing more.
void use_deduced_by_value_on_const() {
  const DeducedByValue a;
  a.f();
}

// CONTROL, no explicit object parameter: an ordinary function template whose
// by-value parameter type is deduced non-const.
// expected-note@+2 {{parameter of type 'S' is declared here}}
// expected-error@+1 {{parameter 't' referenced in contract postcondition must be declared const}}
template <class T> void deduced_ordinary(T t) post(t.x == 0);

void use_deduced_ordinary() {
  S s;
  deduced_ordinary(s); // expected-note {{in instantiation of function template specialization 'deduced_ordinary<S>' requested here}}
}

// CONTROL, dependent but not deduced: a class template's member with a
// by-value parameter of the template parameter type.
template <class T> struct InClassTemplate {
  // expected-note@+2 {{parameter of type 'S' is declared here}}
  // expected-error@+1 {{parameter 't' referenced in contract postcondition must be declared const}}
  void f(T t) post(t.x == 0);
};

void use_in_class_template() {
  InClassTemplate<S> x;
  S s;
  x.f(s); // expected-note {{in instantiation of member function 'InClassTemplate<S>::f' requested here}}
}

// An explicit object parameter of the enclosing class template's own type.
template <class T> struct ExplicitObjectInClassTemplate : S {
  // expected-note@+2 {{parameter of type 'ExplicitObjectInClassTemplate<int>' is declared here}}
  // expected-error@+1 {{parameter 'self' referenced in contract postcondition must be declared const}}
  void f(this ExplicitObjectInClassTemplate self) post(self.x == 0);
};

void use_explicit_object_in_class_template() {
  ExplicitObjectInClassTemplate<int> x;
  x.f(); // expected-note {{in instantiation of member function 'ExplicitObjectInClassTemplate<int>::f' requested here}}
}

// A declaration-only template that is odr-used and IS well-formed stays quiet.
struct DeducedConst : S {
  template <class Self> void f(this const Self self) post(self.x == 0);
};

void use_deduced_const() {
  DeducedConst a;
  a.f();
}

// A declaration-only template that is never odr-used is never instantiated,
// and a valid specialization exists, so it is not diagnosed.
struct NeverUsed : S {
  template <class Self> void f(this Self self) post(self.x == 0);
};
