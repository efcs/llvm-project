// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts -Wno-unused-value

// A dependent discarded comma operand in a postcondition, mirrored from
// g++.dg/contracts/cpp26/postcondition-discarded-operand-dependent.C.
//
// GCC segfaulted on every shape below that its front end builds with
// build_min_nt -- a dependent member access, call, subscript or ?: -- because
// its [dcl.contract.func]/7 walk asked whether the discarded operand had void
// type before establishing that it had a type at all.  The predicate is
// type-dependent while parsed, since the result binding's type is auto until
// the return type is deduced, and build_min_nt leaves TREE_TYPE null.
//
// Clang has never had this: its postcondition parameter check walks the
// finished predicate through the ordinary Expr hierarchy, where a dependent
// expression is still a typed node.  This file is a regression pin for that
// -- see OpenBugs/pr126897.cpp for the underlying odr-use rule.

struct Inner { int x; };

struct Plain {
  int id;
  int arr[4];
  Inner in;
  int get() const { return id; }
};

int use(Plain);
int use_scalar(int);

// The shapes that crashed GCC.  None names a parameter, so
// [dcl.contract.func]/7 has nothing to say and all are well-formed.

Plain member_access(int b) post(r : (r.id, true)) { return Plain{b}; }

Plain nested_member(int b) post(r : (r.in.x, true)) { return Plain{b}; }

Plain member_call(int b) post(r : (r.get(), true)) { return Plain{b}; }

Plain subscript(int b) post(r : (r.arr[0], true)) { return Plain{b}; }

Plain free_call(int b) post(r : (use(r), true)) { return Plain{b}; }

// A scalar result reaches the same dependent-call shape, which is what shows
// the class type was never the ingredient on the GCC side either.
int scalar_call(int b) post(r : (use_scalar(r), true)) { return b; }

Plain *via_arrow(int b) post(r : (r->id, true)) { return nullptr; }

auto deduced_return(int b) post(r : (r.id, true)) { return Plain{b}; }

Plain declared_only(int b) post(r : (r.id, true));

// A ?: as the discarded operand.  Its condition is evaluated, so a non-const
// parameter named there is odr-used and must be diagnosed; a const one is fine.
Plain cond_const_param(int const b) post(r : (b ? r.id : r.in.x, true)) {
  return Plain{b};
}

Plain cond_plain_param(int b) post(r : (b ? r.id : r.in.x, true)) { // expected-error {{parameter 'b' referenced in contract postcondition must be declared const}} expected-note {{parameter of type 'int' is declared here}}
  return Plain{b};
}

// A lambda instantiated as part of a template: a separate code path on GCC,
// and the second face of the same crash there.
template <class T> Plain in_template(T a) {
  auto inner = [](int b) post(r : (r.id, true)) { return Plain{b}; };
  return inner((int)a);
}
Plain instantiate_it() { return in_template(1); }

// Repeating a contract on both declarations of a member function is
// ill-formed, [dcl.contract.func]/6.  GCC segfaulted instead of saying so.
struct Redeclared {
  Plain f(int b); // expected-note {{previously declared without contracts here}}
};
Plain Redeclared::f(int b) post(r : (r.id, true)) { // expected-error {{method out-of-line definition differs in contract specifier sequence}}
  return Plain{b};
}

// CONTROLS -- well-formed on both compilers, and never crashed GCC.

int bare_scalar_result(int b) post(r : (r, true)) { return b; }
Plain bare_class_result(int b) post(r : (r, true)) { return Plain{b}; }

int scalar_arith(int b) post(r : (r + 0, true)) { return b; }
int scalar_negate(int b) post(r : (-r, true)) { return b; }
Plain member_arith(int b) post(r : (r.id + 0, true)) { return Plain{b}; }
Plain member_cast_void(int b) post(r : ((void)r.id, true)) { return Plain{b}; }

struct FirstDecl {
  Plain f(int b) post(r : (r.id, true));
  static Plain g(int b) post(r : (r.id, true));
};
Plain FirstDecl::f(int b) { return Plain{b}; }
Plain FirstDecl::g(int b) { return Plain{b}; }

// The odr-use rule itself must still land on both sides of the comma: a
// parameter that is an argument to a call in the discarded operand is
// odr-used.  (The exempt case -- the parameter itself as the discarded
// operand -- is in OpenBugs/pr126897.cpp.)
Plain odr_used_param(int b) post(r : (use_scalar(b), true)) { // expected-error {{parameter 'b' referenced in contract postcondition must be declared const}} expected-note {{parameter of type 'int' is declared here}}
  return Plain{b};
}
