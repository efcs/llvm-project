// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts -Wno-unused-value
// XFAIL: *
//
// OPEN BUG (sema, postcondition parameters): a parameter named as the
// DISCARDED left operand of a comma operator in a postcondition is not
// odr-used ([basic.def.odr]/5.3: the lvalue-to-rvalue conversion is not
// applied to a discarded-value expression of non-volatile type,
// [expr.context]/2), so [dcl.contract.func]/7 does not apply to it.
//   Expected (P2900R14 [dcl.contract.func]/7 + [basic.def.odr]/5): no
//     diagnostic for f, g, h, cls, nested, exempt_param.
//   Actual: "parameter 'b' referenced in contract postcondition must be
//     declared const" for each of them -- the check treats every potentially
//     evaluated reference as an odr-use.

// PR126897: a parameter named as the discarded left operand of a comma is not
// odr-used, so [dcl.contract.func]/7 does not reach it.
//
// [basic.def.odr]/5: a variable x named by a potentially-evaluated expression
// E is odr-used "unless ... x is a variable of non-reference type, and E is an
// element of the set of potential results of a discarded-value expression to
// which the lvalue-to-rvalue conversion is not applied".  [expr.context]
// applies that conversion to a discarded-value expression only when it is a
// glvalue of volatile-qualified type, so for an ordinary parameter it is not
// applied and naming it is not an odr-use.
//
// The AST draws the line where the wording does: in `(b, true)` the operand is
// a bare lvalue DeclRefExpr, while in `(true, b)` the LValueToRValue
// conversion wraps the whole comma, so the potential result there IS
// converted and stays an odr-use.
//
// GCC mirror: g++.dg/contracts/cpp26/pr126897.C

struct S {
  int x;
  bool ok() const;
};

// The report's own case.
void f(bool b) post((b, true)) {}

// The same through an explicit discard.
void g(bool b) post(((void)b, true)) {}

// ... and with a result-name-introducer.
int h(int x) post(r : ((x, true) && r >= 0)) { return x; }

// A parameter of class type, discarded.
void cls(S s) post((s, true)) {}

// Nested commas: only the last operand is the value, and `(x, y, true)` parses
// as `((x, y), true)`, so the exemption has to recurse.
int nested(int x, int y) post(r : ((x, y, true) && r >= 0)) { return x + y; }

// The parameter is discarded in one place and odr-used in another: the
// odr-use still counts.
// expected-note@+2 {{parameter of type 'int' is declared here}}
// expected-error@+1 {{parameter 'x' referenced in contract postcondition must be declared const}}
int both(int x) post(r : ((x, true) && x == r)) { return x; }

// The RIGHT operand of a comma is the value of the expression, so it is
// odr-used.
// expected-note@+2 {{parameter of type 'bool' is declared here}}
// expected-error@+1 {{parameter 'b' referenced in contract postcondition must be declared const}}
bool rhs(bool b) post((true, b)) { return b; }

// A discarded CALL still odr-uses its arguments -- the parameter is not a
// potential result of the call, it is an argument to it.
bool use(int);
// expected-note@+2 {{parameter of type 'int' is declared here}}
// expected-error@+1 {{parameter 'x' referenced in contract postcondition must be declared const}}
int arg(int x) post(r : ((use(x), true) && r >= 0)) { return x; }

// Likewise a discarded member call on the parameter.
// expected-note@+2 {{parameter of type 'S' is declared here}}
// expected-error@+1 {{parameter 's' referenced in contract postcondition must be declared const}}
void memcall(S s) post((s.ok(), true)) {}

// A class-typed result alongside the discarded parameter (moved here from
// Sema/postcondition-discarded-operand-dependent.cpp).
struct Plain { int id; };
Plain exempt_param(int b) post(r : (b, r.id != 0)) { return Plain{b}; }

// An unevaluated operand is not an odr-use either, and never was.
void unevaluated(int x) post(sizeof(x) > 0) {}

// A reference parameter is outside the rule entirely, discarded or not.
void ref(int &r) post((r, true)) {}

// CONTROL: the ordinary odr-use still fires.
// expected-note@+2 {{parameter of type 'int' is declared here}}
// expected-error@+1 {{parameter 'x' referenced in contract postcondition must be declared const}}
int plain(int x) post(r : x == r) { return x; }

// CONTROL: and is satisfied by const.
int constant(const int x) post(r : x == r) { return x; }
