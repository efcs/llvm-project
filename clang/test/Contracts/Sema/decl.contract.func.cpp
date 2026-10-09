// RUN: %clang_cc1 -fcontracts -std=c++26 -verify %s



class X {
private:
  int m; // expected-note {{declared private here}}

public:
  void f() pre(m > 0);             // OK
  friend void g(X x) pre(x.m > 0); // OK
};
void h(X x) pre(x.m > 0); // expected-error {{'m' is a private member}}
double i;
int j;
auto l1 = [i = j] pre(i > 0) {}; // OK, refers to captured int i

// [dcl.contract.func]p7: a non-reference parameter which is odr-used by a
// postcondition shall be const on all declarations of the function. The
// postconditions belong to the first declaration and are checked once; a
// definition which omits them is checked against them, not re-diagnosed.
int f(const int i) post(r : r == i); // expected-note {{parameter referenced in postcondition of first declaration here}}
int g(int i)  post(r : r == i); // expected-error {{parameter 'i' referenced in contract postcondition must be declared const}}
// expected-note@-1 {{parameter of type 'int' is declared here}}
int f(int i)                    // expected-error {{parameter 'i' of type 'int' must be declared const because it is referenced in a postcondition of the function}}
{
  return i;
}
int g(int i) // no further diagnostic: the first declaration was diagnosed
{
  return i;
}