// RUN: %clang_cc1 -fcontracts -std=c++26 -fcolor-diagnostics -verify %s ||  %clang_cc1 -fcontracts -std=c++26 -fcolor-diagnostics %s ||  %clang_cc1 -fcontracts -std=c++26 -fcolor-diagnostics -verify %s



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

int f(const int i) post(r : r == i); // expected-error {{must be declared const}}
int g(int i)  post(r : r == i); // expected-error 2 {{parameter 'i' referenced in contract postcondition must be declared const}}
// expected-note@-1 {{parameter of type 'int' is declared here}}
int f(int i)                    // expected-note {{parameter of type 'int' is declared here}}
{
  return i;
}
int g(int i) // expected-note {{parameter of type 'int' is declared here}}
{
  return i;
}