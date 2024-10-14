// RUN: %clang_cc1 -fcontracts -std=c++26 -fcolor-diagnostics -verify %s

struct A {}; // trivially copyable
struct B {   // not trivially copyable
  B() {}
  B(const B &) {}
};
template <typename T> T f(T * const ptr) post(r : &r == ptr) { return T{}; }
int main() {
  A a = f(&a); // The postcondition check may fail.
  B b = f(&b); // The postcondition check is guaranteed to succeed.
}

int f(int &p) post(p >= 0)   // OK
    post(r : r >= 0);        // OK
auto g(auto &p) post(p >= 0) // OK
    post(r : r >= 0);        // OK
auto h(int &p) post(p >= 0)  // expected-note {{function return type declared here}}
    post(r : r >= 0);        // expected-error {{postcondition with result name 'r' cannot appear on a function declaration with deduced return type unless it is a definition}}
auto i(int &p) post(p >= 0)  // OK
    post(r : r >= 0)         // OK
{
  return p = 0;
}