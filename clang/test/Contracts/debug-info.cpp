// RUN: %clangxx -fcontracts -fcontract-evaluation-semantic=enforce -g -std=c++26 -c  %s -o /dev/null

int foo(const int x) post(x != 0) { return x; }


struct S {
  S(const int x) pre(x != 0) post(x != 1) : x(x) {}
  ~S() pre(x != 0) post(true) {}
  int x;
};