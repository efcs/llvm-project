// RUN: // RUN: %clang_cc1 -std=c++2a  -fcontracts %s -fsyntax-only
// XFAIL: *


template <class T>
auto bar(T x) post(r : ++r != 42) {
return x;
}
template auto bar(int);