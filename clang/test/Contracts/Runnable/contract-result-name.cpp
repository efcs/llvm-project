// RUN: %clang_cc1 -std=c++2a  -fcontracts %s -fsyntax-only


int main() {
  // TODO(EricWF): Fix me
}


template <class T>
T baz(const T x) post(r : r != 42) {
return x != 42;
}
template int baz(int);


template <class T>
auto bar(T x) post(r : r != 42) {
return x;
}
template auto bar(int);

auto foo(const int x) post(r : r != x) { return x; }
