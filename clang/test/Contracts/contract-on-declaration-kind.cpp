// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// Declarations which are not function declarations (in the sense of
// [dcl.contract.func]) cannot have a function-contract-specifier-seq, even
// though their declarator looks like a function declarator.

template <class T> T f(const T x) pre(x > T(0)) { return x; }

// [temp.explicit]: an explicit instantiation declares a specialization, which
// is not a first declaration ([dcl.contract.func]p4); it used to be accepted
// and its contracts silently ignored.
template int f<int>(const int x) pre(x > 1); // expected-error {{an explicit instantiation cannot have function contract specifiers}}
extern template long f<long>(const long x) pre(x > 1); // expected-error {{an explicit instantiation cannot have function contract specifiers}}
template short f<short>(const short x) pre(x > T(0)); // expected-error {{an explicit instantiation cannot have function contract specifiers}}
template char f<char>(const char x) post(r: r > 0) pre(true); // expected-error {{an explicit instantiation cannot have function contract specifiers}}

template <class T> struct S {
  T m(const T x) pre(x > T(0)) { return x; }
  static T sm(const T x) pre(x > T(0)) { return x; }
};
template int S<int>::m(const int x) pre(x > 0); // expected-error {{an explicit instantiation cannot have function contract specifiers}}
template int S<int>::sm(const int x) pre(x > 0); // expected-error {{an explicit instantiation cannot have function contract specifiers}}
template struct S<long>;

// The explicit instantiation itself is still performed.
template unsigned f<unsigned>(const unsigned x);
template unsigned S<unsigned>::m(const unsigned x);
unsigned use(unsigned u) { return f(u) + S<unsigned>().m(u); }

// [temp.deduct.guide]: a deduction-guide is not a function declaration; it used
// to be accepted silently.
template <class T> struct G { G(T); };
template <class T> G(T) -> G<T> pre(true); // expected-error {{a deduction guide cannot have function contract specifiers}}
template <class T> G(T *) -> G<T> post(r: true) pre(true); // expected-error {{a deduction guide cannot have function contract specifiers}}
G(char) -> G<int> pre(true); // expected-error {{a deduction guide cannot have function contract specifiers}}
G(long) -> G<long>;

struct Outer {
  template <class T> struct M { M(T); };
  template <class T> M(T) -> M<T> pre(true); // expected-error {{a deduction guide cannot have function contract specifiers}}
  template <class T> M(T *) -> M<T>;
};

void guides() {
  G g1(1);
  G g2(1L);
  Outer::M m(1);
}
