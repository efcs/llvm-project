// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -emit-llvm -o /dev/null -DNO_ERRORS %s

// A generic lambda within a postcondition which captures (or otherwise refers
// to) the result binding of the function: instantiating its call operator
// used to assert in LocalInstantiationScope::findInstantiationOf because a
// result binding is a local entity of the function which is not a variable.

int by_copy(const int v) post(r: [r](auto q) { return r == q; }(v)) { return v; }
int by_ref(const int v) post(r: [&r](auto q) { return r == q; }(v)) { return v; }
int by_default(const int v) post(r: [=](auto q) { return r == q; }(v)) { return v; }
int by_ref_default(const int v) post(r: [&](auto q) { return r == q; }(v)) { return v; }
int explicit_tpl(const int v) post(r: [r]<class Q>(Q q) { return r == q; }(v)) { return v; }
int explicit_obj(const int v) post(r: [r, v](this auto self) { return r == v; }()) { return v; }
int uncaptured(const int v) post(r: [](auto q) { return sizeof(r) == sizeof(q); }(v)) { return v; }
int unevaluated(const int v) post(r: [](auto q) { return __is_same(decltype(r), int) && __is_same(decltype((r)), const int &) && q; }(v)) { return v; }
int not_called(const int v) post(r: ([r](auto q) { return r == q; }, true)) { return v; } // expected-warning {{left operand of comma operator has no effect}}
int nested(const int v) post(r: [r](auto q) { return [r, q] { return r == q; }(); }(v)) { return v; }
int nested_generic(const int v) post(r: [r](auto q) { return [r](auto p) { return r == p; }(q); }(v)) { return v; }

// The copy of the result binding is const in a non-mutable lambda, and not
// const in a mutable one; a reference capture refers to the (const) result
// binding itself.
int types(const int v) post(r: [r](auto q) {
  static_assert(__is_same(decltype(r), int));
  static_assert(__is_same(decltype((r)), const int &));
  return r == q;
}(v)) { return v; }
int types_mutable(const int v) post(r: [r](auto q) mutable {
  static_assert(__is_same(decltype((r)), int &));
  r = q;
  return r == q;
}(v)) { return v; }
int types_ref(const int v) post(r: [&r](auto q) {
  static_assert(__is_same(decltype((r)), const int &));
  return r == q;
}(v)) { return v; }

// Within a function template, the result binding of the instantiation is the
// one the lambda refers to.
template <class T> T tpl(const T v) post(r: [r](auto q) { return r == q; }(v)) { return v; }
template <class T> T tpl_ref(const T v) post(r: [&r](auto q) { return r == q; }(v)) { return v; }
template <class T> T tpl_uncaptured(const T v) post(r: [](auto q) { return sizeof(r) == sizeof(q); }(v)) { return v; }
template <class T> auto tpl_deduced(const T v) post(r: [r](auto q) { return r == q; }(v)) { return v; }
template <class T> struct CT {
  T m;
  auto get() const post(r: [r, this](auto q) { return r == m && q; }(true)) { return m; }
  template <class U> auto conv(const U u) const post(r: [&](auto q) { return r == u && q; }(true)) { return static_cast<T>(u); }
};

// Within a lambda.
auto lam = [](const int v) post(r: [r](auto q) { return r == q; }(v)) { return v; };
auto lam_generic = [](const auto v) post(r: [r](auto q) { return r == q; }(v)) { return v; };

void instantiate() {
  tpl(1); tpl_ref(1); tpl_uncaptured(1); tpl_deduced(1);
  CT<int> c{1}; c.get(); c.conv(2.0);
  lam(1); lam_generic(1); lam_generic(1.0);
}

#ifndef NO_ERRORS
// The result binding is const outside of a copy-capturing lambda.
int modify(const int v) post(r: [&r](auto q) { r = q; return true; }(v)) { return v; } // expected-error {{cannot assign to result name 'r' because it is considered 'const' inside of a contract}} \
                                                                                        // expected-note {{in instantiation of function template specialization 'modify(const int)::(lambda)::operator()<int>' requested here}}
int modify_copy(const int v) post(r: [r](auto q) { r = q; return true; }(v)) { return v; } // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}} \
                                                                                            // expected-note {{in instantiation of function template specialization 'modify_copy(const int)::(lambda)::operator()<int>' requested here}}
#endif
