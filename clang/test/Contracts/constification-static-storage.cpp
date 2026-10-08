// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [expr.prim.id.unqual]p7: a variable declared outside of a contract assertion
// is const within its predicate. A lambda-expression within the predicate with
// a capture-default of '=' makes a copy of the local entities it uses, and an
// id-expression in its body names the copy ([expr.prim.id.unqual]p4) -- but
// only an entity with automatic storage duration can be captured; a static or
// thread-local variable, or a variable declared extern, is still the entity
// declared outside of the contract assertion.

int g = 0;
void f(int p) {
  static int s = 0;
  thread_local int t = 0;
  extern int e;
  int a = 0;
  contract_assert([=] { s = 1; return true; }()); // expected-error {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
  contract_assert([=] { t = 1; return true; }()); // expected-error {{cannot assign to variable 't' because it is considered 'const' inside of a contract}}
  contract_assert([=] { e = 1; return true; }()); // expected-error {{cannot assign to variable 'e' because it is considered 'const' inside of a contract}}
  contract_assert([=] { g = 1; return true; }()); // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}
  contract_assert([=]() mutable { s = 1; return true; }()); // expected-error {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
  contract_assert([&] { s = 1; return true; }()); // expected-error {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
  contract_assert([] { s = 1; return true; }()); // expected-error {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
  contract_assert((s = 1, true)); // expected-error {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
  contract_assert([=] { return __is_same(decltype((s)), const int &); }());
  contract_assert([=] { return __is_same(decltype((t)), const int &); }());
  contract_assert([=]() mutable { return __is_same(decltype((s)), const int &); }());

  // Entities with automatic storage duration are copied, and the copy is only
  // const because the lambda is not mutable.
  contract_assert([=] { a = 1; return true; }()); // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  contract_assert([=] { p = 1; return true; }()); // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  contract_assert([=]() mutable { a = 1; p = 2; return true; }());
  contract_assert([=] { return __is_same(decltype((a)), const int &); }());
  contract_assert([=]() mutable { return __is_same(decltype((a)), int &); }());
}

void pre_post(int p) pre([=] { s = 1; return true; }()) // expected-error {{use of undeclared identifier 's'}}
                     pre([=] { g = 1; return true; }()) // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}
                     pre([=] { p = 1; return true; }()) // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
                     pre([=]() mutable { g = 1; return true; }()); // expected-error {{cannot assign to variable 'g' because it is considered 'const' inside of a contract}}

template <class T> void tpl() {
  static T s = 0;
  contract_assert([=] { s = 1; return true; }()); // expected-error {{cannot assign to variable 's' because it is considered 'const' inside of a contract}} \
                                                  // expected-note {{while substituting into a lambda expression here}}
  contract_assert([=]() mutable { s = 1; return true; }()); // expected-error {{cannot assign to variable 's' because it is considered 'const' inside of a contract}} \
                                                            // expected-note {{while substituting into a lambda expression here}}
}
template void tpl<int>(); // expected-note 2 {{in instantiation of function template specialization 'tpl<int>' requested here}}
