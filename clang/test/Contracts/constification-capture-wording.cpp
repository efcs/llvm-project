// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// The diagnostic for an assignment to an entity which is const within the
// predicate of a contract assertion says why it is const: because of the
// contract assertion ([expr.prim.id.unqual]p7), or because the id-expression
// names the copy made by a non-mutable lambda-expression ([expr.prim.id.unqual]p4).

void f(int x, int &ref) {
  int y = x;
  // A lambda-expression within the predicate copies the entity: the copy is
  // const because the lambda is not mutable.
  contract_assert([=] { x = 1; return true; }()); // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  contract_assert([x] { x = 1; return true; }()); // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  contract_assert([=] { return [=] { y = 1; return true; }(); }()); // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  contract_assert([=]() mutable { x = 1; return true; }());
  // A reference capture refers to the entity, which is const because of the
  // contract assertion.
  contract_assert([&] { x = 1; return true; }()); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert([&x] { ++x; return true; }()); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert([&]() mutable { x = 1; return true; }()); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert((x = 1, true)); // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  contract_assert((ref = 1, true)); // expected-error {{cannot assign to variable 'ref' because it is considered 'const' inside of a contract}}

  // A lambda-expression enclosing the contract assertion: the copy made by a
  // non-mutable lambda is const because of the capture; in a mutable lambda
  // it is const only because of the contract assertion (see OPEN_QUESTIONS
  // 1.1).
  auto l1 = [x] { contract_assert((x = 1, true)); }; // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  auto l2 = [x]() mutable { contract_assert((x = 1, true)); }; // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  auto l3 = [&x] { contract_assert((x = 1, true)); }; // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  auto l4 = [x] { return [x] { contract_assert((x = 1, true)); }; }; // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  auto l5 = [x] { return [&x] { contract_assert((x = 1, true)); }; }; // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  auto l6 = [x]() mutable { return [&x] { contract_assert((x = 1, true)); }; }; // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  auto l7 = [x](int p) pre((x = 1, true)) {}; // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
  auto l8 = [x](int p) mutable pre((x = 1, true)) {}; // expected-error {{cannot assign to variable 'x' because it is considered 'const' inside of a contract}}
  (void)l1; (void)l2; (void)l3; (void)l4; (void)l5; (void)l6; (void)l7; (void)l8;
}

// A result binding is const within the predicate; a copy of it made by a
// non-mutable lambda is const because of the capture.
int g(const int v) post(r: [=] { r = 1; return true; }()) { return v; } // expected-error {{cannot assign to a variable captured by copy in a non-mutable lambda}}
int h(const int v) post(r: [&] { r = 1; return true; }()) { return v; } // expected-error {{cannot assign to result name 'r' because it is considered 'const' inside of a contract}}
int i(const int v) post(r: (r = 1, true)) { return v; } // expected-error {{cannot assign to result name 'r' because it is considered 'const' inside of a contract}}
int j(const int v) post(r: [=]() mutable { r = 1; return true; }()) { return v; }

// A lambda-expression without a capture-default cannot capture a result
// binding implicitly; the diagnostic names it as a result name.
int k(const int v) post(r: [] { return r > 0; }()) { return v; } // expected-error {{result name 'r' cannot be implicitly captured in a lambda with no capture-default specified}} \
                                                                 // expected-note {{'r' declared here}} expected-note {{lambda expression begins here}} \
                                                                 // expected-note 2 {{capture 'r' by}} expected-note 2 {{default capture by}}
void bind() {
  int arr[2] = {1, 2};
  auto [a, b] = arr; // expected-note {{'a' declared here}}
  contract_assert([] { return a > 0; }()); // expected-error {{variable 'a' cannot be implicitly captured in a lambda with no capture-default specified}} \
                                           // expected-note {{lambda expression begins here}} \
                                           // expected-note 2 {{capture 'a' by}} expected-note 2 {{default capture by}}
}
