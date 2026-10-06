// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// Within the predicate of a contract assertion, an id-expression naming a
// variable declared outside the predicate is const ([basic.contract.general]).
// A lambda that implicitly or explicitly captures such a variable by reference
// therefore captures it as const, and mutating it through the capture is
// ill-formed.  This must hold both for ordinary functions and for function
// templates, where the check happens at instantiation.  Capturing by copy cuts
// the chain (the copy is a fresh, mutable object), and a variable declared
// *inside* the predicate is not constified.

// --- Non-template cases (const enforced at parse time) ---

namespace non_template {

// A: implicit '[&]' capture -- mutation is an error.
void a(int x) {
  contract_assert([&] { return ++x > 0; }()); // expected-error {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
}

// E: explicit '[&x]' capture -- mutation is an error.
void e(int x) {
  contract_assert([&x] { return ++x > 0; }()); // expected-error {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
}

// B: '[=] mutable' captures by copy -- the copy is mutable, so this is OK.
void b(int x) {
  contract_assert([=]() mutable { return ++x > 0; }());
}

// C: by-copy capture then inner '[&]' captures the (const) copy field of the
// outer closure; but the outer copy cut the contract chain, so OK.
void c(int x) {
  contract_assert([=]() mutable { return [&] { return ++x > 0; }(); }());
}

// D: 'x' is a local declared *inside* the predicate; it is not constified.
void d(int) {
  contract_assert([] { int x = 0; return [&] { return ++x > 0; }(); }());
}

// G: nested explicit by-reference captures -- error at the innermost mutation.
void g(int x) {
  contract_assert([&x] { return [&x] { return ++x > 0; }(); }()); // expected-error {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
}

// H: nested implicit by-reference captures -- error at the innermost mutation.
void h(int x) {
  contract_assert([&] { return [&] { return ++x > 0; }(); }()); // expected-error {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
}

} // namespace non_template

// --- Template cases (const enforced at instantiation) ---

namespace templated {

// FA: implicit '[&]' capture in a template -- must be an error at instantiation.
template <class T> void fa(T x) {
  contract_assert([&] { return ++x > 0; }());
  // expected-error@-1 {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
  // expected-note@-2 {{while substituting into a lambda expression here}}
}
// expected-note@+1 {{in instantiation of function template specialization 'templated::fa<int>' requested here}}
template void fa<int>(int);

// FB: '[=] mutable' captures by copy in a template -- OK.
template <class T> void fb(T x) {
  contract_assert([=]() mutable { return ++x > 0; }());
}
template void fb<int>(int);

// FC: by-copy then inner '[&]' in a template -- OK.
template <class T> void fc(T x) {
  contract_assert([=]() mutable { return [&] { return ++x > 0; }(); }());
}
template void fc<int>(int);

// FD: predicate-local 'x' in a template -- not constified, OK.
template <class T> void fd(T) {
  contract_assert([] { int x = 0; return [&] { return ++x > 0; }(); }());
}
template void fd<int>(int);

} // namespace templated
