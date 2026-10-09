// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [dcl.contract.func]: a non-reference value parameter odr-used in a
// postcondition must be declared const.  Mirror of GCC's
// g++.dg/contracts/cpp26/dcl.contract.res.p1-pack-empty.C, which is a guard
// here rather than a fix: GCC carried this property from a template's
// parameters to an instantiation's by walking the two parameter lists in
// lockstep, which does not survive a function parameter pack -- a pack fills
// one slot in the pattern but expands to N in the instantiation.  A pack that
// expanded to NOTHING ran the walk off the end of the shorter list and
// crashed the compiler (with or without a postcondition -- any contract
// specifier started the walk), and a parameter written after a pack was
// paired with a pack element instead of itself, so a well-formed program was
// rejected with the wrong parameter named.
//
// Clang reproduces none of that: it checks each parameter where the
// postcondition references it, so nothing depends on the two lists lining up.
// These cases are pinned so both compilers keep agreeing on them.
//
// A parameter whose type is written non-dependently is diagnosed at parse
// time, so every case that has to reach the instantiation gives its parameter
// a dependent type.

// --- A contract specifier and a pack that expands to nothing.  No
//     postcondition is involved: on GCC any contract specifier was enough. ---
template <class... A>
void pre_empty(int x, A &&...a) pre(x > 0) {}

// --- The same with a postcondition, and with the pack itself used in one. ---
template <class... A>
int post_empty(const int x, A... a) post(r : r > x) { return x + 1; }

template <class... A>
int post_empty_uses_pack(const A... a) post(r : (int(a) + ... + 0) >= r) {
  return 0;
}

// --- A parameter written after the pack, const, so no instantiation of it is
//     in the wrong: the pack element opposite it must not be blamed. ---
template <class... A, class T>
int tail_const(A... a, const T y) post(r : r > y) { return y + 1; }

// --- A parameter after the pack that really is non-const: still diagnosed,
//     and naming Y rather than whichever pack element shares its position. ---
template <class... A, class T>
int tail_nonconst(A... a, T y) // expected-note {{parameter of type 'int' is declared here}}
    post(r : r > y) {          // expected-error {{parameter 'y' referenced in contract postcondition must be declared const}}
  return y + 1;
}

// --- The same, reached with an empty pack. ---
template <class... A, class T>
int tail_nonconst_empty(A... a, T y) // expected-note {{parameter of type 'int' is declared here}}
    post(r : r > y) {                // expected-error {{parameter 'y' referenced in contract postcondition must be declared const}}
  return y + 1;
}

// --- A parameter before the pack. ---
template <class T, class... A>
int front_const(const T x, A... a) post(r : r > x) { return x + 1; }

template <class T, class... A>
int front_nonconst(T x, A... a) // expected-note {{parameter of type 'int' is declared here}}
    post(r : r > x) {           // expected-error {{parameter 'x' referenced in contract postcondition must be declared const}}
  return x + 1;
}

// --- Parameters on both sides of the pack at once. ---
template <class T, class... A, class U>
int both_sides(const T x, A... a, const U y) post(r : r > x + y) {
  return x + y + 1;
}

// --- The same shapes written as an abbreviated function template, where the
//     pack is invented from `auto...' rather than declared.  This is the
//     spelling GCC's ICE keeps being reported in (most recently PR127343);
//     nothing here distinguishes an invented template parameter pack from a
//     written one, but both compilers are pinned to it. ---
void abbrev_pre(auto... a) pre(true) {}

int abbrev_front_const(const auto x, auto... a) post(r : r > x) {
  return x + 1;
}

int abbrev_front_nonconst(auto x, auto... a) // expected-note {{parameter of type 'int' is declared here}}
    post(r : r > x) {                        // expected-error {{parameter 'x' referenced in contract postcondition must be declared const}}
  return x + 1;
}

// --- A member of a class template, whose contracts are parsed later. ---
template <class T> struct holder {
  template <class... A>
  int f(const T x, A... a) post(r : r > x) { return x + 1; }
};

// --- A separate declaration and definition. ---
template <class... A, class T>
int declared_first(A... a, const T y) post(r : r > y);

template <class... A, class T> int declared_first(A... a, const T y) {
  return y + 1;
}

void g() {
  // Empty packs.
  pre_empty(1);
  post_empty(1);
  post_empty_uses_pack();
  tail_const<>(1);
  front_const(1);
  both_sides<int>(1, 2);
  declared_first<>(1);
  abbrev_pre();
  abbrev_front_const(1);

  // Packs expanding to one element: the lists are the same length, but a tail
  // parameter is still displaced by the pack.
  pre_empty(1, 2);
  post_empty(1, 2);
  post_empty_uses_pack(1);
  tail_const<int>(1, 2);
  front_const(1, 2);
  both_sides<int, int>(1, 2, 3);
  declared_first<int>(1, 2);
  abbrev_pre(1);
  abbrev_front_const(1, 2);

  // Packs expanding to more than one element.
  pre_empty(1, 2, 3);
  post_empty(1, 2, 3);
  post_empty_uses_pack(1, 2);
  tail_const<int, int>(1, 2, 3);
  front_const(1, 2, 3);
  both_sides<int, int, int>(1, 2, 3, 4);
  declared_first<int, int>(1, 2, 3);
  abbrev_pre(1, 2);
  abbrev_front_const(1, 2, 3);

  holder<int>().f(1);
  holder<int>().f(1, 2);

  // The genuinely non-const cases, each instantiated exactly once.
  tail_nonconst<int, int>(1, 2, 3);
  // expected-note@-1 {{in instantiation of function template specialization 'tail_nonconst<int, int, int>' requested here}}
  tail_nonconst_empty<>(1);
  // expected-note@-1 {{in instantiation of function template specialization 'tail_nonconst_empty<int>' requested here}}
  front_nonconst(1, 2);
  // expected-note@-1 {{in instantiation of function template specialization 'front_nonconst<int, int>' requested here}}
  abbrev_front_nonconst(1, 2);
  // expected-note@-1 {{in instantiation of function template specialization 'abbrev_front_nonconst<int, int>' requested here}}
}
