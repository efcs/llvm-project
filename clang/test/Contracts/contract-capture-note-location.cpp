// Mirror of g++.dg/contracts/cpp26/contract-capture-note-location.C.
//
// A contract assertion may not cause a capture that exists only for it, and
// the diagnostic for that carries notes.  On the GCC side the note's location
// was read out of the capture's initializer, which stops being a DECL once
// the capture crosses two lambdas, giving a garbage line number (and a
// tree-check ICE in a checking build).
//
// Clang has never had that bug -- it does not build the note from the
// capture initializer at all.  This test exists as a regression pin: every
// diagnostic below is anchored to the line it belongs on, so a location that
// went wrong would stop matching.  Two and three levels of nesting are
// covered because a one-level-only fix would still be wrong at three, and
// generic lambdas are covered because the original upstream report blamed
// them and they are in fact irrelevant to the GCC bug.  In the generic case
// the outer lambda is diagnosed at definition time and the inner one when it
// is instantiated, with the same notes.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

void one_level() {
  int x = 1;
  auto l = [&]() { contract_assert(x > 0); };
  // expected-error@-1 {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
  // expected-note@-2 {{capture of local entity 'x' is required here}}
  // expected-note@-3 {{within contract context introduced here}}
 
  l();
}

void two_levels() {
  int x = 1;
  auto outer = [&]() {
    auto inner = [&]() { contract_assert(x > 0); };
    // expected-error@-1 {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
    // expected-note@-2 {{capture of local entity 'x' is required here}}
    // expected-note@-3 {{within contract context introduced here}}
   
    inner();
  };
  outer();
}

void three_levels() {
  int x = 1;
  auto a = [&]() {
    auto b = [&]() {
      auto c = [&]() { contract_assert(x > 0); };
      // expected-error@-1 {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
      // expected-note@-2 {{capture of local entity 'x' is required here}}
      // expected-note@-3 {{within contract context introduced here}}
     
      c();
    };
    b();
  };
  a();
}

void two_levels_generic() {
  int x = 1;
  auto outer = [&](auto f) {
    // expected-error@-1 {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
    auto inner = [&](auto g) { contract_assert(x + f + g > 0); };
    // expected-error@-1 {{implicit capture of local entity 'x' is not allowed when used exclusively in contract assertions}}
    // expected-error@-2 {{implicit capture of local entity 'f' is not allowed when used exclusively in contract assertions}}
    // expected-note@-3 2 {{capture of local entity 'x' is required here}}
    // expected-note@-4 {{capture of local entity 'f' is required here}}
    // expected-note@-5 3 {{within contract context introduced here}}
    inner(1);
  };
  outer(1);
  // expected-note@-1 {{in instantiation of function template specialization}}
}
