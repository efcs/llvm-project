// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts

// [expr.prim.id.unqual]/3+d reaches inside a lambda that appears in the
// predicate: the paragraph's own example marks `++n`, on a namespace-scope
// `int n`, an error inside `pre([=,&i,*this] mutable {...})`.
//
// The variable cases follow from getContractConstification carrying no
// automatic-storage restriction.  GCC needs a dedicated fix for the lambda
// case, because its gate asks whether the INNERMOST binding level is the
// contract scope, which stops being true the moment a lambda pushes its own;
// Clang's does not ask that, so most of this file is a regression pin here
// for a bug that is real on the other compiler.
//
// Two lambda-specific shapes can go wrong in opposite directions:
//
//   * a member reached through a CAPTURED `*this` must not be constified,
//     since /3+e marks `++this->z` and `++z` "OK, captured *this" -- the rule
//     constifies a VARIABLE declared outside the assertion, and a non-static
//     data member of the captured object is not one (Clang gets this wrong;
//     see OpenBugs/constify-captured-this-member.cpp); and
//   * a FUNCTION-LOCAL STATIC must be constified, being "a variable declared
//     outside of C" like any other.  Naming it DIRECTLY in a predicate is the
//     control kept below, which is what separates a lambda gap from a
//     storage-duration one.
//
// GCC mirror: g++.dg/contracts/cpp26/contract-predicate-constify-lambda.C

int n = 0;
thread_local int t_n = 0;

struct HasStatic {
  static int s;
};
int HasStatic::s = 0;

struct X {
  bool m();
};

struct Y {
  int z = 0;

  void f(int i, int *p, int &r, X x, X *px)
      pre([=, &i, *this]() mutable {
        // expected-error@+1 {{cannot assign to variable 'n' because it is considered 'const' inside of a contract}}
        ++n;
        // expected-error@+1 {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
        ++i;
        ++p;       // OK: a member of the closure type
        ++r;       // OK: a non-reference member of the closure type
        // `++this->z` / `++z` through the captured *this are OK per the
        // standard's example; see OpenBugs/constify-captured-this-member.cpp.
        (void)x;
        (void)px;

        int j = 17; // declared INSIDE the predicate, so not constified
        ++j;        // OK

        [&]() {
          int k = 34;
          // expected-error@+1 {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
          ++i;
          ++j; // OK
          ++k; // OK
        }();
        return true;
      }()) {}
};

// The storage durations Clang does constify, named from inside a lambda in the
// predicate rather than directly.  Each is "a variable declared outside of C".
void from_lambda() {
  // expected-error@+1 {{cannot assign to variable 'n' because it is considered 'const' inside of a contract}}
  contract_assert([]() { ++n; return true; }());
  // expected-error@+1 {{cannot assign to variable 't_n' because it is considered 'const' inside of a contract}}
  contract_assert([]() { ++t_n; return true; }());
  // expected-error@+1 {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
  contract_assert([]() { ++HasStatic::s; return true; }());

  static int local_static = 0;
  // expected-error@+1 {{cannot assign to variable 'local_static' because it is considered 'const' inside of a contract}}
  contract_assert([&]() { ++local_static; return true; }());
}

// CONTROL: named directly in the predicate, including a function-local static.
// This case always worked, which is what identified the failure above as a
// LAMBDA gap rather than a storage-duration one.
void named_directly() {
  static int local_static = 0;
  // expected-error@+1 {{cannot assign to variable 'n' because it is considered 'const' inside of a contract}}
  contract_assert(++n);
  // expected-error@+1 {{cannot assign to variable 'local_static' because it is considered 'const' inside of a contract}}
  contract_assert(++local_static);
}

// CONTROL: outside a contract entirely, a lambda constifies nothing.
void not_in_a_contract() {
  []() {
    ++n;
    ++t_n;
    ++HasStatic::s;
  }();
}
