// RUN: %clangxx -std=c++17 %s -o %t && %t
// XFAIL: *
//
// OPEN BUG area: codegen (core language, no contracts).

// OPEN BUG: when a local's destructor
// throws AFTER the by-value return object has been constructed, Clang never
// destroys the return object.
//
// [except.ctor]/2: "If an exception is thrown during the destruction of
// temporaries or local variables for a return statement, the destructor for
// the returned object (if any) is also invoked." So `live` must be back to 0.
// Clang leaves it at 1.
//
// The MIRROR is
// gcc/testsuite/g++.dg/contracts/cpp26/open-bug-retval-throwing-cleanup.C,
// which PASSES: GCC implements this deliberately, with a
// current_retval_sentinel mechanism in cp/except.cc whose stated purpose is
// to "clean up the return value if a local destructor throws". Same test
// content, opposite expectation.
//
// No contracts involved; plain C++17.  It is the no-contract control for the
// contracts-side version of the same bug: if the plain-C++ case is also wrong,
// the defect is not in the contract machinery.

int live = 0;

struct Counted {
  int v;
  Counted(int x) : v(x) { ++live; }
  Counted(const Counted &o) : v(o.v) { ++live; }
  ~Counted() { --live; }
};

struct ThrowOnDestroy {
  bool armed;
  ~ThrowOnDestroy() noexcept(false) {
    if (armed)
      throw 42;
  }
};

Counted f(bool arm) {
  ThrowOnDestroy guard{arm};
  Counted result(7);
  return result; // built, then `guard` throws on the way out
}

int main() {
  try {
    f(true);
  } catch (int) {
  }
  if (live != 0) // GCC: 0. Clang: 1.
    __builtin_abort();
  return 0;
}
