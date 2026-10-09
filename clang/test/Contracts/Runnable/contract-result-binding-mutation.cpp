// A mutation performed through a postcondition's result binding is observable
// by the caller.
//
// The position taken here: a predicate that is evaluated is evaluated
// faithfully, so its effects stand.  [basic.contract.eval] permits *not
// evaluating* a predicate -- "an alternative evaluation that produces the same
// value ... but has no side effects" may be substituted -- but that is
// all-or-nothing.  It does not permit evaluating a predicate and then
// discarding what it did.
//
// The write is easy to lose.  EmitPostContracts stores the return value into
// the return slot and binds the result name there, so the predicate's write
// lands in the slot correctly -- but the epilogue has already materialised
// the value it is about to return, and returning that copy discards the
// write.  The slot must be re-read after the postconditions have run.  GCC
// makes the same mutations observable.
//
// This file covers results returned directly.  A CLASS-typed result is still
// bound to a copy and its mutations are still lost; that gap is pinned
// separately in contract-result-binding-mutation-class.cpp.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts %libcxx_flags -o %t && %t

static int failures = 0;

static void check(const char *what, int got, int want) {
  if (got != want) {
    __builtin_printf("FAIL: %s: got %d, expected %d\n", what, got, want);
    ++failures;
  }
}

// The original reproducer: increment through a const_cast.
int direct_increment() post(r : (const_cast<int &>(r)++, true)) { return 1; }

// Assignment rather than increment.
int assignment() post(r : (const_cast<int &>(r) = 42, true)) { return 0; }

// The mutation performed inside a called function taking `const &'.  This is
// the spelling that reaches the binding through a reference parameter, which
// on the GCC side was a distinct bug (the binding denoted a second object).
static bool bump(const int &r) {
  const_cast<int &>(r) += 10;
  return true;
}
int via_reference_parameter() post(r : bump(r)) { return 5; }

// Two postconditions, each mutating: both effects must stand, in order.
int two_postconditions() post(r : (const_cast<int &>(r)++, true))
    post(r : (const_cast<int &>(r) *= 3, true)) {
  return 2;
}

// A postcondition with no result name must be unaffected.
int no_result_name() post(true) { return 77; }

// A class-typed result with a NON-TRIVIAL copy constructor and destructor
// must be bound WITHOUT introducing a temporary -- [dcl.contract.res]
// Example 2's `B' row, "the postcondition check succeeds, no temporary is
// introduced".  An extra copy here would be observable as extra constructor
// and destructor calls, so the counts are pinned, not just the address.
//
// (Example 2's `A' row permits a temporary for a result the implementation
// returns by other means, and both compilers do introduce one for a
// TRIVIALLY-copyable class -- where it costs no constructor call and is
// unobservable except that a mutation through it is lost.  That is what
// contract-result-binding-mutation-class.cpp pins.)
struct Counted {
  int a;
  static int ctor, copy, move, dtor;
  Counted(int v) : a(v) { ++ctor; }
  Counted(const Counted &o) : a(o.a) { ++copy; }
  Counted(Counted &&o) : a(o.a) { ++move; }
  ~Counted() { ++dtor; }
};
int Counted::ctor = 0, Counted::copy = 0, Counted::move = 0, Counted::dtor = 0;

static const Counted *seen_addr = nullptr;
static bool note(const Counted &r) {
  seen_addr = &r;
  return true;
}
Counted counted() post(r : note(r)) { return Counted(1); }

int main() {
  check("increment through const_cast", direct_increment(), 2);
  check("assignment through const_cast", assignment(), 42);
  check("mutation via a const& parameter", via_reference_parameter(), 15);
  check("two mutating postconditions", two_postconditions(), 9);
  check("no result name", no_result_name(), 77);

  {
    Counted::ctor = Counted::copy = Counted::move = Counted::dtor = 0;
    Counted c = counted();
    check("non-trivial result: constructions", Counted::ctor, 1);
    check("non-trivial result: copies", Counted::copy, 0);
    check("non-trivial result: moves", Counted::move, 0);
    if (seen_addr != &c) {
      __builtin_printf("FAIL: non-trivial result: the predicate saw a "
                       "temporary, not the returned object\n");
      ++failures;
    }
  }

  if (failures)
    __builtin_abort();
  return 0;
}
