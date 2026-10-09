// A mutation through a postcondition's result binding is observable for a
// CLASS-typed result, however the ABI returns it.
//
// Be precise about what is and is not going on, because "the binding names a
// copy" sounds much worse than it is:
//
//   * For a class with a NON-TRIVIAL copy constructor or destructor, no
//     temporary is introduced at all -- the binding names the returned object
//     itself and no extra construction or destruction is observable.
//     [dcl.contract.res] Example 2's `B' row requires that, and
//     contract-result-binding-mutation.cpp pins the object counts.
//   * For a TRIVIALLY-copyable class, both compilers introduce a temporary,
//     which Example 2's `A' row expressly permits and which costs no
//     constructor call.
//
// The bug was what happened after that temporary: Clang returned a value it
// had materialised BEFORE running the postconditions, so a write the
// predicate made into the return slot was dropped.  For a class the value is
// usually COERCED out of the slot -- { i64, i64 } for a four-int struct, i32
// for a one-int struct -- so re-reading the slot is not a plain load, which is
// why the first fix covered only scalars.  Redo the coercion instead.
//
// A genuinely sret-returned class needed nothing: the callee writes the
// caller's object directly.  It is covered anyway, since which of the three
// paths a given class takes is an ABI detail nobody should have to re-derive.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts %libcxx_flags -o %t && %t

struct Big { // coerced to { i64, i64 }
  int a, b, c, d;
};
struct Small { // coerced to i32
  int a;
};
struct Huge { // returned indirectly, via sret
  int a;
  char pad[64];
};

static int ran = 0;

static bool bumpBig(const Big &r) {
  ++ran;
  const_cast<Big &>(r).a += 7;
  return true;
}
static bool bumpSmall(const Small &r) {
  ++ran;
  const_cast<Small &>(r).a += 9;
  return true;
}
static bool bumpHuge(const Huge &r) {
  ++ran;
  const_cast<Huge &>(r).a += 11;
  return true;
}

Big big() post(r : bumpBig(r)) { return Big{1, 2, 3, 4}; }

Small small_() post(r : bumpSmall(r)) { return Small{1}; }

Huge huge() post(r : bumpHuge(r)) {
  Huge h{};
  h.a = 1;
  return h;
}

int main() {
  ran = 0;
  int ba = big().a;
  if (ran != 1)
    __builtin_abort(); // the predicate must run at all
  if (ba != 8)
    return 1;

  ran = 0;
  int sa = small_().a;
  if (ran != 1)
    __builtin_abort();
  if (sa != 10)
    return 1;

  ran = 0;
  int ha = huge().a;
  if (ran != 1)
    __builtin_abort();
  if (ha != 12)
    return 1;

  return 0;
}
