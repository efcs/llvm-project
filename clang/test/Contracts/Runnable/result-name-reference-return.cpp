// A postcondition with a result-name-introducer on a function returning a
// REFERENCE compiles, and the binding names the referred-to object.
//
// Two distinct things have to be right, and only the first announces itself:
//
//   Expr.h: Assertion `(t.isNull() || !t->isReferenceType()) &&
//                      "Expressions can't have reference type"' failed.
//
// EmitPostContracts must not build an OpaqueValueExpr of the result name's
// type, which for a reference-returning function is a reference -- and an
// Expr may not have one.  Behind that, EmitDeclRefLValue must not map the
// result name straight onto the return slot: for a reference return that slot
// holds the reference itself, a pointer, and reinterpreting it as the
// referred-to object gives mismatched IR types.  The slot is loaded instead.
//
// Both are CODEGEN properties -- -fsyntax-only is clean either way, so this
// must be a runtime test.  GCC compiles and runs the same program correctly.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts %libcxx_flags -o %t && %t

static int g = 100;

// Merely naming the result was enough to crash; the mutation was not needed.
int &plain() post(r : r > 0) { return g; }

// A write through the binding reaches the referred-to object directly -- no
// copy back is involved, unlike a by-value result.
int &mutating() post(r : (const_cast<int &>(r) += 5, true)) { return g; }

// The binding must name the object the function returns, not a copy of it.
static const int *seen = nullptr;
static bool note(const int &r) {
  seen = &r;
  return true;
}
int &identity() post(r : note(r)) { return g; }

int main() {
  g = 100;
  if (plain() != 100)
    return 1;

  g = 100;
  if (mutating() != 105)
    return 1;

  g = 100;
  if (&identity() != &g || seen != &g)
    return 1;

  return 0;
}
