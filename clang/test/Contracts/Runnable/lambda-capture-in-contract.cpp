// [expr.prim.lambda.capture]/3.3 allows a capture-default or simple-capture in
// a lambda-introducer when the lambda appears within a contract assertion and
// its innermost enclosing scope is the corresponding contract-assertion scope.
// So a lambda written in a contract predicate may capture the enclosing
// function's parameters.
//
// Every case is checked BY THE VALUE THE PREDICATE SAW, not by whether it
// compiles: several of these forms can be accepted with no capture recorded
// at all and then crash in CodeGen ("DeclRefExpr for Decl not entered in
// LocalDeclMap?"), so "it compiles" is never evidence of anything.  A
// contract predicate cannot assign to a variable it names, so the value is
// recorded through a called function.
//
// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts %libcxx_flags -o %t && %t

int g_seen = -1;

bool record(int v) {
  g_seen = v;
  return true;
}

// Free functions: the four legal capture forms. These are the ones that were
// broken -- the predicate is parsed off the declarator, before the parameters
// are reparented onto the function.
void copy_cap(int x) pre([x] { return record(x); }()) {}
void ref_cap(int x) pre([&x] { return record(x); }()) {}
void default_ref(int x) pre([&] { return record(x); }()) {}
void default_copy(int x) pre([=] { return record(x); }()) {}

// A postcondition. The parameter must be const: capturing it odr-uses it, and
// [dcl.contract.func] requires a non-reference parameter odr-used by a
// postcondition predicate to have const type.
int post_cap(const int x) post(r : [x] { return record(x); }()) { return x + 1; }

// Member functions, which already worked -- kept as the reference behaviour
// the free-function cases above must now match.
struct S {
  int m = 7;

  void mem(int x) pre([x] { return record(x); }()) {}
  void mem_default(int x) pre([&] { return record(x); }()) {}

  // A parameter captured alongside 'this', which reaches the member through
  // the remapped dummy rather than through a capture.
  void mem_this(int x) pre([this, x] { return record(x + m); }()) {}
};

// An assertion-statement in the body, which reaches the capture machinery by
// a third path -- neither a function-contract-specifier on a free function nor
// on a member -- and had no coverage until the audit of 2026-09-05.
void in_assert(int x) {
  int local = x + 1;
  contract_assert([x] { return record(x); }());
  contract_assert([&local] { return record(local); }());
  contract_assert([=] { return record(x + local); }());
}

// A nested contract_assert inside a predicate lambda, NAMING THE CAPTURE.
// GCC ICEd on exactly this shape (expand_expr_real_1: "Variables inherited
// from containing functions should have been lowered by this point"): a
// capture named from a contract_assert nested inside a predicate lambda is
// one lowering step further in than a capture named by the predicate itself.
// Clang gets it right, and the value check is what proves the capture -- not
// merely that it compiles.
void nested_assert_on_capture(int x)
    pre([x] { contract_assert(x >= 0); return record(x); }()) {}

// A lambda's own precondition capturing that lambda's parameter.
void lambda_own_pre() {
  auto l = [](int x) pre([x] { return record(x); }()) {};
  l(66);
}

int main() {
  copy_cap(11);
  if (g_seen != 11)
    __builtin_abort();

  ref_cap(22);
  if (g_seen != 22)
    __builtin_abort();

  default_ref(33);
  if (g_seen != 33)
    __builtin_abort();

  default_copy(44);
  if (g_seen != 44)
    __builtin_abort();

  post_cap(55);
  if (g_seen != 55)
    __builtin_abort();

  S s;

  s.mem(77);
  if (g_seen != 77)
    __builtin_abort();

  s.mem_default(88);
  if (g_seen != 88)
    __builtin_abort();

  s.mem_this(99);
  if (g_seen != 99 + 7)
    __builtin_abort();

  lambda_own_pre();
  if (g_seen != 66)
    __builtin_abort();

  in_assert(120);
  if (g_seen != 120 + 121)
    __builtin_abort();

  nested_assert_on_capture(131);
  if (g_seen != 131)
    __builtin_abort();

  return 0;
}
