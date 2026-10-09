// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts

// [expr.prim.id.unqual]/3+d: inside the predicate of a contract assertion C,
// an id-expression naming "a variable declared outside of C of object type T"
// has type const T.  There is NO storage-duration restriction -- P2900R9
// removed one ("Made implicit const in contract predicates apply to all
// variables, rather than just those with automatic storage duration"), and
// the paragraph's own example opens with a namespace-scope `int n` and
// `pre(++n) // error: attempting to modify const lvalue`.
//
// Constifying only variables of AUTOMATIC storage duration leaves a
// namespace-scope variable, a function-local static, a thread_local and a
// static data member writable inside a predicate.  There are two places to
// encode that restriction by accident -- getContractConstification's
// storage-duration test, and isUsageAcrossContract returning false outright
// for any non-local variable -- and neither may.
//
// GCC mirror: g++.dg/contracts/cpp26/contract-predicate-constify-storage.C

// A namespace-scope variable.
int g_n = 0;
// expected-error@+1 {{cannot assign to variable 'g_n' because it is considered 'const' inside of a contract}}
void f_namespace() pre(++g_n);

// A namespace-scope POINTER: the pointer is const, the pointee is not.
int g_i = 0;
int *g_p = &g_i;
// expected-error@+1 {{cannot assign to variable 'g_p' because it is considered 'const' inside of a contract}}
void f_pointer_itself() pre(++g_p);
void f_pointee() pre(++*g_p); // OK

// A function-local static.
void f_local_static() {
  static int s = 0;
  // expected-error@+1 {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
  contract_assert(++s);
}

// A thread_local.
thread_local int t_n = 0;
// expected-error@+1 {{cannot assign to variable 't_n' because it is considered 'const' inside of a contract}}
void f_thread_local() pre(++t_n);

// A static data member -- a variable, so the rule reaches it.
struct HasStatic {
  static int s;
};
int HasStatic::s = 0;
// expected-error@+1 {{cannot assign to variable 's' because it is considered 'const' inside of a contract}}
void f_static_member() pre(++HasStatic::s);

// CONTROL: a local of automatic storage duration, which Clang already
// constifies.  This one passes today.
void f_automatic() {
  int a = 0;
  // expected-error@+1 {{cannot assign to variable 'a' because it is considered 'const' inside of a contract}}
  contract_assert(++a);
}
