// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce -fsyntax-only -verify %s
// expected-no-diagnostics
//
// (Only the constant-evaluation half of the original test is kept here; the
// run-time half belongs with the Runnable tests.)

// Mirror of gnu_gcc's contract-constexpr-repeat-call-shapes.C: pre, post and
// contract_assert whose predicate re-calls a constexpr function already
// evaluated in the same constant evaluation.
//
// A REGRESSION PIN, not a fix.  Clang has never had this defect -- all of it
// compiles and runs clean here, and did before the mirror was written.  It is
// carried because GCC did have it, twice: once as PR125459 / GCC-3, and again
// through postconditions in the 2026-09-21 follow-up on that PR, which is
// what prompted this file.  The shapes are worth pinning on both evaluators.
//
// The GCC copy marks nine cases `[regresses]' -- the ones that fail on a
// stock GCC without the fix -- and explains why a by-value parameter reaches
// the defect in none of them while a reference bound to a prvalue does.  That
// analysis is GCC's constexpr evaluator and does not transfer; the shapes do.
// See the GCC file for it rather than duplicating it here.

struct Tag
{
  int v;
  constexpr bool ok () const { return v >= 0; }
};

/* Reference parameter: a prvalue argument materialises a new temporary for
   every call, so two calls never share a cache key.  */
constexpr bool by_ref (const Tag &t) { return t.v >= 0; }

/* By value, and so cacheable: every use below passes a different argument
   the second time.  */
constexpr bool by_val (Tag t) { return t.v >= 0; }

/* A non-trivial destructor makes the call uncacheable, so a repeat with
   identical arguments still re-evaluates the body.  */
struct Guard
{
  int v;
  constexpr Guard (int x) : v (x) {}
  constexpr ~Guard () {}
};
constexpr bool by_guard (const Guard &g) { return g.v >= 0; }

/* Returned by value, so the callee has a RESULT_DECL with storage.  Box is
   cacheable; BoxG is not.  */
struct Box
{
  int a, b, c;
  constexpr bool ok () const { return a >= 0; }
};
constexpr Box make_box (int a) { Box b { a, a + 1, a + 2 }; return b; }

struct BoxG
{
  int a, b, c;
  constexpr BoxG (int x) : a (x), b (x + 1), c (x + 2) {}
  constexpr ~BoxG () {}
  constexpr bool ok () const { return a >= 0; }
};
constexpr BoxG make_boxg (int a) { BoxG b (a); return b; }

constexpr bool box_ok_ref (const Box &b) { return b.a >= 0; }

/* ---- contract_assert: the earlier call sits above it in the body ------- */

constexpr int
ca_temp_inside (int s)
{
  bool first = by_ref (Tag { s });
  contract_assert (by_ref (Tag { s }));
  return first ? s : -1;
}

constexpr int
ca_temp_outside (int s)
{
  Tag t { s };			/* made out here, not in the predicate */
  bool first = by_val (t);
  contract_assert (by_val (Tag { s + 1 }));
  return first ? s : -1;
}

constexpr int
ca_guard (int s)
{
  bool first = by_guard (Guard (s));
  contract_assert (by_guard (Guard (s)));
  return first ? s : -1;
}

static_assert (ca_temp_inside (1) == 1);
static_assert (ca_temp_outside (1) == 1);
static_assert (ca_guard (1) == 1);

/* ---- pre: only the caller can have made the earlier call --------------- */

constexpr int pre_temp_inside (int s) pre (by_ref (Tag { s })) { return s; }
constexpr int pre_temp_outside (int s) pre (by_val (Tag { s + 1 }))
{ return s; }
constexpr int pre_guard (int s) pre (by_guard (Guard (s))) { return s; }

constexpr int
drive_pre (int s)
{
  bool a = by_ref (Tag { s });
  bool b = by_val (Tag { s + 1 });
  bool c = by_guard (Guard (s));
  return (a && b && c)
	 ? pre_temp_inside (s) + pre_temp_outside (s) + pre_guard (s)
	 : -1;
}

static_assert (drive_pre (1) == 3);

/* ---- post: the body always precedes the predicate ---------------------- */
/* A by-value parameter odr-used in a postcondition has to be const.  */

constexpr int
post_temp_inside (const int s) post (by_ref (Tag { s }))
{
  bool first = by_ref (Tag { s });
  return first ? s : -1;
}

constexpr int
post_temp_outside (const int s) post (by_val (Tag { s + 1 }))
{
  Tag t { s };
  bool first = by_val (t);
  return first ? s : -1;
}

constexpr int
post_guard (const int s) post (by_guard (Guard (s)))
{
  bool first = by_guard (Guard (s));
  return first ? s : -1;
}

static_assert (post_temp_inside (1) == 1);
static_assert (post_temp_outside (1) == 1);
static_assert (post_guard (1) == 1);

/* ---- the return-value slot --------------------------------------------- */

constexpr Box
box_assert (int s)
{
  Box first = make_box (s);
  contract_assert (make_box (s + 1).ok ());
  return first;
}

constexpr BoxG
boxg_assert (int s)
{
  BoxG first = make_boxg (s);
  contract_assert (make_boxg (s).ok ());
  return first;
}

/* A result name binds the return slot itself, and these predicates both read
   it and re-call the function that filled it.  */

constexpr Box
box_post_named (const int s) post (r : r.ok () && make_box (s + 1).ok ())
{
  return make_box (s);
}

constexpr BoxG
boxg_post_named (const int s) post (r : r.ok () && make_boxg (s).ok ())
{
  return make_boxg (s);
}

/* The result binding passed to a function taking a reference, alongside a
   fresh temporary through the same parameter.  */

constexpr Box
box_post_by_ref (const int s)
  post (r : box_ok_ref (r) && box_ok_ref (make_box (s + 1)))
{
  return make_box (s);
}

static_assert (box_assert (1).a == 1);
static_assert (boxg_assert (1).a == 1);
static_assert (box_post_named (1).a == 1);
static_assert (boxg_post_named (1).a == 1);
static_assert (box_post_by_ref (1).a == 1);

/* ---- a pre and a post on one function, both re-calling ----------------- */

constexpr Box
both_ends (const int s)
  pre (by_ref (Tag { s }))
  post (r : r.ok () && by_ref (Tag { s }))
{
  Box b = make_box (s);
  contract_assert (by_ref (Tag { s }));
  return b;
}

static_assert (both_ends (1).a == 1);
