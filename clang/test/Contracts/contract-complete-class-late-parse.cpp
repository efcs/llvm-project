// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// expected-no-diagnostics

// [class.mem.general]p7: a contract assertion on a member function is a
// complete-class context, so its predicate may name members declared later in
// the class -- exactly as a member function body may.  That works by caching
// the contract's tokens and replaying them at the end of the class.
//
// The caching decision is made in ParseFunctionDeclarator, which looks at the
// token sitting directly after the declarator.  A trailing requires-clause is
// parsed *after* that point, so a contract written behind it was reached by a
// different path that parsed it eagerly -- and every member declared after it
// became "use of undeclared identifier".
//
// Each case names a member declared after the contract, since that is the
// whole point; a case that named an earlier member would pass whether the
// contract was deferred or not.
//
// (P2900R14 does not allow contracts on virtual functions, so the
// virt-specifier shapes are not covered here.)

// ---------------------------------------------------------------------------
// Nothing between the declarator and the contract.  These always worked and
// are here as controls: a fix that deferred too eagerly, or not at all, breaks
// them rather than the cases below.
// ---------------------------------------------------------------------------

struct Plain {
  int f() const pre(g() >= 0);
  int g() const;
};

struct PlainDtor {
  ~PlainDtor() pre(g() >= 0);
  int g() const;
};

struct PlainPost {
  int f() const post(r : r >= h());
  int h() const;
};

struct PlainBoth {
  int f() const pre(g() >= 0) post(r : r <= h());
  int g() const;
  int h() const;
};

struct PlainNoexcept {
  int f() const noexcept pre(g() >= 0);
  int g() const;
};

struct PlainTrailingReturn {
  auto f() const -> int pre(g() >= 0);
  int g() const;
};

struct PlainDefinition {
  int f() const pre(g() >= 0) { return g(); }
  int g() const;
};

// The deferral is per-declarator, so a member-declarator-list must not lose it
// for the second and later declarators, whose Declarator is recycled.
struct AfterOtherMembers {
  int a, b;
  int f() const pre(g() >= 0);
  int g() const;
};

template <class X>
struct InClassTemplate {
  int f() const pre(g() >= 0);
  int g() const;
};

template struct InClassTemplate<int>;

// Two contracted members in one class, so the second is parsed with the first
// already cached.
struct Two {
  int f() const pre(g() >= 0);
  int w() const post(r : r <= h());
  int g() const;
  int h() const;
};

// ---------------------------------------------------------------------------
// A trailing requires-clause between the declarator and the contract.  Not a
// complete-class context itself, but it sits in the other arm of the same
// if/else and so was broken the same way.
// ---------------------------------------------------------------------------

struct Requires {
  template <class T>
  int f() const requires (sizeof(T) > 0) pre(g() >= 0);
  int g() const;
};

struct RequiresPost {
  template <class T>
  int f() const requires (sizeof(T) > 0) post(r : r <= h());
  int h() const;
};

// ---------------------------------------------------------------------------
// POSITIVE CONTROL: the named member comes first, so no deferral is needed to
// find it.  This isolates the defect to WHEN the contract is parsed rather
// than to what is visible from it.
// ---------------------------------------------------------------------------

struct MemberDeclaredFirst {
  int k() const;
  template <class T>
  int f() const requires (sizeof(T) > 0) pre(k() >= 0);
};
