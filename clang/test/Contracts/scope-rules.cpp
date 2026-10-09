// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -verify %s
// expected-no-diagnostics

// Contract scope rules: result-name-introducer and assertion scope

// Result name is visible only in its postcondition predicate
int f(const int x)
  pre (x > 0)
  post (r: r == x * 2)
{
  return x * 2;
}

// Multiple postconditions: each 'r' is independently scoped
int g(const int x)
  post (r: r > 0)
  post (r: r < 100)
{
  return x;
}

// Result name does not shadow parameters (different scope)
int h(const int r)
  pre (r > 0)
  post (result: result == r)
{
  return r;
}

// contract_assert introduces its own scope
void scope_test() {
  int x = 5;
  contract_assert(x > 0);

  {
    int y = 10;
    contract_assert(y > x);
  }
}

// Variables declared before the contract are visible in predicates
struct S {
  int member;

  int get() const
    post (r: r == member)
  {
    return member;
  }

  void set(const int v)
    pre (v >= 0)
  {
    member = v;
  }
};
