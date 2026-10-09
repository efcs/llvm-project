// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -verify %s
// expected-no-diagnostics

// Multiple contract specifiers in sequence

// Multiple preconditions
int clamp(const int x, const int lo, const int hi)
  pre (lo <= hi)
  pre (x >= lo)
  pre (x <= hi)
  post (r: r >= lo)
  post (r: r <= hi)
{
  return x;
}

// Alternating pre/post (order matters but both kinds can appear)
int validated(const int x)
  pre (x > 0)
  post (r: r > 0)
  pre (x < 1000)
  post (r: r < 1000)
{
  return x;
}

// Contracts on member functions with multiple specifiers
struct Buffer {
  int size() const
    post (r: r >= 0);

  void resize(const int n)
    pre (n >= 0)
    pre (n <= 1024)
    post (size() == n);

  int& at(int i)
    pre (i >= 0)
    pre (i < size());
};

// Template with multiple contracts
template<typename T>
T abs_val(const T x)
  pre (x == x)
  post (r: r >= T{0})
{
  return x >= T{0} ? x : -x;
}

void test() {
  clamp(5, 0, 10);
  validated(42);
  abs_val(-3);
  abs_val(-2.5);
}
