// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -verify %s
// expected-no-diagnostics

// Contracts on nested class member functions

struct Outer {
  int limit = 100;

  struct Inner {
    int value;

    int get() const
      post (r: r >= 0)
    {
      return value;
    }

    void set(int v)
      pre (v >= 0)
    {
      value = v;
    }
  };

  Inner make(const int v) const
    pre (v <= limit)
    post (r: r.value == v)
  {
    Inner i;
    i.value = v;
    return i;
  }
};

// Nested class with access to enclosing private members via friendship
class Container {
  int capacity_ = 10;

  class Iterator {
    int pos_;
  public:
    explicit Iterator(int p) : pos_(p) {}
    int position() const
      post (r: r >= 0)
    {
      return pos_;
    }
  };

public:
  Iterator begin() const
    post (r: r.position() == 0)
  {
    return Iterator(0);
  }
};

// Deeply nested
struct A {
  struct B {
    struct C {
      int f(const int x)
        pre (x > 0)
        post (r: r == x * 2)
      {
        return x * 2;
      }
    };
  };
};

void test() {
  Outer o;
  Outer::Inner i = o.make(5);
  i.set(10);
  i.get();

  Container c;
  c.begin();

  A::B::C abc;
  abc.f(3);
}
