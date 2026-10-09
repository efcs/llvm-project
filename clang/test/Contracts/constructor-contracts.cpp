// RUN: %clang_cc1 -fcontracts -std=c++26 -fsyntax-only -verify %s
// expected-no-diagnostics

// Contracts on constructors and destructors

struct Widget {
  int value;

  Widget(int v)
    pre (v >= 0)
    : value(v) {}

  Widget(int v, int max)
    pre (v >= 0)
    pre (v <= max)
    pre (max > 0)
    : value(v) {}

  ~Widget()
    pre (value >= 0)
  {}
};

// Constructor with default arguments
struct Config {
  int timeout;
  int retries;

  Config(int t = 30, int r = 3)
    pre (t > 0)
    pre (r >= 0)
    : timeout(t), retries(r) {}
};

// Copy and move constructors with contracts
struct Buffer {
  int *data;
  int size;

  Buffer(int n)
    pre (n > 0)
    : data(new int[n]), size(n) {}

  Buffer(const Buffer& other)
    pre (other.size > 0)
    : data(new int[other.size]), size(other.size) {}

  Buffer(Buffer&& other)
    pre (other.data != nullptr)
    : data(other.data), size(other.size)
  {
    other.data = nullptr;
    other.size = 0;
  }

  ~Buffer() { delete[] data; }
};

// Template constructor with contracts
template<typename T>
struct Container {
  T value;

  Container(T v)
    pre (v != T{})
    : value(v) {}
};

void test() {
  Widget w1(5);
  Widget w2(3, 10);

  Config c1;
  Config c2(60, 5);

  Buffer b1(10);
  Buffer b2(b1);
  Buffer b3(static_cast<Buffer&&>(b1));

  Container<int> ci(42);
  Container<double> cd(3.14);
}
