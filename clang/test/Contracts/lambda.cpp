// RUN: %clang_cc1 -std=c++26 -fsyntax-only -fcolor-diagnostics -verify %s -fcontracts ||  %clang_cc1 -std=c++26 -fsyntax-only -fcolor-diagnostics  %s -fcontracts ||  %clang_cc1 -std=c++26 -fsyntax-only -verify -fcolor-diagnostics  %s -fcontracts


namespace test_one {
constexpr void f() {
  constexpr int z = 42;
  constexpr int zz = 101;
  constexpr auto y = [=, yyy=z, z=z](int x)
    -> int
            pre(x == 1)
            post(r : r == 2) // expected-error {{contract failed during execution of constexpr function}}
            pre(z == 42 && yyy == 42)
            { ((void)z); ((void)yyy); return 1; };
  static_assert(y(1)); // expected-error {{static assertion expression is not an integral constant expression}}
  // expected-note@-1 {{in call to 'y.operator()(1)'}}
}

}

namespace test_two {
  void odr_use(const void*);
  void f(int x) {
    (void)[&]() // expected-error {{'x' is not allowed}}
      pre( // expected-note {{contract}}
      x // expected-note {{required here}}
      ) { };
    (void)[&]() pre(x) { odr_use(&x);};
  }
}

namespace ConstificationDoesntApplyToLambdaLocal {

void foo(int x) {
  contract_assert([](int y) {
    int z = 42;
    ++y; // OK,
    ++z; // OK, it's a local
    return true;
  }(42));
  contract_assert([&](int y) {
    ((void)x);
    ++y;
    ++x; // expected-error {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
    return true;
  }(42));
}

} // namespace ConstificationDoesntApplyToLambdaLocal


int f(bool b) {
  contract_assert([&] { int z = 101; [&]() { return z; }(); return b; }());
}
template <class T, class U>
struct AssertSame;

template <class T>
struct AssertSame<T, T> {
  AssertSame();
    ~AssertSame();
};

template <class T>
struct AssertConstRef;

template <class T>
struct AssertConstRef<const T&> {
  AssertConstRef();
  ~AssertConstRef();
};


template <class T>
struct AssertNonConstRef;

template <class T>
struct AssertNonConstRef<T&> {
  AssertNonConstRef();
  ~AssertNonConstRef();
  static_assert(!__is_const(T), "");
};


namespace DeclTypeHasConst {


int global = 42;

void f(int p) {
  static int z = 42;
  struct A {
    int a = 42;
    const int b = 101;
    int c = -1;

  } v;
  auto &[a, b, c] = v;
  contract_assert(&a == &v.a);

  AssertSame<decltype((v)),  A&>{};
  contract_assert([&]() {
    AssertSame<decltype((v)), const A&>{};
    return true;
  }());
}
} // namespace DeclTypeHasConst

namespace Other {
void f(int x) {
  int a; contract_assert([&] {
    ++a; // expected-error {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
    using T = decltype((a));
    AssertConstRef<T>{};
    return true;
  }()
);
}

} // namespace Other


namespace CaptureByRef {
void foo(bool b) {
  contract_assert([&]() {  return b; }());

}
} // end namespace CaptureByRef

namespace CaptureByVal {
template <class T>
constexpr bool is_const(T &&) {
  return __is_const(T);
}
void foo(bool b) {
  contract_assert([=]() { return b; }());
  contract_assert([=]() mutable {
    static_assert(!is_const(b));
    return b;
  }());
}
} // CaptureByVal

namespace NestedCapture {
void f(bool b) {
  contract_assert([&]() {
    return [&]() {
      return b;
    }();
  }());

  [&]() { // expected-error {{'b' is not allowed}}
    contract_assert([&]() { // expected-note {{within contract}}
      return b; // expected-note {{required here}}
    }());
  }();
}
}
namespace CrashTest {
void bar(bool b) { [&]() {  // expected-error {{'b' is not allowed}}
  contract_assert(b); // expected-note {{required here}} expected-note {{within contract}}
}(); }
} // namespace CrashTest

namespace DoubleDeep {

  void foo(int a);
  void foo(int a) {
    [&](int y) {
      ++y;
      [&] (int x) { // expected-error {{'a' is not allowed}}
        ++y;
        ++x;
        contract_assert( // expected-note {{contract}}
          [&]
          (int v) {
            ++y; // expected-error {{inside a contract}}
            ++x; // expected-error {{inside a contract}}
            ++a; // expected-error {{inside a contract}} expected-note {{required here}}
            ++v; // OK

          return [&](int w) {
            ++a; // expected-error {{inside a contract}}
            ++x; // expected-error {{inside a contract}}
            ++y; // expected-error {{inside a contract}}
            ++v; // OK
            ++w; // OK
            return true;
          }(42);
        }(12));
      }(y);
    }(a);
  }
} // namespace DoubleDeep

namespace MemberRef {
struct A {
  int x;
  void f();
};

void A::f() {
  contract_assert([&] {
    [&]() {
      ++x; // expected-error {{inside of a contract}}
    }();
    return true;
  }());
}
} // namespace MemberRef


namespace CaptureThisByRef {
  struct A {
    int x;
    void f() {
      [&] {
        AssertSame<decltype(this), A*>{};
        contract_assert([&]() {
          AssertSame<decltype((this)), const A*>{};
          ++x; // expected-error {{inside of a contract}}

          struct U {
            int z = 42;
            void f() {
              ++z;
            }
          };
          return true;
        }());
      }();
    }
  };
}