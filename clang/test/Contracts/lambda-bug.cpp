// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fcolor-diagnostics %s && exit 1

template <class T>
constexpr bool is_const(T&) { return __is_const(T); }

namespace CaptureByRef {
void foo(bool b) {
  contract_assert([&]() {  return b; }());

}
} // end namespace CaptureByRef
#if 1
namespace CaptureByVal {
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

  [&]() {
    contract_assert([&]() {
        return b;
    }());
  };
}
}
void bar(bool b) { [&]() { contract_assert(b); }(); }

#endif