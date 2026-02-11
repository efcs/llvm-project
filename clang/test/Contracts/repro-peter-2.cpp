// RUN: %clang_cc1 -std=c++26 -fsyntax-only -fcolor-diagnostics -verify %s -fcontracts ||  %clang_cc1 -std=c++26 -fsyntax-only -fcolor-diagnostics  %s -fcontracts ||  %clang_cc1 -std=c++26 -fsyntax-only -verify -fcolor-diagnostics  %s -fcontracts
using size_t = decltype(sizeof(int));

template <size_t x> struct UT { constexpr int size() const { return x; }};

template <size_t bits>
class t {
  explicit t(const UT<bits> f) pre(f.size() == bits/8) {}
};

template
t<128>::t(UT<128> f);

template <>
t<127>::t(const UT<127> f) {}