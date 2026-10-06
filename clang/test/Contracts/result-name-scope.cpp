// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// expected-no-diagnostics

// Result names are only visible inside the postcondition in which they are
// declared. Rebuilding a postcondition (for a deduced return type, a template
// instantiation, or a definition that does not repeat the contracts) must not
// leave the result name behind in the identifier resolver, where it would hide
// later block-scope declarations with the same name.

auto deduced(const int x) post(r : r == x) { return x; }

template <class T> auto deduced_tmpl(const T x) post(r : r == x) { return x; }

template <class T> T tmpl(const T x) post(r : r == x);
template <class T> T tmpl(const T x) { return x; }

int declared(const int x) post(r : r == x);
int declared(const int x) { return x; }

struct S {
  auto member(const int x) post(r : r == x) { return x; }
  template <class T> auto member_tmpl(const T x) post(r : r == x) { return x; }
};

int test_deduced() {
  int r = deduced(1);
  r += 1;
  return r;
}

int test_deduced_tmpl() {
  int r = deduced_tmpl(1);
  r += 1;
  return r;
}

int test_tmpl() {
  int r = tmpl(1);
  r += 1;
  return r;
}

int test_declared() {
  int r = declared(1);
  r += 1;
  return r;
}

int test_members() {
  S s;
  int r = s.member(1) + s.member_tmpl(2);
  r += 1;
  return r;
}

int test_nested() {
  int r = 0;
  {
    int r = deduced(2);
    r += 1;
  }
  r += deduced_tmpl(3);
  return r;
}

int test_lambda() {
  auto l = [](const int x) post(r : r == x) { return x; };
  auto d = [](int x) -> auto post(r : r == x) { return x; };
  int r = l(1) + d(2);
  r += 1;
  return r;
}
