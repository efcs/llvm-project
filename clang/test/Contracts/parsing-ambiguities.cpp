// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts

namespace parsing_basic_test {
  int f(const int x)
  pre (x >= 0)
  post (r: r > x); // r is the return value

  struct A {
    bool empty() const noexcept;
    void clear()
      post (empty()); // return value is optional
  };
}

namespace parsing_trailing_return_type_test {
  auto f() -> int pre(true);

  struct S1 {
    auto g() const & noexcept -> int
      pre(true);
  };
}

// [dcl.contract.func]p6: A virtual function, a deleted function, or a function
// defaulted on its first declaration shall not have a
// function-contract-specifier-seq. These still need to parse correctly.
namespace parsing_virtual_test {
  struct A {
    virtual void f(int i)
      pre(i >= 0); // expected-error {{virtual function 'f' cannot have function contract specifiers}}
  };

  struct B : A {
    void f(int i) override final
      pre(i >= 0); // expected-error {{virtual function 'f' cannot have function contract specifiers}}
  };

  struct C : A {
    void f(int i) override
      pre(i >= 0) = 0; // expected-error {{virtual function 'f' cannot have function contract specifiers}}
  };

  struct D : A {
    void f(int i) pre(i >= 0) override; // expected-error {{'override' must appear before function contract specifiers}} \
                                        // expected-error {{virtual function 'f' cannot have function contract specifiers}}
  };
}

namespace parsing_default_delete_pure_test {
  const bool a = true, b = true, c = true;

  struct X {
    X() pre(a) = default; // expected-error {{function defaulted on its first declaration 'X' cannot have function contract specifiers}}
    X(const X&) pre(b) = delete; // expected-error {{deleted function 'X' cannot have function contract specifiers}}
    virtual void f() pre(c) = 0; // expected-error {{virtual function 'f' cannot have function contract specifiers}}
  };
}

namespace parsing_lambda_test {
  auto f1 = [] (int x)
    pre(x > 0) { return x * x; };

  auto f2 = [] (int x) -> int
    pre(x > 0) { return x * x; };
}

namespace parsing_pre_post_names_test {
  void f(bool pre, bool post)
  pre(pre) pre(post);
}

namespace parsing_requires_clause_test {
  template <typename T> concept C = true;

  template <typename T>
  auto g(T x) -> bool
    requires C<T>
    pre (x > 0);
}

namespace parsing_ambig1_test {
  const bool a = true;
  using pre = int;

  auto f() -> pre pre(a);
  // pre is the return type, pre(a) the precondition
}

namespace parsing_ambig2_test {
  const bool a = true;
  template <typename T> struct pre {};
  using post = int;

  auto g() -> pre<post> pre(a);
  // pre<post> is the return type, pre(a) the precondition
}

namespace parsing_ambig3_test {
  constexpr bool a = true;
  constexpr bool pre(bool) { return true; }

  template <typename T>
  void f1() requires (pre(a));
  // just a requires clause, no postcondition

  using b = bool;

  template <typename T>
  void f2() requires ((b)pre(a));
  // just a requires clause, no postcondition

  template <typename T>
  void f3() requires (a) pre(a);
  // pre(a) is a postcondition
}

namespace parsing_ambig_test4 {
  constexpr int a = 1;
  constexpr int b = 2;
  constexpr int c = 0;
  constexpr int pre(int i) { return i; }

  template <typename T>
  void f() requires (a < b > pre(c)); // expected-error {{chained comparison 'X < Y > Z' does not behave the same as a mathematical expression}}
  // just a requires clause, no postcondition

  template <typename T> constexpr bool d = true;
  using e = char;

  template <typename T>
  void g() requires d < e > pre(c);
  // d<e> is a bool variable template, pre(c) is the precondition
}


