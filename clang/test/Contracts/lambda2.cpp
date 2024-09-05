// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts ||  %clang_cc1 -std=c++26 -fsyntax-only -fcolor-diagnostics  %s -fcontracts || %clang_cc1 -fcolor-diagnostics -std=c++26 -fsyntax-only -verify %s -fcontracts


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

namespace GlobalScope {
  int global = 42;
  struct A {
    int member = 101;
  } global_a{};

  auto global_lambda = [](int p) pre(global) post(global) {
    int local = 202;

    contract_assert(p && local && global && global_a.member);
    contract_assert([&](int p2) { return p2 && p && local && global && global_a.member; }(p));
    contract_assert([=](int p2) { return p2 && p && local && global && global_a.member; }(p));

    [&](int p2) {
      contract_assert(p2 && p && local && global && global_a.member);
      ((void)p);
      ((void)local);
    }(p);

    [&]() // expected-error {{ 'local' is not allowed }}
      pre(local)    // expected-note {{required here}} expected-note {{contract context}}
      post(local)
    {}();

    [=](int p2) {
      contract_assert(p2 && p && local && global && global_a.member);
      ((void)p);
      ((void)local);
    }(p);
  };
} // namespace GlobalScope

namespace ClassScope {
  int global;

    struct A {
        using lambda_t = int(*)(int);
        int member;
        lambda_t lambda = +[](int p) {
          int local = 202;

          contract_assert(
            p
            && local
            && global);
          contract_assert([&](int p2) { return p2 && p && local && global; }(p));
          contract_assert([=](int p2) { return p2 && p && local && global; }(p));

          [&](int p2) { // expected-error {{ 'local' is not allowed }}
                        // expected-error@-1 {{ 'p' is not allowed }}
              contract_assert(   // expected-note 2 {{contract context}}
                p2
                && p // expected-note {{required here}}
                && local   // expected-note {{required here}}
                && global );

          }(p);

          [&, p, local](int p2) {
            contract_assert(p2 && p && local && global );

          }(p);

          [=](int p2) { // expected-error {{ 'local' is not allowed }} expected-error {{'p'}}
              contract_assert( // expected-note 2 {{contract context}}
                p2 && p // expected-note {{here}}
                && local && global ); // expected-note {{here}}
          }(p);
          return 0;
      };
    };
} // namespace ClassScope

namespace VarScope {
using LambdaT = int(*)(int, int);
template <class T>
inline constexpr LambdaT lambda = +[](int p, int pppp) { // expected-note {{while substituting}}
  int local = 202;

  contract_assert(p && local );
  contract_assert(
    [&]
    (int p2) { return
      p2 ||
      pppp ||
      local; }
      (
      p)
  );
  contract_assert([=](int p2) { return p2 || p || local;   }(p));

  [&](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert( // expected-note 2 {{contract context}}
      p2
      &&
      p // expected-note {{here}}
      &&
      local); // expected-note {{here}}

  }(p);

  [=](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert( // expected-note 2 {{contract context}}
      p2 && p && local );  // expected-note 2 {{here}}
  }(p);
  return 0;
};

auto Imp = lambda<int>; // expected-note {{here}}


} // namespace VarScope

namespace NonDependentVarScope {
auto lambda = +[](int p) {
  int local = 202;

  contract_assert(p && local );
  contract_assert(
  [&]
  (int p2) {
    return
      p2 &&
      p &&
      local;
  }(p));
  contract_assert([=](int p2) { return p2 && p && local;   }(p));

    [&](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
      contract_assert(p2 && p && local); // expected-note 2 {{contract context}} expected-note 2 {{required here}}

    }(p);

    [=](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
      contract_assert(p2 && p && local ); // expected-note 2 {{contract context}} expected-note 2 {{required here}}
    }(p);
    return 0;
  };


auto tmpl_lambda = [](auto p) {
  int local = 202;

  contract_assert(p || local );
  contract_assert(
      [&]
          (int p2) {
        return
            p2 ||
                p ||
                local;
      }(p));
  contract_assert([=](int p2) { return p2 || p || local;   }(p));

  [&](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert(p2 || p || local); // expected-note 2 {{contract context}} expected-note 2 {{required here}}

  }(p);

  [=](int p2) { // expected-error {{'p'}} expected-error {{'local'}}
    contract_assert(p2 || p || local ); // expected-note 2 {{contract context}} expected-note 2 {{required here}}
  }(p);
  return 1;
}(1); // expected-note {{here}}

}

namespace ConstificationContext {
  void f(int p) {
    contract_assert([&]() { return [&] () { return ++p; }(); }()); // expected-error {{cannot assign to a variable captured by reference which was captured as const because it is inside a contract}}
    contract_assert([=]() mutable { return [&] () { return ++p; }(); }()); // ok
    contract_assert([&]() { return [=]() mutable { return ++p; }(); }() ); // expected-error {{cannot assign}}
  }
}

