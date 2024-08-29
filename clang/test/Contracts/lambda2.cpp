// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify %s -fcontracts


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
      pre(local)    // expected-note {{ capture of local entity 'local' is required }}
                    // expected-note@-1 {{ within contract context introduced here }}


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
              contract_assert(   // expected-note 2 {{ within contract context introduced here }}
                p2
                && p // expected-error {{'p' in not allowed}}
                     // expected-note@-2 {{ capture of local entity 'p' is required }}
                && local   // expected-note {{ capture of local entity 'local' is required }}
                && global );

          }(p);

          [&, p, local](int p2) {
            contract_assert(p2 && p && local && global );

          }(p);

          [=](int p2) { // expected-error {{ 'local' is not allowed }}
                        // expected-error@-1 {{ 'p' is not allowed }}
              contract_assert(p2 && p && local && global );   // expected-note {{ capture of local entity 'local' is required }}
                                                              // expected-note@-1 2 {{ within contract context introduced here }}
                                                              // expected-note@-2 {{ capture of local entity 'p' is required }}

          }(p);
          return 0;
      };
    };
} // namespace ClassScope

namespace VarScope {
using LambdaT = int(*)(int);
template <class T>
inline constexpr LambdaT lambda = +[](int p) {
  int local = 202;

  contract_assert(p && local );
  contract_assert([&](int p2) { return p2 && p && local; }(p));
  contract_assert([=](int p2) { return p2 && p && local;   }(p));

  [&](int p2) {
    contract_assert(p2 && p && local);

  }(p);

  [=](int p2) {
    contract_assert(p2 && p && local );
  }(p);
  return 0;
};

auto Imp = lambda<int>;


} // namespace VarScope

namespace ConstificationContext {
  void f(int p) {
    contract_assert([=]() mutable { return ++p; }());
  }
}
