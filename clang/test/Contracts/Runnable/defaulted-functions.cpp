// Function contract specifiers are allowed on functions which are explicitly
// defaulted after their first declaration ([dcl.contract.func]p6). Such
// functions are user-provided, so every call evaluates the contracts exactly
// once, around the implicitly defined body.
//
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O0 %s -o %t.observe
// RUN: %t.observe
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -O2 %s -o %t.observe.O2
// RUN: %t.observe.O2
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=enforce -O1 -fno-elide-constructors %s -o %t.enforce
// RUN: %t.enforce
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=quick_enforce -O2 %s -o %t.quick
// RUN: %t.quick
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -O0 -DIGNORED %s -o %t.ignore
// RUN: %t.ignore

#include "contracts-runtime.h"
#include "event-log.h"
#include <compare>

void handle_contract_violation(const std::contracts::contract_violation &) {
  ev("violation");
}

__attribute__((noinline)) bool check(const char *Event) { return ev(Event); }

struct Member {
  Member() { ev("Member()"); }
  Member(const Member &) { ev("Member(copy)"); }
  Member(Member &&) { ev("Member(move)"); }
  Member &operator=(const Member &) { return ev("Member=copy"), *this; }
  Member &operator=(Member &&) { return ev("Member=move"), *this; }
  ~Member() { ev("~Member()"); }
  bool operator==(const Member &) const { return ev("Member=="); }
  std::strong_ordering operator<=>(const Member &) const {
    return ev("Member<=>"), std::strong_ordering::equal;
  }
};

struct S {
  Member M;
  S() pre(check("pre")) post(check("post"));
  S(const S &) pre(check("pre")) post(check("post"));
  S(S &&) pre(check("pre")) post(check("post"));
  S &operator=(const S &) pre(check("pre")) post(R : check("post") && &R == this);
  S &operator=(S &&) pre(check("pre")) post(R : check("post") && &R == this);
  ~S() pre(check("~pre")) post(check("~post"));
  bool operator==(const S &) const pre(check("pre")) post(R : check("post") && R);
  std::strong_ordering operator<=>(const S &) const
      pre(check("pre")) post(R : check("post") && R == 0);
};
S::S() = default;
S::S(const S &) = default;
S::S(S &&) = default;
S &S::operator=(const S &) = default;
S &S::operator=(S &&) = default;
S::~S() = default;
bool S::operator==(const S &) const = default;
std::strong_ordering S::operator<=>(const S &) const = default;

// A class which would be trivially copyable and destructible if its special
// members were defaulted on their first declaration: the calls cannot be
// replaced by a memcpy or dropped.
struct Trivial {
  int I = 0;
  Trivial() pre(check("pre")) post(check("post"));
  Trivial(const Trivial &) pre(check("pre")) post(check("post"));
  Trivial &operator=(const Trivial &)
      pre(check("pre")) post(R : check("post") && R.I == 1);
  ~Trivial() pre(check("~pre")) post(check("~post"));
  friend bool operator==(const Trivial &, const Trivial &)
      pre(check("pre")) post(R : check("post") && !R);
};
Trivial::Trivial() = default;
Trivial::Trivial(const Trivial &) = default;
Trivial &Trivial::operator=(const Trivial &) = default;
Trivial::~Trivial() = default;
bool operator==(const Trivial &, const Trivial &) = default;

union Union {
  int I;
  float F;
  Union() pre(check("pre")) post(check("post"));
  Union(const Union &) pre(check("pre")) post(check("post"));
  Union &operator=(const Union &)
      pre(check("pre")) post(R : check("post") && R.I == 1);
  ~Union() pre(check("~pre")) post(check("~post"));
};
Union::Union() = default;
Union::Union(const Union &) = default;
Union &Union::operator=(const Union &) = default;
Union::~Union() = default;

template <class T> __attribute__((noinline)) T pass(T &V) { return V; }

int main() {
  {
    S A;
    EXPECT_EVENTS(C("pre;") "Member();" C("post;"));
    S B(A);
    EXPECT_EVENTS(C("pre;") "Member(copy);" C("post;"));
    S D(static_cast<S &&>(A));
    EXPECT_EVENTS(C("pre;") "Member(move);" C("post;"));
    A = B;
    EXPECT_EVENTS(C("pre;") "Member=copy;" C("post;"));
    A = static_cast<S &&>(B);
    EXPECT_EVENTS(C("pre;") "Member=move;" C("post;"));
    (void)(A == B);
    EXPECT_EVENTS(C("pre;") "Member==;" C("post;"));
    (void)(A != B);
    EXPECT_EVENTS(C("pre;") "Member==;" C("post;"));
    (void)(A <=> B);
    EXPECT_EVENTS(C("pre;") "Member<=>;" C("post;"));
    (void)(A < B);
    EXPECT_EVENTS(C("pre;") "Member<=>;" C("post;"));
  }
  EXPECT_EVENTS(C("~pre;") "~Member();" C("~post;")
                C("~pre;") "~Member();" C("~post;")
                C("~pre;") "~Member();" C("~post;"));

  {
    Trivial A;
    EXPECT_EVENTS(C("pre;post;"));
    Trivial B(A);
    EXPECT_EVENTS(C("pre;post;"));
    B.I = 1;
    A = B;
    EXPECT_EVENTS(C("pre;post;"));
    A.I = 2;
    (void)(A == B);
    EXPECT_EVENTS(C("pre;post;"));
    (void)(A != B);
    EXPECT_EVENTS(C("pre;post;"));
    {
      Trivial Arr[2];
      EXPECT_EVENTS(C("pre;post;pre;post;"));
    }
    EXPECT_EVENTS(C("~pre;~post;~pre;~post;"));
    delete new Trivial;
    EXPECT_EVENTS(C("pre;post;~pre;~post;"));
    { Trivial Copy = pass(A); }
    EXPECT_EVENTS(C("pre;post;~pre;~post;"));
  }
  EXPECT_EVENTS(C("~pre;~post;~pre;~post;"));

  {
    Union A;
    EXPECT_EVENTS(C("pre;post;"));
    A.I = 1;
    Union B(A);
    EXPECT_EVENTS(C("pre;post;"));
    Union D;
    EXPECT_EVENTS(C("pre;post;"));
    D = B;
    EXPECT_EVENTS(C("pre;post;"));
    if (D.I != 1)
      abort();
  }
  EXPECT_EVENTS(C("~pre;~post;~pre;~post;~pre;~post;"));
  return 0;
}
