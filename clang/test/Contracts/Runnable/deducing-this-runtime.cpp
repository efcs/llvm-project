// REQUIRES: contracts-libcxx
// RUN: %clangxx -std=c++26 %s -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -o %t && %t

// What an explicit object parameter DENOTES inside a contract assertion.
//
// Every check below reports the value or address the predicate actually saw.
// A compile-only probe would prove nothing here: the object-identity family
// of contract bugs (a predicate pointed at a copy, at a stale frame, or at
// the closure instead of the enclosing class) all compile cleanly and then
// read the wrong object.
//
// GCC mirror: g++.dg/contracts/cpp26/deducing-this-runtime.C

#include <contracts>

static int violations = 0;

void handle_contract_violation(const std::contracts::contract_violation &) {
  ++violations;
}

enum { PRE = 0, BODY = 1, POST = 2, NSLOT = 3 };

static const void *addr[NSLOT];
static int val[NSLOT];

// Recording passes an INDEX rather than the address of the destination: a
// predicate const-qualifies the variables it names, so `&slot` is a pointer
// to const in there.
static bool note(int slot, const void *p, int v) {
  addr[slot] = p;
  val[slot] = v;
  return true;
}

// A by-reference explicit object parameter names the caller's object, and a
// postcondition sees what the body did to it.
struct ByRef {
  int x = 1;

  void bump(this ByRef &self)
      pre(note(PRE, &self, self.x))
      post(note(POST, &self, self.x)) {
    note(BODY, &self, self.x);
    self.x = 99;
  }
};

// A by-value explicit object parameter names the PARAMETER, and it is one
// object across the precondition, the body and the postcondition.
struct ByVal {
  int x = 1;

  void peek(this const ByVal self)
      pre(note(PRE, &self, self.x))
      post(note(POST, &self, self.x)) {
    note(BODY, &self, self.x);
  }
};

// CRTP: the explicit object parameter deduces to the DERIVED type, so a
// predicate written on the base names a derived member.
struct Base {
  template <class Self> int value(this const Self &self) post(r : r == self.raw()) {
    return self.raw();
  }
};

struct Derived : Base {
  int raw() const { return 42; }
};

// An explicit-object operator(), reachable both by call syntax and through a
// plain function pointer -- taking the address of an explicit object member
// function yields a pointer to function, not a pointer to member, and
// [dcl.contract.func]/8 requires the contracts to run either way.
struct Callable {
  int k = 7;

  int operator()(this const Callable &self, const int n)
      pre(n > 0) post(r : r == self.k * n) {
    return self.k * n;
  }
};

int main() {
  {
    ByRef o;
    o.bump();
    if (addr[PRE] != (const void *)&o)
      __builtin_abort();
    if (addr[PRE] != addr[BODY] || addr[BODY] != addr[POST])
      __builtin_abort();
    if (val[PRE] != 1 || val[POST] != 99 || o.x != 99)
      __builtin_abort();
  }

  {
    ByVal o;
    o.peek();
    if (addr[PRE] == (const void *)&o)
      __builtin_abort(); // the parameter is a copy
    if (addr[PRE] != addr[BODY] || addr[BODY] != addr[POST])
      __builtin_abort();
    if (val[PRE] != 1 || val[POST] != 1)
      __builtin_abort();
  }

  {
    Derived d;
    if (d.value() != 42)
      __builtin_abort();
  }

  {
    auto twice = [](this auto &&self, const int n) pre(n >= 0) post(r : r == 2 * n) {
      (void)self;
      return 2 * n;
    };
    if (twice(3) != 6)
      __builtin_abort();

    // A recursive lambda needs an explicit return type: its operator() is
    // used before it is defined.
    auto fact = [](this auto &&self, const int n) -> int pre(n >= 0) post(r : r >= 1) {
      return n <= 1 ? 1 : n * self(n - 1);
    };
    if (fact(5) != 120)
      __builtin_abort();
  }

  {
    Callable c;
    if (c(3) != 21)
      __builtin_abort();

    int (*pf)(const Callable &, const int) = &Callable::operator();
    if (pf(c, 2) != 14)
      __builtin_abort();

    // The contracts fire on a direct call and on an indirect one alike.
    const int before = violations;
    c(0);
    if (violations - before != 1)
      __builtin_abort();
    pf(c, 0);
    if (violations - before != 2)
      __builtin_abort();
  }

  return 0;
}
