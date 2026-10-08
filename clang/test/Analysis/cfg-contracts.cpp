// The CFG of a function with contract assertions does not depend on the
// evaluation semantic: the predicate of a contract assertion is always part
// of the CFG, followed by the contract assertion itself. Preconditions are
// evaluated on entry, postconditions in a block of their own which every
// return statement (and the end of the body) leads to, before the exit block.
//
// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -analyzer-checker=debug.DumpCFG %s > %t 2>&1
// RUN: FileCheck --input-file=%t %s
// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -analyzer-checker=debug.DumpCFG %s > %t 2>&1
// RUN: FileCheck --input-file=%t %s
// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe -analyzer-checker=debug.DumpCFG %s > %t 2>&1
// RUN: FileCheck --input-file=%t %s
// RUN: %clang_analyze_cc1 -std=c++26 -fcontracts -fcontract-group-evaluation-semantic=grp=ignore -analyzer-checker=debug.DumpCFG %s > %t 2>&1
// RUN: FileCheck --input-file=%t %s

bool check(int);
int gl;

// CHECK-LABEL: int f(const int x) pre(x > 0) post(r : r > x)
// CHECK: [B6 (ENTRY)]
// CHECK-NEXT: Succs (1): B5
// CHECK: [B1]
// CHECK-NEXT: 1: r
// CHECK-NEXT: 2: [B1.1] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT: 3: x
// CHECK-NEXT: 4: [B1.3] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT: 5: [B1.2] > [B1.4]
// CHECK-NEXT: 6: post(r : [B1.5])
// CHECK-NEXT: Preds (2): B2 B3
// CHECK-NEXT: Succs (1): B0
// CHECK: [B2]
// CHECK-NEXT: 1: check
// CHECK-NEXT: 2: [B2.1] (ImplicitCastExpr, FunctionToPointerDecay, _Bool (*)(int))
// CHECK-NEXT: 3: x
// CHECK-NEXT: 4: [B2.3] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT: 5: [B2.2]([B2.4])
// CHECK-NEXT: 6: contract_assert {{\[\[}}clang::contract_group("grp"){{\]\]}} ([B2.5]);
// CHECK-NEXT: 7: x
// CHECK-NEXT: 8: [B2.7] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT: 9: 1
// CHECK-NEXT: 10: [B2.8] + [B2.9]
// CHECK-NEXT: 11: return [B2.10];
// CHECK-NEXT: Preds (1): B4
// CHECK-NEXT: Succs (1): B1
// CHECK: [B3]
// CHECK-NEXT: 1: x
// CHECK-NEXT: 2: [B3.1] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT: 3: 1
// CHECK-NEXT: 4: [B3.2] - [B3.3]
// CHECK-NEXT: 5: return [B3.4];
// CHECK-NEXT: Preds (1): B4
// CHECK-NEXT: Succs (1): B1
// CHECK: [B4]
// CHECK: T: if [B4.4]
// CHECK: [B5]
// CHECK-NEXT: 1: x
// CHECK-NEXT: 2: [B5.1] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT: 3: 0
// CHECK-NEXT: 4: [B5.2] > [B5.3]
// CHECK-NEXT: 5: pre([B5.4])
// CHECK-NEXT: Preds (1): B6
// CHECK-NEXT: Succs (1): B4
// CHECK: [B0 (EXIT)]
// CHECK-NEXT: Preds (1): B1
int f(const int x) pre(x > 0) post(r : r > x) {
  if (x > 10)
    return x - 1;
  contract_assert [[clang::contract_group("grp")]] (check(x));
  return x + 1;
}

// CHECK-LABEL: void v(int x) post(gl > 0)
// CHECK: [B1]
// CHECK-NEXT: 1: gl
// CHECK-NEXT: 2: [B1.1] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT: 3: 0
// CHECK-NEXT: 4: [B1.2] > [B1.3]
// CHECK-NEXT: 5: post([B1.4])
// CHECK-NEXT: Preds (2): B2 B3
// CHECK-NEXT: Succs (1): B0
// CHECK: [B2]
// CHECK: 4: [B2.3] = [B2.2]
// CHECK-NEXT: Preds (1): B4
// CHECK-NEXT: Succs (1): B1
// CHECK: [B3]
// CHECK-NEXT: 1: return;
// CHECK-NEXT: Preds (1): B4
// CHECK-NEXT: Succs (1): B1
// CHECK: [B0 (EXIT)]
// CHECK-NEXT: Preds (1): B1
void v(int x) post(gl > 0) {
  if (x)
    return;
  gl = x;
}

// CHECK-LABEL: S(int x) pre(x > 0)
// CHECK: [B1]
// CHECK: 3: m([B1.2]) (Member initializer)
// CHECK-NEXT: Preds (1): B2
// CHECK-NEXT: Succs (1): B0
// CHECK: [B2]
// CHECK: 5: pre([B2.4])
// CHECK-NEXT: Preds (1): B3
// CHECK-NEXT: Succs (1): B1
struct S {
  S(int x) pre(x > 0) : m(x) {}
  int m;
};

// The postconditions of a coroutine are those of the function as seen by its
// callers, evaluated when the ramp returns. The CFG models the coroutine as if
// its body ran to completion before that: a co_return statement, like a
// return statement, continues into the postconditions. (The entry block of
// the postconditions of co1 is B1, which the block of the co_return, B2,
// continues into.)
namespace std {
template <class R, class...> struct coroutine_traits {
  using promise_type = typename R::promise_type;
};
template <class P = void> struct coroutine_handle {
  coroutine_handle() = default;
  template <class Q> coroutine_handle(coroutine_handle<Q>);
  static coroutine_handle from_address(void *);
};
struct suspend_never {
  bool await_ready() noexcept;
  void await_suspend(coroutine_handle<>) noexcept;
  void await_resume() noexcept;
};
} // namespace std
struct Task {
  struct promise_type {
    Task get_return_object();
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void return_value(int);
    void unhandled_exception();
  };
  bool valid() const;
};
struct VoidTask {
  struct promise_type {
    VoidTask get_return_object();
    std::suspend_never initial_suspend();
    std::suspend_never final_suspend() noexcept;
    void return_void();
    void unhandled_exception();
  };
};
// CHECK-LABEL: Task co1(const int x) pre(x > 0) post(r : r.valid())
// CHECK: [B1]
// CHECK-NEXT: 1: r
// CHECK-NEXT: 2: [B1.1].valid
// CHECK-NEXT: 3: [B1.2]()
// CHECK-NEXT: 4: post(r : [B1.3])
// CHECK-NEXT: Preds (1): B2
// CHECK-NEXT: Succs (1): B0
// CHECK: [B2]
// CHECK: co_return [B2.{{[0-9]+}}];{{ *}}Preds (1): B3
// CHECK-NEXT: Succs (1): B1
// CHECK: [B3]
// CHECK: 5: pre([B3.4])
// CHECK: [B0 (EXIT)]
// CHECK-NEXT: Preds (1): B1
Task co1(const int x) pre(x > 0) post(r : r.valid()) {
  co_return x;
}
// CHECK-LABEL: VoidTask co2(int x) post(gl > 0)
// CHECK: [B1]
// CHECK-NEXT: 1: gl
// CHECK-NEXT: 2: [B1.1] (ImplicitCastExpr, LValueToRValue, int)
// CHECK-NEXT: 3: 0
// CHECK-NEXT: 4: [B1.2] > [B1.3]
// CHECK-NEXT: 5: post([B1.4])
// CHECK-NEXT: Preds (2): B2 B3
// CHECK-NEXT: Succs (1): B0
// CHECK: [B0 (EXIT)]
// CHECK-NEXT: Preds (1): B1
VoidTask co2(int x) post(gl > 0) {
  if (x)
    co_return;
  gl = x;
}
