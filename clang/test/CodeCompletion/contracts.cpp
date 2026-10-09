bool check(int);
int gl;
struct S { int member; bool ok() const; };

int f(const int x, const int y, const S s)
  pre(check(x))
  // RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -code-completion-at=%s:%(line-1):9 %s -o - | FileCheck -check-prefix=CHECK-PRE %s
  // CHECK-PRE: COMPLETION: check : [#bool#]check(<#int#>)
  post(r : r > x && y)
  // RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -code-completion-at=%s:%(line-1):12 %s -o - | FileCheck -check-prefix=CHECK-POST %s
  // CHECK-POST: COMPLETION: gl : [#int#]gl
  // CHECK-POST: COMPLETION: r : [#int#]r
  // CHECK-POST: COMPLETION: x : [#const int#]x
  // CHECK-POST: COMPLETION: y : [#const int#]y
  post(s.member > 0) {
  // RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -code-completion-at=%s:%(line-1):10 %s -o - | FileCheck -check-prefix=CHECK-MEMBER %s
  // CHECK-MEMBER: COMPLETION: member : [#int#]member
  // CHECK-MEMBER: COMPLETION: ok : [#bool#]ok()[# const#]
  contract_assert(x > gl);
  // RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -code-completion-at=%s:%(line-1):23 %s -o - | FileCheck -check-prefix=CHECK-ASSERT %s
  // CHECK-ASSERT: COMPLETION: gl : [#int#]gl
  // CHECK-ASSERT: COMPLETION: x : [#const int#]x
  return x;
}

auto l = [](int v) pre(v > 0) post(res : res > v) { return v; };
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -code-completion-at=%s:%(line-1):42 %s -o - | FileCheck -check-prefix=CHECK-LAMBDA %s
// CHECK-LAMBDA: COMPLETION: res : [#int#]res
// CHECK-LAMBDA: COMPLETION: v : [#int#]v
