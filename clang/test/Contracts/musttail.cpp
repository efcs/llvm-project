// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcontract-evaluation-semantic=ignore -fsyntax-only -verify %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -emit-llvm -o - %s -DCODEGEN | FileCheck %s

bool check(int);

int callee(int x) pre(check(x)) post(r : check(r));

#ifndef CODEGEN
// The postcondition assertions of a function are evaluated after the callee
// returns, so a function with postconditions cannot perform a tail call.
int post(int x) post(r : check(r)) {
  [[clang::musttail]] return callee(x); // expected-error {{cannot perform a tail call from a function with postcondition assertions}} \
                                        // expected-note {{tail call required by 'clang::musttail' attribute here}}
}

int post_on_first_decl(int x) post(r : check(r));
int post_on_first_decl(int x) {
  [[clang::musttail]] return callee(x); // expected-error {{cannot perform a tail call from a function with postcondition assertions}} \
                                        // expected-note {{tail call required by 'clang::musttail' attribute here}}
}

template <class T> T post_template(T x) post(r : check(r)) {
  [[clang::musttail]] return callee(x); // expected-error {{cannot perform a tail call from a function with postcondition assertions}} \
                                        // expected-note {{tail call required by 'clang::musttail' attribute here}}
}
int use_post_template(int x) { return post_template(x); } // expected-note {{in instantiation of function template specialization 'post_template<int>' requested here}}

auto post_lambda = [](int x) -> int post(r : check(r)) {
  [[clang::musttail]] return callee(x); // expected-error {{cannot perform a tail call from a function with postcondition assertions}} \
                                        // expected-note {{tail call required by 'clang::musttail' attribute here}}
};
#endif

// A function with preconditions can: they are evaluated before the call. The
// function contract assertions of the callee are evaluated by the callee.
//
// CHECK-LABEL: define {{.*}} @_Z3prei(
// CHECK: call {{.*}} @_Z5checki(
// CHECK: musttail call noundef i32 @_Z6calleei(
// CHECK-NEXT: ret i32
int pre(int x) pre(check(x)) {
  contract_assert(check(x));
  [[clang::musttail]] return callee(x);
}
