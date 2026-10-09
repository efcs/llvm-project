// -ast-print must emit a contract specifier as the syntax it was written in,
// so that the printed declaration parses back.  Three things used to make it
// not: the sequence was wrapped in `[[...]]` with no separator between
// specifiers, a postcondition's result-name introducer was dropped, and the
// attribute-specifier-seq of a specifier was not printed at all.
//
// The second RUN line is the real test -- compile what we printed -- and the
// FileCheck pass pins the text, because re-parsing alone would also accept
// output that silently dropped an attribute.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -ast-print %s > %t.cpp
// RUN: FileCheck --input-file=%t.cpp %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only %t.cpp

int two_pre(int x) pre(x > 0) pre(x < 10);
// CHECK: int two_pre(int x) pre(x > 0) pre(x < 10);

// A postcondition may only name a parameter that is declared const.
int result_name(const int x) post(r : r > x);
// CHECK: int result_name(const int x) post(r : r > x);

int attributed(int x) pre [[clang::contract_group("grp")]] (x != 0);
// `[[` is FileCheck's variable syntax, so the attribute's brackets have to be
// escaped in the expectation.
// CHECK: int attributed(int x) pre {{\[\[}}clang::contract_group("grp")]] (x != 0);

void with_assert(int x) {
  contract_assert(x > 0);
}
// CHECK: contract_assert(x > 0);
