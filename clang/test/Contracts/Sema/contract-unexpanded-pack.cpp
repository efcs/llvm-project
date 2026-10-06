// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// A contract condition is a full-expression: like any other, it must not
// contain an unexpanded parameter pack.  This has to be diagnosed at
// template-definition time: a condition that slips through to instantiation
// trips an assertion in DiagnoseUnexpandedParameterPack -- a compiler crash --
// instead of being rejected with a diagnostic.

// Subscripting an unexpanded pack (note: this is NOT pack indexing, which
// would be `args...[0]`).
template <typename... Ts>
void sub(int a, Ts... args) pre(args[0]) {}
// expected-error@-1 {{expression contains unexpanded parameter pack 'args'}}
template void sub<int, int>(int, int, int);

// A bare unexpanded pack in a postcondition.
template <typename... Ts>
void bare(Ts... args) post(args == 0) {}
// expected-error@-1 {{expression contains unexpanded parameter pack 'args'}}
template void bare<int>(int);

// Controls: pack indexing and fold expansions leave no unexpanded pack and
// must remain accepted (preconditions carry no const requirement).
template <typename... Ts>
void ok_idx(const Ts... args) pre(args...[0] > 0) {}
template void ok_idx<int, int>(int, int); // OK, no diagnostic

template <typename... Ts>
void ok_fold(Ts... args) pre((... && (args > 0))) {}
template void ok_fold<int, int>(int, int); // OK, no diagnostic
