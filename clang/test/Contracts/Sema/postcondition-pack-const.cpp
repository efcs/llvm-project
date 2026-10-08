// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [dcl.contract.func]: a non-reference value parameter odr-used in a
// postcondition must be declared const.  When the parameter is reached through
// pack indexing (args...[i]), only the *selected* element is odr-used, so the
// requirement applies to that element: it must not be skipped, which silently
// accepts a non-const element, and it must not be applied to the whole pack,
// which rejects a valid one.

// --- Selected element is non-const -> error. ---
template <typename... Ts>
void sel_nonconst(Ts... args) // expected-note {{parameter of type 'int' is declared here}}
    post(args...[0] == 0) {}  // expected-error {{parameter 'args' referenced in contract postcondition must be declared const}}
template void sel_nonconst<int>(int);
// expected-note@-1 {{in instantiation of function template specialization 'sel_nonconst<int>' requested here}}

// --- Selected element (index 1) IS const, even though a sibling is not ->
//     OK: only the odr-used element is constrained. ---
template <typename... Ts>
void sel_const(Ts... args) post(args...[1] == 0) {}
template void sel_const<int, const int>(int, const int); // OK, no diagnostic

// --- Selected element (index 0) is non-const while a sibling is const ->
//     error (must not be masked by the const sibling). ---
template <typename... Ts>
void sel_nonconst_sibling(Ts... args) // expected-note {{parameter of type 'int' is declared here}}
    post(args...[0] == 0) {}           // expected-error {{parameter 'args' referenced in contract postcondition must be declared const}}
template void sel_nonconst_sibling<int, const int>(int, const int);
// expected-note@-1 {{in instantiation of function template specialization 'sel_nonconst_sibling<int, const int>' requested here}}

// --- Whole-pack const (const Ts... args) makes every element const -> OK. ---
template <typename... Ts>
void all_const(const Ts... args) post(args...[0] == 0) {}
template void all_const<int>(const int); // OK, no diagnostic

// --- Fold expansion over a non-const pack in a postcondition -> error
//     (already worked; kept here so the fold path stays covered). ---
template <typename... Ts>
void fold_nonconst(Ts... args)          // expected-note {{parameter of type 'int' is declared here}}
    post((... && (args == 0))) {}       // expected-error {{parameter 'args' referenced in contract postcondition must be declared const}}
template void fold_nonconst<int>(int);
// expected-note@-1 {{in instantiation of function template specialization 'fold_nonconst<int>' requested here}}
