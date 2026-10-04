// [dcl.contract.func]p4: A declaration D of a function that is not a first
// declaration shall have either no function-contract-specifier-seq or the
// same one as any first declaration F reachable from D; if D and F are in
// different translation units a diagnostic is required only if D is attached
// to a named module. First declarations of a function in different
// translation units shall specify the same function-contract-specifier-seq,
// no diagnostic required.
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: split-file %s %t
//
// Two partitions which each contain a first declaration of the functions.
// RUN: %clang_cc1 -std=c++26 -fcontracts %t/a.cppm -emit-module-interface -o %t/p-a.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts %t/b.cppm -emit-module-interface -o %t/p-b.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fprebuilt-module-path=%t %t/p.cppm \
// RUN:   -emit-module-interface -o %t/p.pcm -verify
//
// The mismatch is found once the declarations are used.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fprebuilt-module-path=%t %t/use.cpp \
// RUN:   -fsyntax-only -verify
//
// The diagnostic can be downgraded or disabled, since no diagnostic is
// required. As for any other warning in the source of a module, the flags
// the module was built with apply.
// RUN: mkdir %t/warn %t/none
// RUN: %clang_cc1 -std=c++26 -fcontracts %t/b.cppm -emit-module-interface \
// RUN:   -o %t/warn/p-b.pcm -Wno-error=mismatched-contract-specifiers
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodule-file=p:a=%t/p-a.pcm \
// RUN:   -fmodule-file=p:b=%t/warn/p-b.pcm %t/p.cppm -emit-module-interface \
// RUN:   -o %t/warn/p.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodule-file=p:a=%t/p-a.pcm \
// RUN:   -fmodule-file=p:b=%t/warn/p-b.pcm -fmodule-file=p=%t/warn/p.pcm \
// RUN:   %t/use.cpp -fsyntax-only -verify=warn
// RUN: %clang_cc1 -std=c++26 -fcontracts %t/b.cppm -emit-module-interface \
// RUN:   -o %t/none/p-b.pcm -Wno-mismatched-contract-specifiers
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodule-file=p:a=%t/p-a.pcm \
// RUN:   -fmodule-file=p:b=%t/none/p-b.pcm %t/p.cppm -emit-module-interface \
// RUN:   -o %t/none/p.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -fmodule-file=p:a=%t/p-a.pcm \
// RUN:   -fmodule-file=p:b=%t/none/p-b.pcm -fmodule-file=p=%t/none/p.pcm \
// RUN:   %t/use.cpp -fsyntax-only -verify=none
//
// A declaration which is not a first declaration: a diagnostic is required.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fprebuilt-module-path=%t %t/c.cppm \
// RUN:   -emit-module-interface -o %t/p-c.pcm -verify
// RUN: %clang_cc1 -std=c++26 -fcontracts -fprebuilt-module-path=%t %t/impl.cpp \
// RUN:   -fsyntax-only -verify

//--- a.cppm
export module p:a;
export int pred(int x) pre(x > 0);
export int missing(int x) pre(x > 0);
export int count(int x) pre(x > 0);
export int kind(const int x) pre(x > 0);
export int name(const int x) post(r : x > 0);
export template <class T> T tmpl(T x) pre(x > T{});
// Renaming of the parameters, template parameters and of the result binding
// is allowed.
export int same(const int x) pre(x > 0) post(r : r > x);
export template <class T> T tsame(T x) pre(x > T{});
export int lambda(int x) pre([x] { return x > 0; }());
export int omitted(int x) pre(x > 0);
export int redecl(int x) pre(x > 0);

//--- b.cppm
export module p:b;
export int pred(int x) pre(x > 1);
export int missing(int x);
export int count(int x) pre(x > 0) pre(x > 1);
export int kind(const int x) post(x > 0);
export int name(const int x) post(x > 0);
export template <class T> T tmpl(T x) pre(x >= T{});
export int same(const int y) pre(y > 0) post(s : s > y);
export template <class U> U tsame(U y) pre(y > U{});
export int lambda(int y) pre([y] { return y > 0; }());
export int omitted(int x) pre(x > 0);
export int redecl(int x) pre(x > 0);

//--- p.cppm
// expected-no-diagnostics
export module p;
export import :a;
export import :b;

//--- use.cpp
import p;
int main() {
  return pred(1) + missing(1) + count(1) + kind(1) + name(1) + tmpl(1) +
         same(1) + tsame(1) + lambda(1) + omitted(1) + redecl(1);
}
// none-no-diagnostics

// expected-error@b.cppm:2 {{'pred' has different function contract specifiers in different translation units; declaration in module 'p:b' has this predicate in its 1st function contract specifier}}
// expected-note@a.cppm:2 {{but the declaration in module 'p:a' has a different predicate}}
// expected-error@b.cppm:3 {{'missing' has different function contract specifiers in different translation units; declaration in module 'p:b' has no function contract specifiers}}
// expected-note@a.cppm:3 {{but the declaration in module 'p:a' has 1 function contract specifier}}
// expected-error@b.cppm:4 {{'count' has different function contract specifiers in different translation units; declaration in module 'p:b' has 2 function contract specifiers}}
// expected-note@a.cppm:4 {{but the declaration in module 'p:a' has 1 function contract specifier}}
// expected-error@b.cppm:5 {{'kind' has different function contract specifiers in different translation units; declaration in module 'p:b' has a postcondition as its 1st function contract specifier}}
// expected-note@a.cppm:5 {{but the declaration in module 'p:a' has a precondition}}
// expected-error@b.cppm:6 {{'name' has different function contract specifiers in different translation units; declaration in module 'p:b' has a 1st function contract specifier without a result name}}
// expected-note@a.cppm:6 {{but the declaration in module 'p:a' has a function contract specifier with a result name}}
// expected-error@b.cppm:7 {{'tmpl' has different function contract specifiers in different translation units; declaration in module 'p:b' has this predicate in its 1st function contract specifier}}
// expected-note@a.cppm:7 {{but the declaration in module 'p:a' has a different predicate}}

// warn-warning@b.cppm:2 {{'pred' has different function contract specifiers in different translation units}}
// warn-note@a.cppm:2 {{but the declaration in module 'p:a' has a different predicate}}
// warn-warning@b.cppm:3 {{'missing' has different function contract specifiers in different translation units}}
// warn-note@a.cppm:3 {{but the declaration in module 'p:a' has 1 function contract specifier}}
// warn-warning@b.cppm:4 {{'count' has different function contract specifiers in different translation units}}
// warn-note@a.cppm:4 {{but the declaration in module 'p:a' has 1 function contract specifier}}
// warn-warning@b.cppm:5 {{'kind' has different function contract specifiers in different translation units}}
// warn-note@a.cppm:5 {{but the declaration in module 'p:a' has a precondition}}
// warn-warning@b.cppm:6 {{'name' has different function contract specifiers in different translation units}}
// warn-note@a.cppm:6 {{but the declaration in module 'p:a' has a function contract specifier with a result name}}
// warn-warning@b.cppm:7 {{'tmpl' has different function contract specifiers in different translation units}}
// warn-note@a.cppm:7 {{but the declaration in module 'p:a' has a different predicate}}

//--- c.cppm
export module p:c;
import :a;
// The first declarations are reachable from these declarations.
int same(const int y) pre(y > 0) post(s : s > y);
int omitted(int x);
int lambda(int y) pre([y] { return y > 0; }());
int redecl(int x) pre(x > 1); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
// expected-note@a.cppm:14 {{contract previously specified with a non-equivalent condition}}
int missing(int x); // OK, omitted
int count(int x) pre(x > 0) pre(x > 1); // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@a.cppm:4 {{previously declared with fewer contracts here (1 vs 2)}}

//--- impl.cpp
module p;
int same(const int z) pre(z > 0) post(t : t > z) { return z + 1; }
int omitted(int x) { return x; }
int redecl(int x) pre(x > 2) { return x; } // expected-error {{function redeclaration differs in contract specifier sequence}}
// expected-note@-1 {{in contract specified here}}
// expected-note@a.cppm:14 {{contract previously specified with a non-equivalent condition}}

// The first declarations of these functions in the partitions don't match.
// expected-error@b.cppm:* 0+ {{has different function contract specifiers in different translation units}}
// expected-note@a.cppm:* 0+ {{but the declaration in module 'p:a' has}}
