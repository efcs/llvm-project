// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [dcl.decl.general], [class.mem.general]: A function-contract-specifier-seq
// shall be present only if the declarator declares a function.
int x pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
int (*fp)() pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
typedef void F() pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
int y = 0, z pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
struct S {
  int m pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
  int bf : 3 pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
};
void local() {
  int l pre(true); // expected-error {{function contract specifiers are only allowed on function declarations}}
}

// [dcl.contract.func]p6: A virtual function, a deleted function, or a function
// defaulted on its first declaration shall not have a
// function-contract-specifier-seq.
struct B {
  virtual void f() pre(true); // expected-error {{virtual function 'f' cannot have function contract specifiers}}
  virtual void g();
  virtual void h();
  virtual void i() pre(true) final; // expected-error {{'final' must appear before function contract specifiers}} \
                            // expected-error {{virtual function 'i' cannot have function contract specifiers}}
};
struct D : B {
  void g() override pre(true); // expected-error {{virtual function 'g' cannot have function contract specifiers}}
};
struct D2 : B {
  void h() pre(true); // expected-error {{virtual function 'h' cannot have function contract specifiers}}
};
template <class Base> struct TD : Base {
  void g() pre(true) {} // expected-error {{virtual function 'g' cannot have function contract specifiers}}
};
template struct TD<B>; // expected-note {{in instantiation of member function 'TD<B>::g' requested here}}

struct E {
  E() pre(true) = default; // expected-error {{function defaulted on its first declaration 'E' cannot have function contract specifiers}}
  E(int) pre(true) = delete; // expected-error {{deleted function 'E' cannot have function contract specifiers}}
  bool operator==(const E &) const pre(true) = default; // expected-error {{function defaulted on its first declaration 'operator==' cannot have function contract specifiers}}
};
void del() pre(true) = delete; // expected-error {{deleted function 'del' cannot have function contract specifiers}}

// Defaulted on a later declaration is fine.
struct Y {
  Y() pre(true);
};
Y::Y() pre(true) = default;

// A result name may only appear on a postcondition.
int g() pre(r: true); // expected-error {{result name 'r' not allowed outside of post condition specifier}}
void v() post(r: true); // expected-error {{result name 'r' cannot be used with a void return type}}
template <class T> T tv() post(r: true) { return T(); } // expected-error {{result name 'r' cannot be used with a void return type}}
void use_tv() { tv<void>(); } // expected-note {{in instantiation of function template specialization 'tv<void>' requested here}}

// Errors in late-parsed member contracts must not crash.
struct Junk {
  void a() pre(1 2); // expected-error {{expected ')'}} expected-note {{to match this '('}}
  void h() pre(1 2) post(r: ); // expected-error {{expected ')'}} expected-note {{to match this '('}} \
                               // expected-error {{result name 'r' cannot be used with a void return type}} \
                               // expected-error {{expected expression}}
};
