// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -verify %s

// [class.mem.general]p11.4.1:
//
//   member-declarator:
//     declarator virt-specifier-seq[opt]
//         function-contract-specifier-seq[opt] pure-specifier[opt]
//     declarator requires-clause function-contract-specifier-seq[opt]
//
// so a function-contract-specifier-seq is the last thing before an optional
// pure-specifier: neither a virt-specifier nor a requires-clause may follow
// it.  Both reversals used to be accepted silently.
//
// A requires-clause after the contracts is rejected only by the generic
// declarator grammar (there is no dedicated diagnostic for it yet).
//
// P2900R14 [dcl.contract.func]p6 additionally forbids contracts on virtual
// functions altogether, so every virt-specifier case below also gets that
// diagnostic; the ordering error must still be produced alongside it.

template <typename> concept C = true;

struct Base {
  virtual void f();
  virtual void g();
  virtual void h();
};

struct Derived : Base {
  void f() pre(true) override; // expected-error {{'override' must appear before function contract specifiers}} \
                               // expected-error {{virtual function 'f' cannot have function contract specifiers}}

  void g() pre(true) final; // expected-error {{'final' must appear before function contract specifiers}} \
                            // expected-error {{virtual function 'g' cannot have function contract specifiers}}

  // The GNU-attribute route reaches a second, separate virt-specifier parse,
  // so a fix applied to only the first one misses this.
  // FIXME: the ordering error is not produced on this route; the declaration
  // is still rejected because the function is virtual.
  void h() pre(true) __attribute__((unused)) override; // expected-error {{virtual function 'h' cannot have function contract specifiers}} \
                                                       // expected-warning {{GCC does not allow an attribute in this position on a function declaration}}
};

struct MemberRequires {
  template <class T>
  void p() pre(true) requires C<T>; // expected-error {{expected ';' at end of declaration list}}
};

template <typename T>
int free_wrong_order(T v) pre(v > T{}) requires C<T>; // expected-error {{expected function body after function declarator}}

// ---------------------------------------------------------------------------
// Controls.  The well-formed order must keep working, and a virt-specifier or
// requires-clause with NO contract in front of it must not be touched -- the
// discriminator is the declarator having taken a contract, and getting that
// wrong would reject every `void f() override;` in the language.
// ---------------------------------------------------------------------------

struct Ok : Base {
  void h() override;
};

struct OkRequires {
  template <class T>
  void p() requires C<T> pre(true);

  template <class T>
  void q() requires C<T>;
};

template <typename T>
int free_right_order(T v) requires C<T> pre(v > T{});
