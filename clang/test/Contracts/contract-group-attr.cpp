// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts -fcontract-group-evaluation-semantic=std.foo=ignore,std.foo.baz=enforce,std.bar=enforce




void test_attribute(int x) pre [[clang::contract_group("foo")]] (x != 0) {
  contract_assert [[clang::contract_group("foo")]] (x != 0); // OK

  contract_assert [[clang::contract_group("")]] (true);  // expected-error {{clang::contract_group group cannot be empty}}
  contract_assert [[clang::contract_group("-bar")]] (true);
  contract_assert [[clang::contract_group("foo*bar")]] (true); // expected-error {{clang::contract_group group "foo*bar" cannot contain '*'}}
  contract_assert [[clang::contract_group("foo-bar")]] (true);

  contract_assert [[clang::contract_group("f")]] // expected-note {{previous attribute is here}}
                  [[clang::contract_group("f")]] (x != 0);  // expected-error {{attribute 'clang::contract_group' cannot appear more than once on a contract specifier}}
}

// The attribute-specifier-seq of a late-parsed member function contract.
struct S {
  void f() pre [[clang::contract_group("foo")]] (m > 0)
           post [[clang::contract_group("foo")]] [[]] (m > 0);
  void g() pre [[clang::contract_group("")]] (m > 0); // expected-error {{clang::contract_group group cannot be empty}}
  int m;
};
void free_function() pre [[clang::contract_group("foo")]] (true);
