// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts -fcontract-group-evaluation-semantic=std.foo=ignore,std.foo.baz=enforce,std.bar=enforce




void test_attribute(int x) pre [[clang::contract_group("foo")]] (x != 0) {
  contract_assert [[clang::contract_group("foo")]] (x != 0); // OK

  contract_assert [[clang::contract_group("")]] (true);  // expected-error {{clang::contract_group group cannot be empty}}
  contract_assert [[clang::contract_group("-bar")]] (true);
  contract_assert [[clang::contract_group("foo*bar")]] (true); // expected-error {{clang::contract_group group "foo*bar" cannot contain '*'}}
  contract_assert [[clang::contract_group("foo-bar")]] (true);

  contract_assert [[clang::contract_group("f")]] // expected-note {{previous attribute is here}}
                  [[clang::contract_group("f")]] (x != 0);  // expected-error {{attribute 'contract_group' cannot appear more than once on a contract specifier}}
}
