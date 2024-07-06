// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts \
//    -fclang-contract-groups=-std.foo,std.foo.baz,std.bar




void test_attribute(int x) pre [[clang::contract_group("foo")]] (x != 0) {
  contract_assert [[clang::contract_group("foo")]] (x != 0); // OK

  contract_assert [[clang::contract_group("")]] (true); // expected-error {{clang::contract_group attribute argument "" cannot be empty}}
  contract_assert [[clang::contract_group("-bar")]] (true); // expected-error {{clang::contract_group attribute argument "-bar" cannot contain '-'}}
  contract_assert [[clang::contract_group("foo*bar")]] (true);
  contract_assert [[clang::contract_group("foo-bar")]] (true); // OK

  contract_assert [[clang::contract_group("f")]] // expected-note {{previous attribute is here}}
                  [[clang::contract_group("f")]] (x != 0); // expected-error {{clang::contract_group appears more than once}}
}

