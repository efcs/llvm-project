// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts


void test_pre_parse(int x) pre(x != 0) {

}

void test_post_parse(int x) post(x != 0) {
}

int test_return_parse(int x) post(r : r == x) {
  return x;
}

struct ImpBC {
  operator bool() const;
};

struct ExpBC {
  explicit operator bool() const;
};

struct ImpBC2 {
  operator int() const;
};

struct NoBool {
};

void test_attributes(int x) {
  contract_assert [[clang::contract_group("bar")]] (x != 0);
}

template <class T>
void foo() pre(T{}) {
  contract_assert(T{}); // expected-error {{'NoBool' is not contextually convertible to 'bool'}}
}

template void foo<int>();
template void foo<ImpBC>();
template void foo<NoBool>(); // expected-note {{requested here}}

void test_converted_to_bool(int x)
  pre((void)true) // expected-error {{value of type 'void' is not contextually convertible to 'bool'}}
  post((void)true) // expected-error {{value of type 'void' is not contextually convertible to 'bool'}}
 {
  contract_assert(x != 0);
  contract_assert((void)true); // expected-error {{value of type 'void' is not contextually convertible to 'bool'}}
  contract_assert(ExpBC{});
  contract_assert(ImpBC{});
  contract_assert(ImpBC2{});

}
