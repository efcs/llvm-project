// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected %s -fcontracts


void test_pre_parse(int x) pre(x != 0) {

}

void test_post_parse(int x) post(x != 0) {
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

void test_attributes(int x) {
  [[foo::dummy]] contract_assert
    [[clang::annotate("bar")]] (x != 0); // FIXME(EricWF): diagnose this

}

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
