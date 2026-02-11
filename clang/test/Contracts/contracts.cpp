// RUN: %clang_cc1 -std=c++26 -fsyntax-only -verify=expected -verify-ignore-unexpected=warning %s -fcontracts


void test_pre_parse(int x) pre(x != 0);
void test_post_parse(const int x) post(x != 0);
int test_dup_names(int& x) // expected-note {{previous declaration is here}}
  post(x :  // expected-error {{declaration of result name 'x' shadows parameter}}
    x != 0);


auto test_trailing_return() -> int post(r : r != 0);

int test_can_redecl_result_name()
  post(r : r != 0) // OK
  post(r : r != 0) // OK
  post(r != 0); // expected-error {{use of undeclared identifier 'r'}}
struct A {
  int xx;

  int test_member(const int x) // expected-note {{previous declaration is here}}
    pre(x != 0)
    post(r : r != 0)
    post(r : r != 1)
    post(x : x != 0); // expected-error {{declaration of result name 'x' shadows parameter}}
  void test_this_access() post(r != 0);

  int r;
};

int test_return_parse(const int x) post(r : r == x) {
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

namespace TestConv {
template<class T>
void foo()
pre(T{}) { // expected-error {{'NoBool' is not contextually convertible to 'bool'}}
  contract_assert(T{}); // expected-error {{'NoBool' is not contextually convertible to 'bool'}}
}

template void foo<int>();
template void foo<ImpBC>();
template void foo<NoBool>(); // expected-note {{requested here}}
} // end namespace TestConv

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

namespace result_name_scope_test {

int test_scope(const int x) post(r : r != x) {
  return r; // expected-error {{use of undeclared identifier 'r'}}
}

struct T {
  int r;

  int test_scope(const int x) post(y : y != x) {
    return r + y; // expected-error {{use of undeclared identifier 'y'}}
  }

  int z = y; // expected-error {{use of undeclared identifier 'y'}}
};
}
int test_result_name_scope() post(r : r != 0) {
  ((void)r); // expected-error {{use of undeclared identifier 'r'}}
  return 42;
}
