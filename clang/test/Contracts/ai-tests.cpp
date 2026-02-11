
// RUN:  %clang_cc1 -std=c++2b  -verify -fcontracts -fsyntax-only %s || %clang_cc1 -std=c++2b -fcontracts -fcolor-diagnostics -fsyntax-only %s ||  %clang_cc1 -std=c++2b  -verify -fcontracts -fcolor-diagnostics -fsyntax-only %s


// Test cases for contract assertions and lambda captures

// Test 1: Postcondition with non-const parameter (should fail)
int test_non_const(int i) // expected-note {{declared here}}
    post(r: r == i) // expected-error {{parameter 'i' referenced in contract postcondition must be declared const}}
{
    return i;
}

// Test 2: Postcondition with const parameter (should pass)
int test_const(const int i)
    post(r: r == i) // OK
{
    return i;
}

// Test 3: Postcondition with array parameter (should fail)
int test_array(const int arr[]) // expected-note {{parameter of type 'const int[]' is declared here}}
    post(r: r == arr[0]) // expected-error {{parameter 'arr' referenced in contract postcondition cannot have an array type}}
{
    return arr[0];
}

// Test 4: Postcondition with function parameter (should fail)
int test_function(int (*func)()) // expected-note {{parameter of type 'int (*)()' is declared here}}
    post(r: r == func()) // expected-error {{parameter 'func' referenced in contract postcondition cannot have a function type}}
{
    return func();
}

// Test 5: Lambda with implicit capture in precondition (should fail)
void test_lambda_implicit_capture() {
    int i = 1;
    auto f = [=] // expected-error  {{implicit capture of local entity 'i' is not allowed when used exclusively in contract assertions}}
        pre( // expected-note {{within contract context introduced here}}
            i > 0 // expected-note {{capture of local entity 'i' is required here}}
        )
      { };
}

// Test 6: Lambda with explicit capture in precondition (should pass)
void test_lambda_explicit_capture() {
    int i = 1;
    auto f = [i](auto z) -> int pre(i > 0) post(i > 0)  { return 1; }; // OK
}

// Test 7: Lambda with implicit capture in contract_assert (should fail)
void test_lambda_implicit_capture_assert() {
    int i = 1;
    auto f = [=] { // expected-error {{implicit capture of local entity 'i'}}
        contract_assert(  // expected-note {{within contract context introduced here}}
          i > 0 // expected-note {{capture of local entity 'i'}}
          );
    };
}

// Test 8: Lambda with implicit capture used both in and out of contract assertion (should pass)
void test_lambda_implicit_capture_mixed() {
    int i = 1;
    auto f = [=] {
        contract_assert(i > 0); // OK
        return i;
    };
}

// Test 9: Nested lambda with implicit capture in precondition (should pass)
void test_nested_lambda_implicit_capture() {
    auto f = [=] pre([]{
        bool x = true;
        return [=]{ return x; }(); // OK, x is captured implicitly in nested lambda
    }()) {};
}

int main() {
    // The main function is empty as these tests are meant to check compile-time errors
    return 0;
}
