// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fconstexpr-steps=1000 -fcontract-evaluation-semantic=observe -verify=expected,observe,ast,observe-ast %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fconstexpr-steps=1000 -fcontract-evaluation-semantic=enforce -verify=expected,enforce,ast %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fconstexpr-steps=1000 -fcontract-evaluation-semantic=observe -verify=expected,observe,bc,observe-bc %s -fexperimental-new-constant-interpreter
// RUN: %clang_cc1 -std=c++26 -fcontracts -fsyntax-only -fconstexpr-steps=1000 -fcontract-evaluation-semantic=enforce -verify=expected,enforce,bc %s -fexperimental-new-constant-interpreter

// A predicate that exhausts the step limit is not a constant expression. With
// the observe semantic, the evaluation continues after the contract violation,
// but the step limit still applies to the rest of the evaluation.

constexpr bool expensive() {
  unsigned long n = 0;
  for (int i = 0; i < 100000; ++i) // #expensive-loop
    n += i; // #expensive-step
  return n > 0;
}

constexpr long body() pre(expensive()) { // #body
  long n = 0;
  for (long i = 0; i < 100000; ++i) // #body-loop
    n += i;
  return n;
}

static_assert(body() > 0); // #use
// The two evaluators attribute the step limit to different statements.
// observe-warning@#body {{contract violation with the 'observe' evaluation semantic: precondition 'expensive()' is not a constant expression}}
// ast-note@#expensive-step {{constexpr evaluation hit maximum step limit of 1000; possible infinite loop?}}
// ast-note@#expensive-step {{use -fconstexpr-steps= to increase this limit (=0 will remove the limit)}}
// bc-note@#expensive-loop {{constexpr evaluation hit maximum step limit of 1000; possible infinite loop?}}
// bc-note@#expensive-loop {{use -fconstexpr-steps= to increase this limit (=0 will remove the limit)}}
// observe-note@#body {{in call to 'expensive()'}}
// observe-note@#use 2 {{in call to 'body()'}}
// observe-error@#use {{static assertion expression is not an integral constant expression}}
// observe-ast-note@#body {{constexpr evaluation hit maximum step limit of 1000; possible infinite loop?}}
// observe-ast-note@#body {{use -fconstexpr-steps= to increase this limit (=0 will remove the limit)}}
// observe-bc-note@#body-loop {{constexpr evaluation hit maximum step limit of 1000; possible infinite loop?}}
// observe-bc-note@#body-loop {{use -fconstexpr-steps= to increase this limit (=0 will remove the limit)}}
// enforce-error@#use {{contract violation in a constant expression}}
// enforce-note@#body {{contract violation with the 'enforce' evaluation semantic: precondition 'expensive()' is not a constant expression}}
// enforce-note@#body {{in call to 'expensive()'}}
// enforce-note@#use {{in call to 'body()'}}

// A predicate that stays within the limit doesn't consume the steps of the
// enclosing evaluation more than once.
constexpr bool cheap() {
  int n = 0;
  for (int i = 0; i < 10; ++i)
    n += i;
  return n > 0;
}
constexpr int fine() pre(cheap()) pre(cheap()) {
  int n = 0;
  for (int i = 0; i < 10; ++i)
    n += i;
  return n;
}
static_assert(fine() == 45);
