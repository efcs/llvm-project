// The text of the predicate of a contract assertion (the value of
// std::contracts::contract_violation::comment()) is recorded in the AST when
// the contract assertion is parsed. It consists of the tokens between the
// parentheses of the contract assertion as they were written, before macro
// expansion, separated by a single space wherever there was whitespace
// between them.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -ast-dump %s | FileCheck %s --check-prefix=DUMP
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s --check-prefix=IR
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu -emit-llvm -o - %s | FileCheck %s --check-prefix=ONLY

#define GT(a, b) ((a) > (b))
#define ZERO 0
#define ID(x) x
#define EMPTY
#define COND x > 0
#define PRE(x) pre(x)
#define PRE_POSITIVE(x) pre((x) > 0)
#define PRE_POST(x) pre(x > ZERO) post(x > ZERO)
#define MY_ASSERT(c) contract_assert(c)
#define MY_ASSERT_NOT_ZERO(c) contract_assert((c) != ZERO)

// DUMP-LABEL: FunctionDecl {{.*}} simple
// DUMP: ContractStmt {{.*}} pre enforce "x > 0"
// DUMP-NEXT: BinaryOperator
int simple(int x) pre(x > 0);

// Whitespace and comments are normalized as if by the '#' operator.
// DUMP-LABEL: FunctionDecl {{.*}} whitespace
// DUMP: ContractStmt {{.*}} pre enforce "x > 0"
// DUMP: ContractStmt {{.*}} pre enforce "x>0"
// DUMP: ContractStmt {{.*}} pre enforce "x > ZERO && true"
// DUMP: ContractStmt {{.*}} pre enforce "x > 0"
int whitespace(int x)
  pre(   x   >   0   )
  pre(x>0)
  pre(x   >
        ZERO  /* comment */ &&  // line comment
      true)
  pre(x \
      > 0);

// The spelling of the tokens is preserved.
// DUMP-LABEL: FunctionDecl {{.*}} spelling
// DUMP: ContractStmt {{.*}} pre enforce "eq(s, \"a  b\") && *s != ' ' && eq(s, R\"(x\ny)\")"
// DUMP: ContractStmt {{.*}} pre enforce "not (s == nullptr) and 0x10 == 1'6"
bool eq(const char *, const char *);
int spelling(const char *s)
  pre(eq(s, "a  b") && *s != ' ' && eq(s, R"(x
y)"))
  pre(not (s == nullptr) and 0x10 == 1'6);

// The text includes the result-name-introducer.
// DUMP-LABEL: FunctionDecl {{.*}} result_name
// DUMP: ContractStmt {{.*}} post enforce "r : r > x"
// DUMP: ContractStmt {{.*}} post enforce "r: r > 0"
int result_name(const int x)
  post(r : r > x)
  post(r: r > 0);

// The attributes of the contract assertion are not part of the text.
// DUMP-LABEL: FunctionDecl {{.*}} attributes
// DUMP: ContractStmt {{.*}} pre {{.*}}"x > 0"
int attributes(int x) pre [[clang::contract_group("g")]] (x > 0);

// Macros are not expanded if the predicate is written at the place of the
// contract assertion.
// DUMP-LABEL: FunctionDecl {{.*}} macros_in_predicate
// DUMP: ContractStmt {{.*}} pre enforce "GT(x, 0)"
// DUMP: ContractStmt {{.*}} pre enforce "ID(x > 0)"
// DUMP: ContractStmt {{.*}} pre enforce "ID(x) > 0"
// DUMP: ContractStmt {{.*}} pre enforce "x > ID(ZERO)"
// DUMP: ContractStmt {{.*}} pre enforce "COND"
// DUMP: ContractStmt {{.*}} pre enforce "COND && true"
// DUMP: ContractStmt {{.*}} pre enforce "x > 0"
int macros_in_predicate(int x)
  pre(GT(x, 0))
  pre(ID(x > 0))
  pre(ID(x) > 0)
  pre(x > ID(ZERO))
  pre(COND)
  pre(COND && true)
  pre(EMPTY x > 0 EMPTY);

// If the predicate is an argument of a macro, the text is that of the
// argument.
// DUMP-LABEL: FunctionDecl {{.*}} macro_argument
// DUMP: ContractStmt {{.*}} pre enforce "x > ZERO"
// DUMP-LABEL: FunctionDecl {{.*}} macro_argument2
// DUMP: ContractStmt {{.*}} contract_assert enforce "x != ZERO"
// DUMP: ContractStmt {{.*}} contract_assert enforce "GT(x, 1)"
int macro_argument(int x) PRE(x > ZERO);
void macro_argument2(int x) {
  MY_ASSERT(x != ZERO);
  MY_ASSERT(GT(x, 1));
}

// If the predicate (or a part of it) is written in the replacement list of a
// macro, there is no text for it at the place of the contract assertion. The
// text of the macro invocation is used instead.
// DUMP-LABEL: FunctionDecl {{.*}} macro_body
// DUMP: ContractStmt {{.*}} pre enforce "PRE_POSITIVE(x)"
// DUMP-LABEL: FunctionDecl {{.*}} macro_body2
// DUMP: ContractStmt {{.*}} pre enforce "PRE_POST(x)"
// DUMP: ContractStmt {{.*}} post enforce "PRE_POST(x)"
// DUMP-LABEL: FunctionDecl {{.*}} macro_body3
// DUMP: ContractStmt {{.*}} contract_assert enforce "MY_ASSERT_NOT_ZERO(x)"
int macro_body(int x) PRE_POSITIVE(x);
int macro_body2(const int x) PRE_POST(x);
void macro_body3(int x) { MY_ASSERT_NOT_ZERO(x); }

// The text is preserved by template instantiation. A definition which omits
// the contracts refers to those of the first declaration.
// DUMP-LABEL: FunctionTemplateDecl {{.*}} templ
// DUMP: ContractStmt {{.*}} pre enforce "x > T{}"
// DUMP: ContractStmt {{.*}} post enforce "r : r == x"
// DUMP: ContractStmt {{.*}} contract_assert enforce "x != T{1}"
// DUMP: FunctionDecl {{.*}} templ 'int (const int)' implicit_instantiation
// DUMP: ContractStmt {{.*}} pre enforce "x > T{}"
// DUMP: ContractStmt {{.*}} post enforce "r : r == x"
// DUMP: ContractStmt {{.*}} contract_assert enforce "x != T{1}"
template <class T> T templ(const T x) pre(x > T{}) post(r : r == x) {
  contract_assert(x != T{1});
  return x;
}
int use_templ = templ(2);

// DUMP-LABEL: CXXMethodDecl {{.*}} redecl 'int (int)'
// DUMP-NEXT: ParmVarDecl
// DUMP-NEXT: ContractSpecifierDecl [[CSD:0x[0-9a-f]+]]
// DUMP-NEXT: ContractStmt {{.*}} pre enforce "x > m"
// DUMP: ContractStmt {{.*}} post enforce "r : r > m"
// DUMP: CXXMethodDecl {{.*}} prev {{.*}} redecl 'int (int)' contracts [[CSD]]
// DUMP-NEXT: ParmVarDecl
// DUMP-NEXT: CompoundStmt
struct S {
  int m;
  int redecl(int x) pre(x > m) post(r : r > m);
};
int S::redecl(int y) { return y; }

// DUMP-LABEL: VarDecl {{.*}} lambda
// DUMP: ContractStmt {{.*}} pre enforce "x > 0"
// DUMP: ContractStmt {{.*}} post enforce "r : r > 0"
auto lambda = [](int x) pre(x > 0) post(r : r > 0) { return x; };

// '#line' changes the presumed location of the contract assertion, but not
// the text.
#line 1000 "foo.h"
// DUMP-LABEL: FunctionDecl {{.*}} line_directive
// DUMP: ContractStmt {{.*}} pre enforce "x > 0 && \"line\""
int line_directive(int x) pre(x > 0 && "line");

// The [[clang::contract_message]] attribute replaces, or is appended to, the
// text of the predicate.
int emit(const int x)
  pre(GT(x,
         0))
  post(r   :   r   >   ZERO)
  pre [[clang::contract_message("custom")]] (x   >   2)
  pre [[clang::contract_message("only", false)]] (x > 3) {
  MY_ASSERT_NOT_ZERO(x);
  return x;
}
// IR-DAG: c"GT(x, 0)\00"
// IR-DAG: c"r : r > ZERO\00"
// IR-DAG: c"x > 2\00"
// IR-DAG: c"custom\00"
// IR-DAG: c"only\00"
// IR-DAG: c"MY_ASSERT_NOT_ZERO(x)\00"
// IR-DAG: c"foo.h\00"
// IR-NOT: x   >   2

// An assertion without a message has a null message pointer in its contract
// data, and the predicate text is omitted (an empty comment) from the contract
// data of an assertion whose message replaces it.
// ONLY: @__contract_data = {{.*}}, ptr @.str.contract.{{[0-9]+}}, ptr null }
// ONLY: @.str.contract.[[EMPTY:[0-9]+]] = private unnamed_addr constant [1 x i8] zeroinitializer
// ONLY-NOT: c"x > 3\00"
// ONLY: @.str.contract.[[ONLY:[0-9]+]] = private unnamed_addr constant [5 x i8] c"only\00"
// ONLY-NEXT: @__contract_data.{{[0-9]+}} = {{.*}}, ptr @.str.contract.[[EMPTY]], ptr @.str.contract.[[ONLY]] }
