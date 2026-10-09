// Lambda-expressions in the function contract specifiers of the functions of
// a module interface unit, in a two-phase build: the module file produced by
// --precompile is compiled itself. The predicates refer to the call operators
// of the closure types before the closure types' own declarations are read,
// so the reader first fakes up a definition for each closure type and then
// merges the real one into it (ASTDeclMerger::MergeDefinitionData; this used
// to assert, see also Modules/pr217858.cppm).
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: split-file %s %t
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   %t/M.cppm -emit-module-interface -o %t/M.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   %t/M.pcm -emit-llvm -o - | FileCheck %t/M.cppm
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fmodule-file=M=%t/M.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp
//
// The same with a reduced BMI.
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   %t/M.cppm -emit-reduced-module-interface -o %t/M.reduced.pcm
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -fmodule-file=M=%t/M.reduced.pcm %t/Use.cpp -emit-llvm -o - \
// RUN:   | FileCheck %t/Use.cpp

//--- M.cppm
export module M;
export inline int pre_lambda(const int x) pre([&] { return x > 0; }()) {
  return x;
}
export inline int post_lambda(const int x) post(r : [r] { return r > 0; }()) {
  return x;
}
export inline int assert_lambda(const int x) {
  contract_assert([=] { return x > 0; }());
  return x;
}
export template <class T> T tmpl_lambda(const T v) post(r : [&] { return r == v; }()) {
  return v;
}
export int non_inline(const int x) pre([&] { return x > 0; }()) { return x; }
export inline int nested(const int x)
    pre([&] { return [&] { return x > 0; }(); }()) {
  return x;
}
export inline int generic(const int x) pre([](auto v) { return v > 0; }(x)) {
  return x;
}
export int use_all(int v) {
  return pre_lambda(v) + post_lambda(v) + assert_lambda(v) + tmpl_lambda(v) +
         non_inline(v) + nested(v) + generic(v);
}

// CHECK-LABEL: define {{.*}} @_ZW1M10non_inlinei(
// CHECK: call {{.*}} @_ZZW1M10non_inlineiENKUlvE_clEv(
// CHECK: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M7use_alli(
// CHECK-LABEL: define {{.*}} @_ZW1M10pre_lambdai(
// CHECK: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M11post_lambdai(
// CHECK: call void @__cxa_contract_violation_post_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M13assert_lambdai(
// CHECK: call void @__cxa_contract_violation_assert_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M11tmpl_lambdaIiET_S1_(
// CHECK: call void @__cxa_contract_violation_post_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M6nestedi(
// CHECK: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M7generici(
// CHECK: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(

//--- Use.cpp
import M;
int use(int v) {
  return pre_lambda(v) + post_lambda(v) + assert_lambda(v) + tmpl_lambda(v) +
         non_inline(v) + nested(v) + generic(v) + use_all(v);
}
// CHECK-LABEL: define {{.*}} @_ZW1M10pre_lambdai(
// CHECK: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M11post_lambdai(
// CHECK: call void @__cxa_contract_violation_post_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M13assert_lambdai(
// CHECK: call void @__cxa_contract_violation_assert_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M11tmpl_lambdaIiET_S1_(
// CHECK: call void @__cxa_contract_violation_post_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M6nestedi(
// CHECK: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(
// CHECK-LABEL: define {{.*}} @_ZW1M7generici(
// CHECK: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(
