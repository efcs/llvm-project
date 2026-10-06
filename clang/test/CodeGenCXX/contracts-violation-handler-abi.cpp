// The code emitted for a violated contract assertion follows the Itanium C++
// Contracts ABI: a static data block describing the assertion (a descriptor
// table pointer, a null 'next' pointer, the source location, the predicate
// text and the diagnostic message) is passed to a runtime entry point whose
// name encodes the assertion kind, the evaluation semantic and the detection
// mode. One descriptor table describes all the data blocks of a translation
// unit.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,X64,ALIAS,OBSERVE,OBSERVE-X64
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,X64,ALIAS,ENFORCE,ENFORCE-X64
// RUN: %clang_cc1 -std=c++26 -fcontracts \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,X64,ALIAS,NOEXC
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple i386-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,X86,ALIAS,OBSERVE
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple aarch64-apple-macosx14 \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,X64,ENFORCE,DARWIN
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-windows-msvc \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,X64,ALIAS,OBSERVE,MSVC
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple aarch64_be-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefixes=CHECK,X64,ALIAS,ENFORCE
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=quick_enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=QUICK
//
// The declarations of the entry points and their attributes, checked in the
// order the entry points are first used.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=DECL-OBSERVE
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=DECL-ENFORCE
// RUN: %clang_cc1 -std=c++26 -fcontracts \
// RUN:   -fcontract-evaluation-semantic=enforce -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=DECL-NOEXC
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=ignore -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=IGNORE

// The descriptor table: version 1 and vendor Clang (0x12), 3 entries with the
// field IDs of the source location (1), the comment (2) and the message (3),
// padding to the alignment of the entries, and the byte offsets of the fields
// in the data block.
// X64: @__contract_descriptor_table = private unnamed_addr constant <{ i8, i8, i8, i8, i8, i8, i8, i8, i64, i64, i64 }> <{ i8 18, i8 3, i8 1, i8 2, i8 3, i8 0, i8 0, i8 0, i64 16, i64 40, i64 48 }>, align 8
// X86: @__contract_descriptor_table = private unnamed_addr constant <{ i8, i8, i8, i8, i8, i8, i8, i8, i32, i32, i32 }> <{ i8 18, i8 3, i8 1, i8 2, i8 3, i8 0, i8 0, i8 0, i32 8, i32 24, i32 28 }>, align 4

// The data block of f's precondition: the descriptor, no next block, the
// source location (file, function as __PRETTY_FUNCTION__ names it, line and
// column of the contract keyword), the comment and the (empty) message.
// CHECK: @.str.contract = private unnamed_addr constant [{{[0-9]+}} x i8] c"{{.*}}contracts-violation-handler-abi.cpp\00"
// CHECK: @.str.contract.1 = private unnamed_addr constant [12 x i8] c"void f(int)\00"
// CHECK: @.str.contract.2 = private unnamed_addr constant [5 x i8] c"p(x)\00"
// CHECK: @.str.contract.3 = private unnamed_addr constant [1 x i8] zeroinitializer
// X64: @__contract_data = private unnamed_addr constant { ptr, ptr, { ptr, ptr, i32, i32 }, ptr, ptr } { ptr @__contract_descriptor_table, ptr null, { ptr, ptr, i32, i32 } { ptr @.str.contract, ptr @.str.contract.1, i32 [[@LINE+18]], i32 15 }, ptr @.str.contract.2, ptr @.str.contract.3 }, align 8
// X86: @__contract_data = private unnamed_addr constant { ptr, ptr, { ptr, ptr, i32, i32 }, ptr, ptr } { ptr @__contract_descriptor_table, ptr null, { ptr, ptr, i32, i32 } { ptr @.str.contract, ptr @.str.contract.1, i32 [[@LINE+17]], i32 15 }, ptr @.str.contract.2, ptr @.str.contract.3 }, align 4
//
// The data blocks of g (below): the enclosing function is named in each, and
// the assertion in the body comes before the postcondition.
// CHECK: @.str.contract.{{[0-9]+}} = private unnamed_addr constant [{{[0-9]+}} x i8] c"void g(const int)\00"
// CHECK: @.str.contract.{{[0-9]+}} = private unnamed_addr constant [{{[0-9]+}} x i8] c"x > 0\00"
// CHECK: @.str.contract.{{[0-9]+}} = private unnamed_addr constant [{{[0-9]+}} x i8] c"np(x)\00"
//
// A definition of the contract-violation handler (below) gets a C-linkage
// alias through which the runtime calls it.
// ALIAS: @__handle_contract_violation = {{.*}}alias void (ptr), ptr @{{_Z25handle_contract_violationRKNSt9contracts18contract_violationE|"\?handle_contract_violation@@YAXAEBVcontract_violation@contracts@std@@@Z"}}
// On Mach-O, which has no aliases, a forwarding function is emitted instead
// (see below), kept from being dead-stripped.
// DARWIN: @llvm.used = appending global [1 x ptr] [ptr @__handle_contract_violation]

bool p(int);
bool np(int) noexcept;
void f(int x) pre(p(x)) {}
// CHECK-LABEL: define {{.*}} @{{_Z1fi|"\?f@@YAXH@Z"}}(

// The predicate may throw, so it is evaluated under a catch-all handler, and a
// violation by exception is reported from the handler with the '_ex' entry
// point, after the exception has been caught.
// OBSERVE: invoke {{.*}} @{{_Z1pi|"\?p@@YA_NH@Z"}}(
// MSVC: catchpad within
// MSVC-NEXT: call void @__cxa_contract_violation_pre_observe_ex(ptr @__contract_data) [ "funclet"(token %{{.*}}) ]
// MSVC-NEXT: catchret
// OBSERVE: br i1 %{{.*}}, label %contract.end, label %contract.violation
// OBSERVE-X64: landingpad
// OBSERVE-X64: call ptr @__cxa_begin_catch(
// OBSERVE-X64-NEXT: invoke void @__cxa_contract_violation_pre_observe_ex(ptr @__contract_data)
// OBSERVE-X64: call void @__cxa_end_catch()
// OBSERVE: contract.violation:
// OBSERVE-NEXT: call void @__cxa_contract_violation_pre_observe_pf(ptr @__contract_data)
// OBSERVE-NEXT: br label %contract.end
// OBSERVE: contract.end:

// The 'enforce' entry points do not return. Inside the handler, the call must
// unwind through the end of the catch; the plain violation needs no landing
// pad in this function.
// ENFORCE-X64: call ptr @__cxa_begin_catch(
// ENFORCE-X64-NEXT: invoke void @__cxa_contract_violation_pre_enforce_ex(ptr @__contract_data) #[[NORETURN:[0-9]+]]
// ENFORCE-X64-NEXT: to label %[[UNREACHABLE:.*]] unwind
// ENFORCE: contract.violation:
// ENFORCE-NEXT: call void @__cxa_contract_violation_pre_enforce_pf(ptr @__contract_data) #[[NORETURN:[0-9]+]]
// ENFORCE-NEXT: unreachable

// Without exceptions, there is no handler for a throwing predicate, and the
// '_noexcept' entry points are used, which do not unwind.
// NOEXC-NOT: landingpad
// NOEXC: contract.violation:
// NOEXC-NEXT: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr @__contract_data) #[[NORETURN:[0-9]+]]
// NOEXC-NEXT: unreachable

// quick_enforce traps, with one trap block per function shared by all of its
// assertions; ignore evaluates nothing.
// QUICK-LABEL: define {{.*}} @_Z1fi(
// QUICK-NOT: __cxa_contract_violation
// QUICK: br i1 %{{.*}}, label %contract.end, label %contract.violation.trap.handler
// QUICK: contract.violation.trap.handler:
// QUICK-NEXT: call void @llvm.trap()
// QUICK-NEXT: unreachable
// IGNORE-LABEL: define {{.*}} @_Z1fi(
// IGNORE-NOT: @_Z1pi
// IGNORE-NOT: __cxa_contract_violation
// IGNORE: ret void

// The kind of the assertion selects the entry point.
void g(const int x) post(np(x)) { contract_assert(x > 0); }
// CHECK-LABEL: define {{.*}} @{{_Z1gi|"\?g@@YAXH@Z"}}(
// OBSERVE: call void @__cxa_contract_violation_assert_observe_pf(ptr @__contract_data.{{[0-9]+}})
// OBSERVE: call void @__cxa_contract_violation_post_observe_pf(ptr @__contract_data.{{[0-9]+}})
// ENFORCE: @__cxa_contract_violation_assert_enforce_pf(ptr @__contract_data.{{[0-9]+}})
// ENFORCE: @__cxa_contract_violation_post_enforce_pf(ptr @__contract_data.{{[0-9]+}})
// QUICK-LABEL: define {{.*}} @_Z1gi(
// QUICK: br i1 %{{.*}}, label %contract.end, label %contract.violation.trap.handler
// QUICK: br i1 %{{.*}}, label %contract.end{{[0-9]*}}, label %contract.violation.trap.handler

// In a noexcept function an exception from the handler would terminate, so the
// '_noexcept' entry points are used and the calls do not unwind. The catch-all
// handler for the predicate is still needed.
void h(int x) noexcept pre(p(x)) {}
// CHECK-LABEL: define {{.*}} @{{_Z1hi|"\?h@@YAXH@Z"}}(
// OBSERVE-X64: landingpad
// OBSERVE-X64: call ptr @__cxa_begin_catch(
// OBSERVE-X64-NEXT: call void @__cxa_contract_violation_pre_observe_ex_noexcept(ptr @__contract_data.{{[0-9]+}})
// OBSERVE-X64-NEXT: {{call|invoke}} void @__cxa_end_catch()
// OBSERVE: contract.violation:
// OBSERVE-NEXT: call void @__cxa_contract_violation_pre_observe_pf_noexcept(ptr @__contract_data.{{[0-9]+}})
// ENFORCE-X64: call ptr @__cxa_begin_catch(
// ENFORCE-X64-NEXT: call void @__cxa_contract_violation_pre_enforce_ex_noexcept(ptr @__contract_data.{{[0-9]+}}) #[[NORETURN_NOUNWIND:[0-9]+]]
// ENFORCE-X64-NEXT: unreachable
// ENFORCE: contract.violation:
// ENFORCE-NEXT: call void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr @__contract_data.{{[0-9]+}}) #{{[0-9]+}}
// ENFORCE-NEXT: unreachable

// A try block inside a noexcept function catches what the handler throws, so
// the throwing entry points are used there.
#ifdef __cpp_exceptions
void t(int x) noexcept {
  try {
    contract_assert(x > 0);
  } catch (...) {
  }
}
#endif
// OBSERVE-LABEL: define {{.*}} @{{_Z1ti|"\?t@@YAXH@Z"}}(
// OBSERVE: invoke void @__cxa_contract_violation_assert_observe_pf(ptr @__contract_data.{{[0-9]+}})
// ENFORCE-LABEL: define {{.*}} @{{_Z1ti|"\?t@@YAXH@Z"}}(
// ENFORCE: invoke void @__cxa_contract_violation_assert_enforce_pf(ptr @__contract_data.{{[0-9]+}}) #[[NORETURN]]

// On Mach-O, which has no aliases, the handler gets a forwarding function
// instead of an alias (see the alias checks above).
namespace std::contracts {
class contract_violation;
}
void handle_contract_violation(const std::contracts::contract_violation&) {}
// X64-LABEL: define {{.*}} @{{_Z25handle_contract_violationRKNSt9contracts18contract_violationE|"\?handle_contract_violation@@YAXAEBVcontract_violation@contracts@std@@@Z"}}(
// X86-LABEL: define {{.*}} @_Z25handle_contract_violationRKNSt9contracts18contract_violationE(
// DARWIN: define void @__handle_contract_violation(ptr noundef nonnull align 1 %[[V:.*]])
// DARWIN-NEXT: entry:
// DARWIN-NEXT: tail call void @_Z25handle_contract_violationRKNSt9contracts18contract_violationE(ptr %[[V]])
// DARWIN-NEXT: ret void

// Declarations of the entry points: the enforce variants do not return, the
// _noexcept variants do not unwind, and all are cold.
// DECL-OBSERVE: declare void @__cxa_contract_violation_pre_observe_ex(ptr noundef) #[[EP:[0-9]+]]
// DECL-OBSERVE: declare void @__cxa_contract_violation_pre_observe_pf(ptr noundef) #[[EP]]
// DECL-OBSERVE: declare void @__cxa_contract_violation_assert_observe_pf(ptr noundef) #[[EP]]
// DECL-OBSERVE: declare void @__cxa_contract_violation_post_observe_pf(ptr noundef) #[[EP]]
// DECL-OBSERVE: declare void @__cxa_contract_violation_pre_observe_ex_noexcept(ptr noundef) #[[EPNX:[0-9]+]]
// DECL-OBSERVE: declare void @__cxa_contract_violation_pre_observe_pf_noexcept(ptr noundef) #[[EPNX]]
// DECL-OBSERVE: attributes #[[EP]] = { cold
// DECL-OBSERVE: attributes #[[EPNX]] = { cold nounwind
// DECL-ENFORCE: declare void @__cxa_contract_violation_pre_enforce_ex(ptr noundef) #[[EP:[0-9]+]]
// DECL-ENFORCE: declare void @__cxa_contract_violation_pre_enforce_pf(ptr noundef) #[[EP]]
// DECL-ENFORCE: declare void @__cxa_contract_violation_assert_enforce_pf(ptr noundef) #[[EP]]
// DECL-ENFORCE: declare void @__cxa_contract_violation_post_enforce_pf(ptr noundef) #[[EP]]
// DECL-ENFORCE: declare void @__cxa_contract_violation_pre_enforce_ex_noexcept(ptr noundef) #[[EPNX:[0-9]+]]
// DECL-ENFORCE: declare void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr noundef) #[[EPNX]]
// DECL-ENFORCE: attributes #[[EP]] = { cold noreturn
// DECL-ENFORCE: attributes #[[EPNX]] = { cold noreturn nounwind
// DECL-NOEXC: declare void @__cxa_contract_violation_pre_enforce_pf_noexcept(ptr noundef) #[[EPNX:[0-9]+]]
// DECL-NOEXC: attributes #[[EPNX]] = { cold noreturn nounwind

// ENFORCE-X64: attributes #[[NORETURN]] = { noreturn }
// ENFORCE-X64: attributes #[[NORETURN_NOUNWIND]] = { noreturn nounwind }
// NOEXC: attributes #[[NORETURN]] = { noreturn nounwind }

// The alias is visible outside of the module regardless of the handler's
// visibility, since only the runtime library references it.
// RUN: %clang_cc1 -std=c++26 -fcontracts -fvisibility=hidden \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=HIDDEN
// RUN: %clang_cc1 -std=c++26 -fcontracts -fvisibility=hidden \
// RUN:   -fcontract-evaluation-semantic=observe -triple aarch64-apple-macosx14 \
// RUN:   -emit-llvm -o - %s | FileCheck %s --check-prefix=HIDDEN-DARWIN
// HIDDEN: @__handle_contract_violation = dso_local alias void (ptr), ptr @_Z25handle_contract_violationRKNSt9contracts18contract_violationE
// HIDDEN: define hidden void @_Z25handle_contract_violationRKNSt9contracts18contract_violationE(
// HIDDEN-DARWIN: define hidden void @_Z25handle_contract_violationRKNSt9contracts18contract_violationE(
// HIDDEN-DARWIN: define {{(dso_local )?}}void @__handle_contract_violation(
