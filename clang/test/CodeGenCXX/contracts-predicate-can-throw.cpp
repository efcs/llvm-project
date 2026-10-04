// A predicate that may exit via an exception must be wrapped so that the
// exception is reported to the violation handler as evaluation_exception
// (detection_mode 2) instead of escaping. Predicates that provably cannot
// throw are not wrapped.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -fcxx-exceptions -fexceptions \
// RUN:   -fcontract-evaluation-semantic=observe -triple x86_64-linux-gnu \
// RUN:   -Wno-unused-value -emit-llvm -o - %s | FileCheck %s

namespace std {
class type_info {
public:
  bool operator==(const type_info &) const noexcept;
};
} // namespace std

struct Thrower {
  Thrower();
  bool ok() const noexcept;
};
struct NoThrow {
  NoThrow() noexcept;
  bool ok() const noexcept;
};
struct Poly {
  virtual ~Poly();
};
struct Der : Poly {
  bool ok() const noexcept;
};

// CHECK-LABEL: define {{.*}} @_Z4ctorv()
// CHECK: landingpad
// CHECK: {{call|invoke}} void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 2,
void ctor() pre(Thrower().ok()) {}

// CHECK-LABEL: define {{.*}} @_Z12nothrow_ctorv()
// CHECK-NOT: landingpad
// CHECK: ret void
void nothrow_ctor() pre(NoThrow().ok()) {}

// CHECK-LABEL: define {{.*}} @_Z8new_exprv()
// CHECK: landingpad
// CHECK: {{call|invoke}} void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 2,
void new_expr() pre(new int) {}

// CHECK-LABEL: define {{.*}} @_Z11dyncast_refR4Poly(
// CHECK: landingpad
// CHECK: {{call|invoke}} void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 2,
void dyncast_ref(Poly &p) pre(dynamic_cast<const Der &>(p).ok()) {}

// CHECK-LABEL: define {{.*}} @_Z11dyncast_ptrP4Poly(
// CHECK-NOT: landingpad
// CHECK: ret void
void dyncast_ptr(Poly *p) pre(dynamic_cast<const Der *>(p)->ok()) {}

// CHECK-LABEL: define {{.*}} @_Z10typeid_ptrP4Poly(
// CHECK: landingpad
// CHECK: {{call|invoke}} void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 2,
void typeid_ptr(Poly *p) pre(typeid(*p) == typeid(Der)) {}

// CHECK-LABEL: define {{.*}} @_Z10typeid_refR4Poly(
// CHECK-NOT: landingpad
// CHECK: ret void
void typeid_ref(Poly &p) pre(typeid(p) == typeid(Der)) {}

bool default_arg(Thrower = Thrower()) noexcept;
// CHECK-LABEL: define {{.*}} @_Z16default_arg_testv()
// CHECK: landingpad
// CHECK: {{call|invoke}} void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 2,
void default_arg_test() pre(default_arg()) {}

// CHECK-LABEL: define {{.*}} @_Z13fnptr_nothrowPDoFbvE(
// CHECK-NOT: landingpad
// CHECK: ret void
void fnptr_nothrow(bool (*fp)() noexcept) pre(fp()) {}

// CHECK-LABEL: define {{.*}} @_Z5fnptrPFbvE(
// CHECK: landingpad
// CHECK: {{call|invoke}} void @__handle_contract_violation_v3(i8 noundef zeroext 2, i8 noundef zeroext 2,
void fnptr(bool (*fp)()) pre(fp()) {}

// Neither the body of a lambda nor unevaluated operands are evaluated.
// CHECK-LABEL: define {{.*}} @_Z6lambdav()
// CHECK-NOT: landingpad
// CHECK: ret void
void lambda() pre(([] { return Thrower().ok(); }, true)) {}

// CHECK-LABEL: define {{.*}} @_Z11unevaluatedv()
// CHECK-NOT: landingpad
// CHECK: ret void
void unevaluated() pre(sizeof(Thrower{}) == 1 && !noexcept(Thrower())) {}

