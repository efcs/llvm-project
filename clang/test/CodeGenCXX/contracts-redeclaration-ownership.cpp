// The function contract assertions of a function are those of its first
// declaration; a definition which omits them evaluates them with its own
// parameters, and the closure types of the lambda-expressions within the
// predicates are those of the first declaration. The code and the mangled
// names are the same whether or not the definition repeats the contracts.
//
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -std=c++26 -fcontracts -triple x86_64-linux-gnu \
// RUN:   -DREPEAT -emit-llvm -o - %s | FileCheck %s

#ifdef REPEAT
#define CONTRACTS(...) __VA_ARGS__
#else
#define CONTRACTS(...)
#endif

// The definition renames the parameters and the result binding; the
// predicates read the definition's parameters and its return slot.
// CHECK-LABEL: define {{.*}} i32 @_Z7renamedii(i32 noundef %y, i32 noundef %z)
// CHECK: %[[RETVAL:retval]] = alloca i32
// CHECK: %[[Y:y.addr]] = alloca i32
// CHECK: %[[Z:z.addr]] = alloca i32
// CHECK: store i32 %y, ptr %[[Y]]
// CHECK: store i32 %z, ptr %[[Z]]
// CHECK: %[[YV:.*]] = load i32, ptr %[[Y]]
// CHECK: %[[ZV:.*]] = load i32, ptr %[[Z]]
// CHECK: icmp sgt i32 %[[YV]], %[[ZV]]
// CHECK: contract.end:
// CHECK: %[[R:.*]] = load i32, ptr %[[RETVAL]]
// CHECK: %[[YV2:.*]] = load i32, ptr %[[Y]]
// CHECK: icmp sgt i32 %[[R]], %[[YV2]]
int renamed(const int x, const int w) pre(x > w) post(r : r > x);
int renamed(const int y, const int z) CONTRACTS(pre(y > z) post(s : s > y)) {
  return y + z;
}

template <class F> bool call(F f, int v) { return f(v); }

// The closure types of a non-inline function are not numbered; the one of
// the first declaration is still the only one that is used.
// CHECK-LABEL: define {{.*}} i32 @_Z9noninlinei(
// CHECK: call {{.*}} i1 @"_Z4callIZ9noninlineiE3$_0EbT_i"(
// CHECK: call {{.*}} i32 @"_ZZ9noninlineiENK3$_1clEi"(
// CHECK-LABEL: define {{.*}} i1 @"_Z4callIZ9noninlineiE3$_0EbT_i"(
// CHECK: call {{.*}} i1 @"_ZZ9noninlineiENK3$_0clEi"(
// CHECK-LABEL: define {{.*}} i32 @"_ZZ9noninlineiENK3$_1clEi"(
int noninline(int x) pre(call([](int v) { return v > 0; }, x));
int noninline(int y) CONTRACTS(pre(call([](int v) { return v > 0; }, y))) {
  auto l = [](int v) { return v + 1; };
  return l(y);
}

// Member functions defined out of line, with a 'this' in the predicate.
struct S {
  int m;
  int method(int x) pre(x > m) post(r : r > m);
};
// CHECK-LABEL: define {{.*}} i32 @_ZN1S6methodEi(ptr {{.*}} %this, i32 noundef %y)
// CHECK: %[[Y:y.addr]] = alloca i32
// CHECK: load i32, ptr %[[Y]]
// CHECK: getelementptr inbounds nuw %struct.S, ptr %{{.*}}, i32 0, i32 0
// CHECK: icmp sgt
int S::method(int y) CONTRACTS(pre(y > m) post(s : s > m)) { return y; }

// The closure type of the lambda-expression in the first declaration is
// numbered first within the (inline) function; the one in the body follows
// it. The closure type passed to the template is the first declaration's.
// CHECK-LABEL: define {{.*}} i32 @_Z6lambdai(
// CHECK: call {{.*}} i1 @_Z4callIZ6lambdaiEUliE_EbT_i(
// CHECK: call {{.*}} i32 @_ZZ6lambdaiENKUliE0_clEi(
inline int lambda(int x) pre(call([](int v) { return v > 0; }, x));
inline int lambda(int y) CONTRACTS(pre(call([](int v) { return v > 0; }, y))) {
  auto l = [](int v) { return v + 1; };
  return l(y);
}

// A function template declared with the contracts and defined without them:
// they are instantiated from the first declaration, before the body (also
// when the specialization is odr-used before its definition is instantiated).
template <class T> T tmpl(T x) pre(call([](T v) { return v > 0; }, x));
// CHECK-LABEL: define {{.*}} i32 @_Z4tmplIiET_S0_(i32 noundef %y)
// CHECK: call {{.*}} i1 @_Z4callIZ4tmplIiET_S1_EUliE_EbS1_i(
// CHECK: call {{.*}} i32 @_ZZ4tmplIiET_S0_ENKUliE0_clEi(
template <class T> T tmpl(T y) CONTRACTS(pre(call([](T v) { return v > 0; }, y))) {
  auto l = [](T v) { return v + 1; };
  return l(y);
}

// A member of a class template declared in the class and defined out of line.
template <class T> struct CT {
  T method(const T x) pre(x > T{}) post(r : r > x);
};
// CHECK-LABEL: define {{.*}} i32 @_ZN2CTIiE6methodEi(ptr {{.*}} %this, i32 noundef %y)
// CHECK: %[[Y:y.addr]] = alloca i32
// CHECK: %[[YV:.*]] = load i32, ptr %[[Y]]
// CHECK: icmp sgt i32 %[[YV]], 0
template <class T> T CT<T>::method(const T y) CONTRACTS(pre(y > T{}) post(s : s > y)) {
  return y + 1;
}

int use(int a) {
  return renamed(a, 0) + lambda(a) + noninline(a) + S{0}.method(a) +
         tmpl(a) + CT<int>().method(a);
}

// The deferred definitions: the closure types of the first declarations are
// the ones that are instantiated with and emitted; those of the bodies are
// numbered after them.
// CHECK-LABEL: define {{.*}} i1 @_Z4callIZ6lambdaiEUliE_EbT_i(
// CHECK: call {{.*}} i1 @_ZZ6lambdaiENKUliE_clEi(
// CHECK-LABEL: define {{.*}} i32 @_ZZ6lambdaiENKUliE0_clEi(
// CHECK-LABEL: define {{.*}} i1 @_ZZ6lambdaiENKUliE_clEi(
// CHECK-LABEL: define {{.*}} i1 @"_ZZ9noninlineiENK3$_0clEi"(
// CHECK-LABEL: define {{.*}} i1 @_Z4callIZ4tmplIiET_S1_EUliE_EbS1_i(
// CHECK: call {{.*}} i1 @_ZZ4tmplIiET_S0_ENKUliE_clEi(
// CHECK-LABEL: define {{.*}} i32 @_ZZ4tmplIiET_S0_ENKUliE0_clEi(
// CHECK-LABEL: define {{.*}} i1 @_ZZ4tmplIiET_S0_ENKUliE_clEi(
