// The program's replacement of ::handle_contract_violation is found by the
// contract-violation runtime through the C-linkage alias
// __handle_contract_violation that Clang emits next to it, in every way a
// program can be put together. This file is the translation unit using a
// contract (main) with -DUSER, and the translation unit defining the
// replacement with -DHANDLER; the runtime prints nothing when the replacement
// is used.
//
// REQUIRES: contracts-libcxx
//
// The handler in the translation unit of the contract.
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -DUSER -DHANDLER -DNAME=same-tu %s -o %t.same && %t.same 2>&1 | FileCheck %s --check-prefix=SAME
// SAME: [handler same-tu] pre
// SAME-NEXT: done
//
// The handler in another translation unit of the executable.
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -c -DHANDLER -DNAME=other-tu %s -o %t.handler.o
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -c -DUSER %s -o %t.user.o
// RUN: %clangxx %libcxx_flags %t.user.o %t.handler.o -o %t.other && %t.other 2>&1 | FileCheck %s --check-prefix=OTHER
// OTHER: [handler other-tu] pre
// OTHER-NEXT: done
//
// The handler in a translation unit compiled with -fvisibility=hidden: the
// alias is given default visibility regardless, since it is the runtime
// library that references it.
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -fvisibility=hidden -c -DHANDLER -DNAME=hidden %s -o %t.hidden.o
// RUN: %clangxx %libcxx_flags %t.user.o %t.hidden.o -o %t.hidden && %t.hidden 2>&1 | FileCheck %s --check-prefix=HIDDEN
// HIDDEN: [handler hidden] pre
// HIDDEN-NEXT: done
//
// The handler in a shared library the executable links against (the runtime
// resolves the alias at load time).
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -fPIC -shared -DHANDLER -DNAME=dso %s -o %t.dso.so
// RUN: %clangxx %libcxx_flags %t.user.o %t.dso.so -Wl,-rpath,%T -o %t.dso && %t.dso 2>&1 | FileCheck %s --check-prefix=DSO
// DSO: [handler dso] pre
// DSO-NEXT: done
//
// The handler in a shared library built with -fvisibility=hidden.
// RUN: %clangxx -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe %libcxx_flags -fPIC -shared -fvisibility=hidden -DHANDLER -DNAME=hidden-dso %s -o %t.hdso.so
// RUN: %clangxx %libcxx_flags %t.user.o %t.hdso.so -Wl,-rpath,%T -o %t.hdso && %t.hdso 2>&1 | FileCheck %s --check-prefix=HIDDEN-DSO
// HIDDEN-DSO: [handler hidden-dso] pre
// HIDDEN-DSO-NEXT: done
//
// Without a replacement, the default handler reports the violation.
// RUN: %clangxx %libcxx_flags %t.user.o -o %t.default && %t.default 2>&1 | FileCheck %s --check-prefix=DEFAULT
// DEFAULT: contract violation in pre(x > 0)
// DEFAULT: done
//
// The handler statically linked with libc++ and libc++abi: the runtime's weak
// reference binds at link time.
// RUN: %if contracts-libcxx-static %{ %clangxx %t.user.o %t.handler.o %libcxx_static_flags -o %t.static && %t.static 2>&1 | FileCheck %s --check-prefix=OTHER %}
//
// The same, with the alias subject to section garbage collection.
// RUN: %if contracts-libcxx-static && system-linux %{ %clangxx -Wl,--gc-sections %t.user.o %t.hidden.o %libcxx_static_flags -o %t.gc && %t.gc 2>&1 | FileCheck %s --check-prefix=HIDDEN %}

#include <contracts>
#include <cstdio>

#define STRINGIFY_(x) #x
#define STRINGIFY(x) STRINGIFY_(x)

#ifdef HANDLER
void handle_contract_violation(const std::contracts::contract_violation& v) {
  std::printf("[handler %s] %s\n", STRINGIFY(NAME), v.kind() == std::contracts::assertion_kind::pre ? "pre" : "other");
  std::fflush(stdout);
}
#endif

#ifdef USER
void f(int x) pre(x > 0) {}

int main() {
  f(-1);
  std::puts("done");
}
#endif
