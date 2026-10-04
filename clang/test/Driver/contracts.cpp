// Check that the C++ contracts options are forwarded to cc1 only when
// contracts are enabled.

// RUN: %clang -### -std=c++26 -c %s 2>&1 | FileCheck --check-prefix=OFF %s
// RUN: %clang -### -std=c++26 -fcontracts -fno-contracts -c %s 2>&1 | FileCheck --check-prefix=OFF %s
// OFF-NOT: "-fcontracts"
// OFF-NOT: "-fcontract-

// RUN: %clang -### -std=c++26 -fcontracts -c %s 2>&1 | FileCheck --check-prefix=ON %s
// ON: "-fcontracts"
// ON-NOT: "-fcontract-evaluation-semantic=
// ON-NOT: "-fcontract-group-evaluation-semantic=

// RUN: %clang -### -std=c++26 -fcontracts -fcontract-evaluation-semantic=observe \
// RUN:   -fcontract-group-evaluation-semantic=a=ignore,b.c=enforce \
// RUN:   -fcontract-group-evaluation-semantic=d=quick_enforce -c %s 2>&1 \
// RUN:   | FileCheck --check-prefix=SEM %s
// SEM: "-fcontracts"
// SEM-SAME: "-fcontract-evaluation-semantic=observe"
// SEM-SAME: "-fcontract-group-evaluation-semantic=a=ignore,b.c=enforce,d=quick_enforce"

// RUN: %clang -### -std=c++26 -fcontracts -fno-contract-exceptions \
// RUN:   -fno-contract-constification -fno-contract-lambda-capture-restrictions \
// RUN:   -c %s 2>&1 | FileCheck --check-prefix=NEG %s
// NEG: "-fcontracts"
// NEG-SAME: "-fno-contract-exceptions"
// NEG-SAME: "-fno-contract-constification"
// NEG-SAME: "-fno-contract-lambda-capture-restrictions"

// Contract options without -fcontracts are unused.
// RUN: %clang -### -std=c++26 -fcontract-evaluation-semantic=observe -c %s 2>&1 \
// RUN:   | FileCheck --check-prefix=UNUSED %s
// UNUSED: warning: argument unused during compilation: '-fcontract-evaluation-semantic=observe'

// -fcontracts requires C++11 or later.
// RUN: not %clang -std=c++98 -fcontracts -fsyntax-only %s 2>&1 | FileCheck --check-prefix=CXX98 %s
// CXX98: error: invalid argument '-fcontracts' not allowed with 'c++98'
// RUN: not %clang -x c -std=c17 -fcontracts -fsyntax-only %s 2>&1 | FileCheck --check-prefix=C %s
// C: error: invalid argument '-fcontracts' not allowed with 'c17'

// Invalid group names and semantics are diagnosed.
// RUN: not %clang -std=c++26 -fcontracts -fsyntax-only \
// RUN:   -fcontract-group-evaluation-semantic=a..b=enforce %s 2>&1 | FileCheck --check-prefix=BAD-GROUP %s
// BAD-GROUP: error: contract group name "a..b" cannot contain empty subgroup ".."
// RUN: not %clang -std=c++26 -fcontracts -fsyntax-only \
// RUN:   -fcontract-group-evaluation-semantic=a=bogus %s 2>&1 | FileCheck --check-prefix=BAD-SEM %s
// BAD-SEM: error: contract group name "bogus" is an invalid value
