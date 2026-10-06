//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// The contract-violation runtime of the Itanium C++ Contracts ABI: the entry
// points the compiler calls when a contract assertion is violated, the
// dispatch to the contract-violation handler, and the default handler.
//
// libc++abi hosts the runtime when libc++ is built on it; the implementation
// is shared with libc++, which hosts it itself when it is built on another ABI
// library (as is done for operator new). It is exported with libc++abi's
// annotations through <__contracts/abi.h>, which the cxxabi.h configuration
// included here selects.
//
//===----------------------------------------------------------------------===//

#include "cxxabi.h"

#include "support/contracts_runtime.ipp"
