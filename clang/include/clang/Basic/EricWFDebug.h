//===- EricWFDebug.h - A File that needs to die -----------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Defines debugging macros that should only be present when EricWF is
/// debugging the code. It's a nice way to keep track of places EricWF has
/// modified and needs to remove.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_BASIC_ERICWFDEBUG_H
#define LLVM_CLANG_BASIC_ERICWFDEBUG_H

#include "llvm/Support/Debug.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"

namespace clang {

#ifdef ERICWF_DEBUG_ENABLED
constexpr bool EricWFDebugEnabled = true;
#else
constexpr bool EricWFDebugEnabled = false;
#endif

inline void printSourceLocation(const char *file, const char *func,
                                unsigned line) {
  if (not EricWFDebugEnabled)
    return;
  llvm::errs() << file << ":" << line;
  if (func != nullptr)
    llvm::errs() << ": in " << func;
  llvm::errs() << "\n";
}

#define ERICWF_PRINT_SOURCE_LOC()                                              \
  ::clang::printSourceLocation(__FILE__, __func__, __LINE__)

#define ERICWF_STACK_TRACE(N)                                                  \
  do {                                                                         \
    if (::clang::EricWFDebugEnabled) {                                         \
      llvm::PrintStackTrace(llvm::errs(), N)                                   \
    }                                                                          \
  } while (0)

} // namespace clang

#define ERICWF_PRINT(x)                                                        \
  do {                                                                         \
    ERICWF_PRINT_SOURCE_LOC();                                                 \
    if constexpr (::clang::EricWFDebugEnabled)                                 \
      llvm::errs() << #x << " = " << x;                                        \
  } while (0)

#define ERICWF_DEBUG_BLOCK                                                     \
  ERICWF_PRINT_SOURCE_LOC();                                                   \
  if constexpr (::clang::EricWFDebugEnabled)

#define ERICWF_DEBUG_BLOCK_QUIET if constexpr (::clang::EricWFDebugEnabled)

#ifdef ERICWF_DEBUG_ENABLED
#define ERICWF_ASSERT(...) assert(__VA_ARGS__)
#else
#define ERICWF_ASSERT(...) ((void)0)
#endif

#endif // LLVM_CLANG_BASIC_ERICWFDEBUG_H
