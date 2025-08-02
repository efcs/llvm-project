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
#include <cassert>
#include <string_view>

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
      llvm::sys::PrintStackTrace(llvm::errs(), N);                             \
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

#ifdef ERICWF_DEBUG_ENABLED
#define ERICWF_FANCY_ASSERT(...)                                               \
  if (::clang::EricWFFancyAssertHolder assertObj{                              \
          bool(__VA_ARGS__), #__VA_ARGS__, __FILE__, __PRETTY_FUNCTION__,      \
          __LINE__};                                                           \
      !assertObj.value)
#else
#define ERICWF_FANCY_ASSERT(...)                                               \
  assert(__VA_ARGS__);                                                         \
  if constexpr (false)
#endif

namespace clang {
class Stmt;
class Decl;
class DeclContext;
class ASTContext;

class EricWFFancyAssertHolder {
public:
  EricWFFancyAssertHolder(bool condition, const char *condition_expr,
                          const char *file, const char *func, int line)
      : value(condition), expr(condition_expr), file(file), func(func),
        line(line) {}

  void printAndDie() {
    llvm::errs() << file << ":" << line << ": ";
    if (func) {
      llvm::errs() << func << ": ";
    }
    llvm::errs() << "Assertion `" << expr << "' failed.\n";
    std::terminate();
  }

  ~EricWFFancyAssertHolder() {
    if (!value) {
      printAndDie();
    }
  }

  EricWFFancyAssertHolder(EricWFFancyAssertHolder const &) = delete;
  EricWFFancyAssertHolder &operator=(EricWFFancyAssertHolder const &) = delete;

  explicit operator bool() const { return !value; }

  bool value;
  const char *expr;
  const char *file;
  const char *func;
  int line;
};

void EricWFDump(const Stmt *S, const ASTContext *Ctx = nullptr);
void EricWFDump(const Decl *D, const ASTContext *Ctx = nullptr);
void EricWFDump(const DeclContext *D, const ASTContext *Ctx = nullptr);

void EricWFDump(std::string_view Message, const Stmt *S,
                const ASTContext *Ctx = nullptr);

void EricWFDump(std::string_view Message, const Decl *D,
                const ASTContext *Ctx = nullptr);
void EricWFDump(std::string_view Message, const DeclContext *D,
                const ASTContext *Ctx = nullptr);

namespace ericwf_impl {
enum class WhoAmI { Child, Parent };
struct ForkResult {
  WhoAmI Who;
  int ChildExitCode = -1;
};
ForkResult ForkInternal();

} // end namespace ericwf_impl


template <class Pred,  class Callable>
bool EricWFDebugFork(Pred &&P, Callable &&C) {
  if constexpr (EricWFDebugEnabled) {
    if (Pred()) {
      ericwf_impl::ForkResult Res = ericwf_impl::ForkInternal();
      if (Res.Who == ericwf_impl::WhoAmI::Child) {
        C();
        std::exit(0);
      } else {
        int ChildExitCode = Res.ChildExitCode;
        return ChildExitCode == 0;
      }
    }
  }
}

} // end namespace clang

#endif // LLVM_CLANG_BASIC_ERICWFDEBUG_H
