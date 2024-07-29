//===-- CGContracts.h - state for LLVM CodeGen for Contracts  ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This is the internal state used for llvm translation for contracts
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_CODEGEN_CGCONTRACTS_H
#define LLVM_CLANG_LIB_CODEGEN_CGCONTRACTS_H

#include "clang/AST/CharUnits.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/Type.h"
#include "clang/Basic/ContractOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Value.h"

namespace llvm {
class Value;
}

namespace clang {
namespace CodeGen {
class CodeGenFunction;
class CodeGenModule;

class CodeGenContracts {
public:
  CodeGenContracts(CodeGenFunction &CGF);

  CodeGenFunction *operator->() { return &CGF; }
  CodeGenFunction const *operator->() const { return &CGF; }

  CodeGenFunction &CGF;
  CodeGenModule &CGM;
  const ASTContext &Ctx;
};

class CGContractInfo;

struct ContractViolationCallArgs {

  ContractEvaluationSemantic Semantic;
  ContractViolationDetection DetectionMode;

  llvm::Value *SemanticVal = nullptr;
  llvm::Value *DetectionVal = nullptr;
  llvm::Value *LocationVal = nullptr;
};

struct ContractViolationBlock {
  enum ViolationBlockKind {
    /// A singular block that simply traps.
    VBK_SharedTrap,

    /// An Observe block - Emitted as
    ///  __
    ///  try {
    ///    bool result = <predicate>
    ///

    VBK_InlineEnforce,
    VBK_InlineObserve,
    VBK_SharedEnforce
  };

  ContractViolationBlock(CodeGenFunction &CGF, ContractEvaluationSemantic Sem);

  void addIncoming(CodeGenFunction &CGF, ContractStmt const *);
  void addIncomingWithReturn(CodeGenFunction &CGF, ContractStmt const *,
                             llvm::BasicBlock *RetBlock);

  const ContractEvaluationSemantic Semantic;

  bool handlerCanReturnNormally() const {
    return Semantic == ContractEvaluationSemantic::Observe;
  }

  bool invokesHandler() const {
    return Semantic == ContractEvaluationSemantic::Enforce ||
           Semantic == ContractEvaluationSemantic::Observe;
  }

  bool isTrap() const {
    return Semantic == ContractEvaluationSemantic::QuickEnforce;
  }

  bool isObserves() const {
    return Semantic == ContractEvaluationSemantic::Observe;
  }

  bool isEnforce() const {
    return Semantic == ContractEvaluationSemantic::Enforce;
  }

  ViolationBlockKind getBlockKind() const { return BlockKind; }

  llvm::BasicBlock *getBlock() const {
    // C++ should allow this by way of the "common initial sequence" rule.
    return BlockInfo.SharedTrap.Block;
  }

private:
  void CreateSharedEnforceBlock(CodeGenFunction &CGF);

  llvm::BasicBlock *CreateTrapBlock(CodeGenFunction &CGF);
  llvm::BasicBlock *CreateObserveBlock(CodeGenFunction &CGF);

private:
  struct TrappingBlockInfo {
    llvm::BasicBlock *Block;
  };

  struct SharedEnforceBlock {
    llvm::BasicBlock *Block;
    llvm::PHINode *Location;
    llvm::PHINode *Detection;
  };

  struct InlineObserveBlock {
    llvm::BasicBlock *Block;
    llvm::Value *Location;
    llvm::Value *Detection;
    llvm::BasicBlock *ContinueBlock;
  };

  struct InlineEnforceBlock {
    llvm::BasicBlock *Block;
    llvm::Value *Location;
    llvm::Value *Detection;
    llvm::BasicBlock *ContinueBlock;
  };

  union BlockInfoUnion {
    TrappingBlockInfo SharedTrap;
    SharedEnforceBlock SharedEnforce;
    InlineEnforceBlock InlineEnforce;
    InlineObserveBlock InlineObserve;
  } BlockInfo;

  ViolationBlockKind BlockKind;
};

struct ContractsInfo {
  std::optional<ContractViolationBlock> &
  operator[](ContractEvaluationSemantic Sem) {
    switch (Sem) {
    case ContractEvaluationSemantic::Enforce:
      return ViolationBlocks[0];
    case ContractEvaluationSemantic::QuickEnforce:
      return ViolationBlocks[1];
    case ContractEvaluationSemantic::Observe:
      return ViolationBlocks[2];
    case ContractEvaluationSemantic::Ignore:
    case ContractEvaluationSemantic::Invalid:
      llvm_unreachable("Unhandeled case");
    }
  }

  std::optional<ContractViolationBlock> ViolationBlocks[3];
};

} // end namespace CodeGen
} // end namespace clang

#endif
