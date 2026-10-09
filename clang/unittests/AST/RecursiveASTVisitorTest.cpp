//===- unittest/AST/RecursiveASTVisitorTest.cpp ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/TypeLoc.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/FunctionExtras.h"
#include "llvm/ADT/STLExtras.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include <cassert>

using namespace clang;
using ::testing::ElementsAre;

namespace {
class ProcessASTAction : public clang::ASTFrontendAction {
public:
  ProcessASTAction(llvm::unique_function<void(clang::ASTContext &)> Process)
      : Process(std::move(Process)) {
    assert(this->Process);
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) {
    class Consumer : public ASTConsumer {
    public:
      Consumer(llvm::function_ref<void(ASTContext &CTx)> Process)
          : Process(Process) {}

      void HandleTranslationUnit(ASTContext &Ctx) override { Process(Ctx); }

    private:
      llvm::function_ref<void(ASTContext &CTx)> Process;
    };

    return std::make_unique<Consumer>(Process);
  }

private:
  llvm::unique_function<void(clang::ASTContext &)> Process;
};

enum class VisitEvent {
  StartTraverseFunction,
  EndTraverseFunction,
  StartTraverseAttr,
  EndTraverseAttr,
  StartTraverseEnum,
  EndTraverseEnum,
  StartTraverseTypedefType,
  EndTraverseTypedefType,
  StartTraverseObjCInterface,
  EndTraverseObjCInterface,
  StartTraverseObjCProtocol,
  EndTraverseObjCProtocol,
  StartTraverseObjCProtocolLoc,
  EndTraverseObjCProtocolLoc,
};

class CollectInterestingEvents
    : public RecursiveASTVisitor<CollectInterestingEvents> {
public:
  bool TraverseFunctionDecl(FunctionDecl *D) {
    Events.push_back(VisitEvent::StartTraverseFunction);
    bool Ret = RecursiveASTVisitor::TraverseFunctionDecl(D);
    Events.push_back(VisitEvent::EndTraverseFunction);

    return Ret;
  }

  bool TraverseAttr(Attr *A) {
    Events.push_back(VisitEvent::StartTraverseAttr);
    bool Ret = RecursiveASTVisitor::TraverseAttr(A);
    Events.push_back(VisitEvent::EndTraverseAttr);

    return Ret;
  }

  bool TraverseEnumDecl(EnumDecl *D) {
    Events.push_back(VisitEvent::StartTraverseEnum);
    bool Ret = RecursiveASTVisitor::TraverseEnumDecl(D);
    Events.push_back(VisitEvent::EndTraverseEnum);

    return Ret;
  }

  bool TraverseTypedefTypeLoc(TypedefTypeLoc TL, bool TraverseQualifier) {
    Events.push_back(VisitEvent::StartTraverseTypedefType);
    bool Ret =
        RecursiveASTVisitor::TraverseTypedefTypeLoc(TL, TraverseQualifier);
    Events.push_back(VisitEvent::EndTraverseTypedefType);

    return Ret;
  }

  bool TraverseObjCInterfaceDecl(ObjCInterfaceDecl *ID) {
    Events.push_back(VisitEvent::StartTraverseObjCInterface);
    bool Ret = RecursiveASTVisitor::TraverseObjCInterfaceDecl(ID);
    Events.push_back(VisitEvent::EndTraverseObjCInterface);

    return Ret;
  }

  bool TraverseObjCProtocolDecl(ObjCProtocolDecl *PD) {
    Events.push_back(VisitEvent::StartTraverseObjCProtocol);
    bool Ret = RecursiveASTVisitor::TraverseObjCProtocolDecl(PD);
    Events.push_back(VisitEvent::EndTraverseObjCProtocol);

    return Ret;
  }

  bool TraverseObjCProtocolLoc(ObjCProtocolLoc ProtocolLoc) {
    Events.push_back(VisitEvent::StartTraverseObjCProtocolLoc);
    bool Ret = RecursiveASTVisitor::TraverseObjCProtocolLoc(ProtocolLoc);
    Events.push_back(VisitEvent::EndTraverseObjCProtocolLoc);

    return Ret;
  }

  std::vector<VisitEvent> takeEvents() && { return std::move(Events); }

private:
  std::vector<VisitEvent> Events;
};

std::vector<VisitEvent> collectEvents(llvm::StringRef Code,
                                      const Twine &FileName = "input.cc") {
  CollectInterestingEvents Visitor;
  clang::tooling::runToolOnCode(
      std::make_unique<ProcessASTAction>(
          [&](clang::ASTContext &Ctx) { Visitor.TraverseAST(Ctx); }),
      Code, FileName);
  return std::move(Visitor).takeEvents();
}
} // namespace

TEST(RecursiveASTVisitorTest, AttributesInsideDecls) {
  /// Check attributes are traversed inside TraverseFunctionDecl.
  llvm::StringRef Code = R"cpp(
__attribute__((annotate("something"))) int foo() { return 10; }
  )cpp";

  EXPECT_THAT(collectEvents(Code),
              ElementsAre(VisitEvent::StartTraverseFunction,
                          VisitEvent::StartTraverseAttr,
                          VisitEvent::EndTraverseAttr,
                          VisitEvent::EndTraverseFunction));
}

TEST(RecursiveASTVisitorTest, EnumDeclWithBase) {
  // Check enum and its integer base is visited.
  llvm::StringRef Code = R"cpp(
  typedef int Foo;
  enum Bar : Foo;
  )cpp";

  EXPECT_THAT(collectEvents(Code),
              ElementsAre(VisitEvent::StartTraverseEnum,
                          VisitEvent::StartTraverseTypedefType,
                          VisitEvent::EndTraverseTypedefType,
                          VisitEvent::EndTraverseEnum));
}

TEST(RecursiveASTVisitorTest, InterfaceDeclWithProtocols) {
  // Check interface and its protocols are visited.
  llvm::StringRef Code = R"cpp(
  @protocol Foo
  @end
  @protocol Bar
  @end

  @interface SomeObject <Foo, Bar>
  @end
  )cpp";

  EXPECT_THAT(collectEvents(Code, "input.m"),
              ElementsAre(VisitEvent::StartTraverseObjCProtocol,
                          VisitEvent::EndTraverseObjCProtocol,
                          VisitEvent::StartTraverseObjCProtocol,
                          VisitEvent::EndTraverseObjCProtocol,
                          VisitEvent::StartTraverseObjCInterface,
                          VisitEvent::StartTraverseObjCProtocolLoc,
                          VisitEvent::EndTraverseObjCProtocolLoc,
                          VisitEvent::StartTraverseObjCProtocolLoc,
                          VisitEvent::EndTraverseObjCProtocolLoc,
                          VisitEvent::EndTraverseObjCInterface));
}

namespace {
class CollectContracts : public RecursiveASTVisitor<CollectContracts> {
public:
  bool VisitImplicitCode = false;
  bool shouldVisitImplicitCode() const { return VisitImplicitCode; }

  bool VisitContractStmt(ContractStmt *S) {
    Kinds.push_back(S->getContractKindString().str());
    return true;
  }
  bool VisitResultNameDecl(ResultNameDecl *D) {
    ++ResultNames;
    return true;
  }
  bool VisitBuiltinType(BuiltinType *T) {
    ++BuiltinTypes;
    return true;
  }

  std::vector<std::string> Kinds;
  unsigned ResultNames = 0;
  unsigned BuiltinTypes = 0;
};

CollectContracts collectContracts(llvm::StringRef Code, bool Implicit) {
  CollectContracts Visitor;
  Visitor.VisitImplicitCode = Implicit;
  clang::tooling::runToolOnCodeWithArgs(
      std::make_unique<ProcessASTAction>(
          [&](clang::ASTContext &Ctx) { Visitor.TraverseAST(Ctx); }),
      Code, {"-std=c++26", "-fcontracts"}, "input.cc");
  return Visitor;
}
} // namespace

TEST(RecursiveASTVisitorTest, FunctionContracts) {
  llvm::StringRef Code = R"cpp(
  int g(int);
  int f(int x) pre(g(x) > 0) post(r : g(r) > 0) {
    contract_assert(x != 3);
    return x;
  }
  )cpp";
  for (bool Implicit : {false, true}) {
    CollectContracts V = collectContracts(Code, Implicit);
    EXPECT_THAT(V.Kinds, ElementsAre("pre", "post", "contract_assert"));
    EXPECT_EQ(V.ResultNames, 1u);
  }
}

TEST(RecursiveASTVisitorTest, LambdaContracts) {
  // The contracts of a lambda's call operator are written in the source, so
  // they must be visited even when not visiting implicit code (where the call
  // operator itself is not traversed).
  llvm::StringRef Code = R"cpp(
  int g(int);
  void f() {
    auto l = [](int x) pre(g(x) > 0) post(r : g(r) > 0) { return x; };
  }
  )cpp";
  for (bool Implicit : {false, true}) {
    CollectContracts V = collectContracts(Code, Implicit);
    EXPECT_THAT(V.Kinds, ElementsAre("pre", "post")) << "Implicit=" << Implicit;
    EXPECT_EQ(V.ResultNames, 1u);
  }
}

TEST(RecursiveASTVisitorTest, ResultNameType) {
  // The (implicit) type of a result name is traversed: the result name
  // contributes one more 'int' than the same declaration without it.
  CollectContracts With = collectContracts("int h() post(r : true);", false);
  CollectContracts Without = collectContracts("int h() post(true);", false);
  EXPECT_EQ(With.ResultNames, 1u);
  EXPECT_EQ(Without.ResultNames, 0u);
  EXPECT_EQ(With.BuiltinTypes, Without.BuiltinTypes + 1);
}
