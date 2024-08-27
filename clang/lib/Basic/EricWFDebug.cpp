#include "clang/Basic/EricWFDebug.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/TextNodeDumper.h"

namespace clang {
void showDeclContext(const DeclContext *DC, const ASTContext *) {
  assert(isa<Decl>(DC));
  showDecl(cast<Decl>(DC));
}

void showDecl(const Decl *D) {
  llvm::errs() << D->getDeclKindName();
  if (auto *ND = dyn_cast<NamedDecl>(D)) {
    llvm::errs() << " " << ND->getQualifiedNameAsString();
  }
  llvm::errs() << "\n";
}
} // namespace clang