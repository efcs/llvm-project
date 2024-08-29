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

static TextNodeDumper createDumper(const ASTContext *Context) {
  if (Context)
    return TextNodeDumper(llvm::errs(), *Context, true);
  else
    return TextNodeDumper(llvm::errs(), true);
}

void EricWFDump(const Stmt *S, const ASTContext *Context) {
  createDumper(Context).Visit(S);
}

void EricWFDump(const Decl *D, const ASTContext *Context) {
  createDumper(Context).Visit(D);
}

void printWithBanner(const char *CMessage, bool IsClosingBanner) {
  std::string Message = std::string(CMessage);
  if (IsClosingBanner)
    Message = "Done " + Message;
  const int BannerWidth = 78;
  const int MessageWidth = Message.size();
  const int BannerLeftWidth = (BannerWidth - MessageWidth - 2) / 2;
  const int BannerRightWidth = BannerWidth - MessageWidth - 2 - BannerLeftWidth;

  std::string FullMessage = std::string('=', BannerLeftWidth) + " " + Message +
                            " " + std::string('=', BannerRightWidth);
  if (IsClosingBanner)
    llvm::errs() << "\n";
  llvm::errs() << FullMessage << "\n";
}

struct BannerWrapper {
  const char *CMessage;

  explicit BannerWrapper(const char *Message) : CMessage(Message) {
    if (CMessage)
      printWithBanner(CMessage, false);
  }

  ~BannerWrapper() {
    if (CMessage)
      printWithBanner(CMessage, true);
  }

  BannerWrapper(BannerWrapper const &) = delete;
  BannerWrapper &operator=(const BannerWrapper &) = delete;
};

void EricWFDump(const char *Message, const Stmt *S, const ASTContext *Context) {
  BannerWrapper Banner(Message);
  EricWFDump(S, Context);
}

void EricWFDump(const char *Message, const Decl *D, const ASTContext *Context) {
  BannerWrapper Banner(Message);
  EricWFDump(D, Context);
}

} // namespace clang