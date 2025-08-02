#include "clang/Basic/EricWFDebug.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/TextNodeDumper.h"

#include <sys/types.h>
#include <sys/wait.h>

namespace clang {

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


void EricWFDump(const DeclContext *DC, const ASTContext *Context) {
  if (!DC) {
    llvm::errs() << "Null Decl Context!\n";
    return;
  }
  if (DC->hasValidDeclKind()) {
    const auto *D = cast<Decl>(DC);
    createDumper(Context).Visit(D);
  } else {
    llvm::errs() << "DeclContext " << DC->getDeclKindName() << "\n";
  }
}

void printWithBanner(std::string_view CMessage, bool IsClosingBanner) {
  std::string Message = std::string(CMessage);
  if (IsClosingBanner)
    Message = "Done " + Message;
  const int BannerWidth = 78;
  const int MessageWidth = Message.size();
  const int BannerLeftWidth = (BannerWidth - MessageWidth - 2) / 2;
  const int BannerRightWidth = BannerWidth - MessageWidth - 2 - BannerLeftWidth;

  std::string FullMessage = std::string(BannerLeftWidth, '=') + " " + Message +
                            " " + std::string(BannerRightWidth, '=');
  if (IsClosingBanner)
    llvm::errs() << "\n";
  llvm::errs() << FullMessage << "\n";
}

struct BannerWrapper {
  std::string_view CMessage;

  explicit BannerWrapper(std::string_view Message) : CMessage(Message) {
    if (!CMessage.empty())
      printWithBanner(CMessage, false);
  }

  ~BannerWrapper() {
    if (!CMessage.empty())
      printWithBanner(CMessage, true);
  }

  BannerWrapper(BannerWrapper const &) = delete;
  BannerWrapper &operator=(const BannerWrapper &) = delete;
};

void EricWFDump(std::string_view Message, const Stmt *S,
                const ASTContext *Context) {
  BannerWrapper Banner(Message);
  EricWFDump(S, Context);
}

void EricWFDump(std::string_view Message, const Decl *D,
                const ASTContext *Context) {
  BannerWrapper Banner(Message);
  EricWFDump(D, Context);
}

void EricWFDump(std::string_view Message, const DeclContext *D,
                const ASTContext *Context) {
  BannerWrapper Banner(Message);
  EricWFDump(D, Context);
}

namespace ericwf_impl {
ForkResult ForkInternal() {
  if constexpr (EricWFDebugEnabled) {
    pid_t c_pid = fork();
    if (c_pid == -1) {
      llvm::errs() << "Failed to fork\n";
      std::exit(1);
    } else if (c_pid > 0) {
      int status;
      wait(&status);
      if (WIFEXITED(status)) {
        printf("The process ended with exit(%d).\\n", WEXITSTATUS(status));
        return {WhoAmI::Parent, WEXITSTATUS(status)};
      }
      if (WIFSIGNALED(status)) {
        printf("The process ended with kill -%d.\\n", WTERMSIG(status));
        return {WhoAmI::Parent, WTERMSIG(status)};
      }
      return {WhoAmI::Parent, status};
    } else {
      return {WhoAmI::Child, 0};
    }
  } else {
    std::terminate();
  }
}
} // namespace ericwf_impl

} // namespace clang