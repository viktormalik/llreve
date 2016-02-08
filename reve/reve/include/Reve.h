#pragma once

#include "MonoPair.h"
#include "PathAnalysis.h"
#include "SMT.h"

#include "clang/CodeGen/CodeGenAction.h"
#include "clang/Driver/Driver.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Option/Option.h"

auto main(int argc, const char **argv) -> int;
auto zipFunctions(llvm::Module &mod1, llvm::Module &mod2) -> llvm::ErrorOr<
    std::vector<MonoPair<llvm::Function *>>>;
auto initializeArgs(const char *exeName, std::string input1, std::string input2)
    -> std::vector<const char *>;
auto initializeDiagnostics(void) -> std::unique_ptr<clang::DiagnosticsEngine>;
auto initializeDriver(clang::DiagnosticsEngine &diags)
    -> std::unique_ptr<clang::driver::Driver>;
auto preprocessFunction(llvm::Function &fun, std::string prefix)
    -> std::shared_ptr<llvm::FunctionAnalysisManager>;
auto getCmd(clang::driver::Compilation &comp, clang::DiagnosticsEngine &diags)
    -> llvm::ErrorOr<
    MonoPair<llvm::opt::ArgStringList>>;
template <typename T> auto makeErrorOr(T Arg) -> llvm::ErrorOr<T>;
auto getModule(const char *exeName, std::string input1, std::string input2)
    -> MonoPair<std::unique_ptr<clang::CodeGenAction>>;
auto getCodeGenAction(const llvm::opt::ArgStringList &ccArgs,
                      clang::DiagnosticsEngine &diags)
    -> std::unique_ptr<clang::CodeGenAction>;
auto parseInOutInvs(std::string fileName1, std::string fileName2)
    -> MonoPair<SMTRef>;
auto processFile(std::string file, SMTRef &in, SMTRef &out) -> void;
auto externDeclarations(llvm::Module &mod1, llvm::Module &mod2,
                        std::vector<SMTRef> &declarations, uint8_t mem,
                        std::multimap<string, string> funCondMap) -> void;
auto funArgs(llvm::Function &fun, std::string prefix, uint32_t varArgs)
    -> std::vector<SortedVar>;
auto getVarArgs(llvm::Function &fun) -> std::set<uint32_t>;
auto externFunDecl(llvm::Function &fun, int program, uint8_t mem)
    -> std::vector<SMTRef>;
auto doesNotRecurse(llvm::Function &fun) -> bool;
auto globalDeclarations(llvm::Module &mod1, llvm::Module &mod2)
    -> std::vector<SMTRef>;
auto globalDeclarationsForMod(int globalPointer, llvm::Module &mod,
                              llvm::Module &otherMod, int program) -> std::vector<SMTRef>;
auto collectFunConds() -> std::multimap<string, string>;
auto collectFunCondsInFile(std::string file) -> std::multimap<string, string>;
auto doesAccessMemory(const llvm::Module &mod) -> bool;