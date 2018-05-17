/*
 * Pass for removing 'undef' values from the LLVM IR
 *
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#pragma once

#include <llvm/IR/PassManager.h>
#include <llvm/IR/Instructions.h>

class NondetRemovalPass : public llvm::PassInfoMixin<NondetRemovalPass> {
  public:
    llvm::PreservedAnalyses run(llvm::Function &Fun,
                                llvm::FunctionAnalysisManager &fam);

  protected:
    std::map<const llvm::Type *, llvm::Function *> nondetFunctions;
    unsigned nextNondetFun = 0;

    void setNondet(llvm::Value *Value, llvm::LLVMContext &Context);
    llvm::Value * replaceNondetByCall(llvm::Value *toReplace,
                                      llvm::Instruction *insertBefore,
                                      llvm::Module *mod);
};
