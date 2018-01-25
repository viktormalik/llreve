//
// Created by vmalik on 1/25/18.
//

#pragma once

#include <llvm/IR/PassManager.h>

class NondetRemovalPass : public llvm::PassInfoMixin<NondetRemovalPass> {
  public:
    llvm::PreservedAnalyses run(llvm::Function &Fun,
                                llvm::FunctionAnalysisManager &fam);

  protected:
    void setNondet(llvm::Value *Value, llvm::LLVMContext &Context);
};
