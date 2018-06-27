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
#include <set>
#include "MonoPair.h"

class UndefRemovalPass : public llvm::PassInfoMixin<UndefRemovalPass> {
  public:
    llvm::PreservedAnalyses run(llvm::Function &Fun,
                                llvm::FunctionAnalysisManager &fam);

  protected:
    std::map<const llvm::Type *, llvm::Function *> undefFunctions;

    void setUndef(llvm::Value *Value, llvm::LLVMContext &Context);
    llvm::Value * replaceUndefByCall(llvm::Value *toReplace,
                                     llvm::Instruction *insertBefore,
                                     llvm::Module *mod);
};

auto coupleUndefCalls(MonoPair<llvm::Module &> modules)
    -> std::set<MonoPair<llvm::Function *>>;
