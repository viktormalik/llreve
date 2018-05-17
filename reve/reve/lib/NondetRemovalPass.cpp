/*
 * Pass for removing 'undef' values from the LLVM IR
 *
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#include "NondetRemovalPass.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>

using namespace llvm;

PreservedAnalyses NondetRemovalPass::run(
        Function &Fun, FunctionAnalysisManager &fam) {
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (auto PhiNode = dyn_cast<PHINode>(&Instr)) {
                for (unsigned i = 0; i < PhiNode->getNumIncomingValues(); ++i) {
                    Value *value = PhiNode->getIncomingValue(i);
                    BasicBlock *block = PhiNode->getIncomingBlock(i);
                    if (isa<UndefValue>(value)) {
                        // Undef values are replaced by call to $nondetX
                        // functions
                        auto newCall =
                                replaceNondetByCall(value,
                                                    block->getTerminator(),
                                                    Fun.getParent());
                        PhiNode->setIncomingValue(i, newCall);
                    }
                }
            } else {
                for (auto &Op : Instr.operands()) {
                    if (isa<UndefValue>(Op)) {
                        auto newCall = replaceNondetByCall(Op, &Instr,
                                                           Fun.getParent());
                        Op = newCall;
                    }
                }
            }
        }
    }
    return llvm::PreservedAnalyses();
}

void NondetRemovalPass::setNondet(Value *Value, LLVMContext &Context) {
    MDNode *M = MDNode::get(Context,
                            ConstantAsMetadata::get(
                                    ConstantInt::getTrue(Context)));
    if (auto Instr = dyn_cast<Instruction>(Value))
        Instr->setMetadata("is_nondet", M);
    else if (auto Fun = dyn_cast<Function>(Value))
        Fun->setMetadata("is_nondet", M);
}

/*
 * Creates a function replacing the 'nondet' value. One function is created for
 * each type. Later, functions are synchronised between modules based on their
 * suffices (this should be handled in a better way)
 */
Value * NondetRemovalPass::replaceNondetByCall(llvm::Value *toReplace,
                                               llvm::Instruction *insertBefore,
                                               llvm::Module *mod) {
    Type *type = toReplace->getType();
    auto FunMapIt = nondetFunctions.find(type);
    if (FunMapIt == nondetFunctions.end()) {
        Function *newFun = Function::Create(
                FunctionType::get(type, false),
                Function::LinkageTypes::ExternalLinkage,
                "$nondet" +
                std::to_string(nextNondetFun++),
                mod);
        setNondet(newFun, mod->getContext());
        FunMapIt = nondetFunctions.emplace(
                type, newFun).first;
    }
    auto newCall = CallInst::Create(FunMapIt->second, {}, "", insertBefore);
    setNondet(newCall, mod->getContext());
    return newCall;
}
