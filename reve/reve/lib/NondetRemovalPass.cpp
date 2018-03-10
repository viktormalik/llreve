//
// Created by vmalik on 1/25/18.
//

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
