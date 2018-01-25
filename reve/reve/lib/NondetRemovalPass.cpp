//
// Created by vmalik on 1/25/18.
//

#include "NondetRemovalPass.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>

using namespace llvm;

PreservedAnalyses NondetRemovalPass::run(
        Function &Fun, FunctionAnalysisManager &fam) {
    std::map<const Type *, Function *> nondetFunctions;
    unsigned nextNondetFun = 0;
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (auto PhiNode = dyn_cast<PHINode>(&Instr)) {
                for (unsigned i = 0; i < PhiNode->getNumIncomingValues(); ++i) {
                    Value *value = PhiNode->getIncomingValue(i);
                    BasicBlock *block = PhiNode->getIncomingBlock(i);
                    if (isa<UndefValue>(value)) {
                        Type *type = value->getType();
                        auto FunMapIt = nondetFunctions.find(type);
                        if (FunMapIt == nondetFunctions.end()) {
                            Function *newFun = Function::Create(
                                    FunctionType::get(type, false),
                                    Function::LinkageTypes::ExternalLinkage,
                                    "$nondet" +
                                    std::to_string(nextNondetFun++),
                                    Fun.getParent());
                            setNondet(newFun, Fun.getContext());
                            FunMapIt = nondetFunctions.emplace(
                                    type, newFun).first;
                        }
                        auto newCall = CallInst::Create(FunMapIt->second,
                                                        {}, "",
                                                        block->getTerminator());
                        setNondet(newCall, Fun.getContext());
                        PhiNode->setIncomingValue(i, newCall);
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
