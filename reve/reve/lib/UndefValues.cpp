/*
 * Pass for removing 'undef' values from the LLVM IR
 *
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#include "UndefValues.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <Helper.h>

using namespace llvm;

PreservedAnalyses UndefRemovalPass::run(
        Function &Fun, FunctionAnalysisManager &fam) {
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (auto PhiNode = dyn_cast<PHINode>(&Instr)) {
                for (unsigned i = 0; i < PhiNode->getNumIncomingValues(); ++i) {
                    Value *value = PhiNode->getIncomingValue(i);
                    BasicBlock *block = PhiNode->getIncomingBlock(i);
                    if (isa<UndefValue>(value)) {
                        // Undef values are replaced by call to $undef
                        // functions
                        auto newCall =
                                replaceUndefByCall(value,
                                                   block->getTerminator(),
                                                   Fun.getParent());
                        PhiNode->setIncomingValue(i, newCall);
                    }
                }
            } else {
                for (auto &Op : Instr.operands()) {
                    if (isa<UndefValue>(Op)) {
                        auto newCall = replaceUndefByCall(Op, &Instr,
                                                          Fun.getParent());
                        Op = newCall;
                    }
                }
            }
        }
    }
    return llvm::PreservedAnalyses();
}

void UndefRemovalPass::setUndef(Value *Value, LLVMContext &Context) {
    MDNode *M = MDNode::get(Context,
                            ConstantAsMetadata::get(
                                    ConstantInt::getTrue(Context)));
    if (auto Instr = dyn_cast<Instruction>(Value))
        Instr->setMetadata("is_undef", M);
    else if (auto Fun = dyn_cast<Function>(Value))
        Fun->setMetadata("is_undef", M);
}

/*
 * Creates a function replacing the 'undef' value. One function is created for
 * each type. Later, functions are synchronised between modules based on their
 * suffices
 */
Value * UndefRemovalPass::replaceUndefByCall(llvm::Value *toReplace,
                                             llvm::Instruction *insertBefore,
                                             llvm::Module *mod) {
    Type *type = toReplace->getType();
    auto FunMapIt = undefFunctions.find(type);
    if (FunMapIt == undefFunctions.end()) {
        Function *newFun = Function::Create(
                FunctionType::get(type, false),
                Function::LinkageTypes::ExternalLinkage,
                "$undef$" + typeName(type),
                mod);
        setUndef(newFun, mod->getContext());
        FunMapIt = undefFunctions.emplace(type, newFun).first;
    }
    auto newCall = CallInst::Create(FunMapIt->second, {}, "", insertBefore);
    setUndef(newCall, mod->getContext());
    return newCall;
}

std::set<MonoPair<llvm::Function *>> coupleUndefCalls(
        MonoPair<llvm::Module &> modules) {
    std::set<MonoPair<llvm::Function *>> result;
    for (auto &FunFirst : modules.first) {
        if (FunFirst.getMetadata("is_undef")) {
            auto *FunSecond = modules.second.getFunction(FunFirst.getName());
            if (!FunSecond) continue;

            result.emplace(&FunFirst, FunSecond);
        }
    }
    return result;
}
