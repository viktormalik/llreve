/*
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#include "ModuleSimplifier.h"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Passes/PassBuilder.h>

llvm::PreservedAnalyses IndependentSimplifyPass::run(
        llvm::Function &Fun,
        llvm::FunctionAnalysisManager &fam) {
    std::vector<llvm::Instruction *> toRemove;
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (auto CallInst = llvm::dyn_cast<llvm::CallInst>(&Instr)) {
                auto CalledFun = CallInst->getCalledFunction();

                // Remove arguments of printk
                if (CalledFun && CalledFun->getName() == "printk") {
                    auto OpType = llvm::dyn_cast<llvm::PointerType>(
                            CallInst->getOperand(0)->getType());
                    auto newCall = llvm::CallInst::Create(
                            CalledFun,
                            {llvm::ConstantPointerNull::get(OpType)},
                            "", &Instr);
                    CallInst->replaceAllUsesWith(newCall);
                    toRemove.push_back(&Instr);
                }
            }
        }
    }
    for (auto i : toRemove)
        i->eraseFromParent();
    toRemove.clear();

    return llvm::PreservedAnalyses();
}

llvm::AnalysisKey FunctionAbstractionsGenerator::Key;

/*
 * Creates a new function for each type of function that is called indirectly
 * or as an inline assembly.
 */
FunctionAbstractionsGenerator::Result FunctionAbstractionsGenerator::run(
        llvm::Module &Module,
        llvm::ModuleAnalysisManager &mam) {
    FunMap funAbstractions;
    int i = 0;
    std::vector<llvm::Instruction *> toErase;

    for (auto &Fun : Module) {
        for (auto &BB : Fun) {
            for (auto &Instr : BB) {
                if (auto CallInstr = llvm::dyn_cast<llvm::CallInst>(&Instr)) {
                    auto funCalled = CallInstr->getCalledFunction();
                    if (funCalled) continue;
                    auto CalledType = CallInstr->getCalledValue()->getType();
                    if (!CalledType->isPointerTy()) continue;

                    auto FunType = llvm::dyn_cast<llvm::FunctionType>(
                            llvm::dyn_cast<llvm::PointerType>(
                                    CalledType)->getElementType());

                    std::string funHash = FunHash(CallInstr->getCalledValue());
                    auto funAbstr = funAbstractions.find(funHash);
                    llvm::Function *newFun;

                    if (funAbstr == funAbstractions.end()) {
                        std::vector<llvm::Type *> newParamTypes;
                        newParamTypes = FunType->params();
                        if (!CallInstr->isInlineAsm())
                            newParamTypes.push_back(CalledType);
                        auto newFunType = llvm::FunctionType::get(
                                FunType->getReturnType(), newParamTypes, false);

                        const std::string funName =
                                abstractionPrefix(CallInstr->getCalledValue()) +
                                std::to_string(i++);
                        newFun = llvm::Function::Create(
                                newFunType,
                                llvm::Function::ExternalLinkage,
                                funName, &Module);
                        funAbstractions.try_emplace(funHash, newFun);
                    } else {
                        newFun = funAbstr->second;
                    }

                    std::vector<llvm::Value *> args;
                    for (auto &a : CallInstr->arg_operands()) {
                        if (auto argVal = llvm::dyn_cast<llvm::Value>(&a))
                            args.push_back(argVal);
                    }
                    if (!CallInstr->isInlineAsm())
                        args.push_back(CallInstr->getCalledValue());
                    auto newCall = llvm::CallInst::Create(newFun, args, "",
                                                          CallInstr);
                    newCall->dump();
                    CallInstr->replaceAllUsesWith(newCall);
                    toErase.push_back(&Instr);
                }
            }
            for (auto &I : toErase)
                I->eraseFromParent();
            toErase.clear();
        }
    }
    return funAbstractions;
}

std::string FunctionAbstractionsGenerator::FunHash(llvm::Value *Fun) {
    std::string result = typeToStr(Fun->getType());
    if (auto inlineAsm = llvm::dyn_cast<llvm::InlineAsm>(Fun)) {
        result += "$" + inlineAsm->getAsmString() + "$" +
                  inlineAsm->getConstraintString();
    }
    return result;
}

std::string FunctionAbstractionsGenerator::abstractionPrefix(llvm::Value *Fun) {
    if (llvm::isa<llvm::InlineAsm>(Fun))
        return "llreve__inlineasm$";
    else
        return "llreve__indirect$";
}

const std::string typeToStr(const llvm::Type *Type) {
    std::string result;
    llvm::raw_string_ostream rso(result);
    Type->print(rso);
    return rso.str();
}

void ModuleSimplifier::simplifyModules() {
    llvm::FunctionPassManager fpm(false);
    llvm::FunctionAnalysisManager fam(false);
    llvm::PassBuilder pb;
    pb.registerFunctionAnalyses(fam);
    fpm.addPass(IndependentSimplifyPass {});
    fpm.addPass(llvm::DCEPass {});
    for (auto &Fun : First)
        fpm.run(Fun, fam);
    for (auto &Fun : Second)
        fpm.run(Fun, fam);

    llvm::ModuleAnalysisManager mam(false);
    mam.registerPass([] { return FunctionAbstractionsGenerator (); });

    unifyFunctionAbstractions(
            mam.getResult<FunctionAbstractionsGenerator>(First),
            mam.getResult<FunctionAbstractionsGenerator>(Second));

    for (auto &FunFirst : First) {
        auto FunSecond = Second.getFunction(FunFirst.getName());
        if (!FunSecond) continue;

        if (FunFirst.isDeclaration() || FunSecond->isDeclaration())
            continue;

        DifferentialGlobalNumberState gs(&First, &Second);
        llvm::FunctionComparator fComp(&FunFirst, FunSecond, &gs);
        if (fComp.compare() == 0) {
            FunFirst.deleteBody();
            FunSecond->deleteBody();
        }
    }
}

/*
 * Makes sure that functions implementing same abstractions are called the same
 * in both modules.
 */
void ModuleSimplifier::unifyFunctionAbstractions(
        FunctionAbstractionsGenerator::FunMap &FirstMap,
        FunctionAbstractionsGenerator::FunMap &SecondMap) {
    for (auto &FirstFun : FirstMap) {
        auto SecondFun = SecondMap.find(FirstFun.first());

        if (SecondFun == SecondMap.end())
            continue;

        if (FirstFun.second->getName() == SecondFun->second->getName())
            continue;

        if (!(trySwap(FirstMap, FirstFun.first(),
                      SecondFun->second->getName()) ||
              trySwap(SecondMap, SecondFun->first(),
                      FirstFun.second->getName()))) {
            FirstFun.second->setName(SecondFun->second->getName());
        }
    }
}

bool ModuleSimplifier::trySwap(FunctionAbstractionsGenerator::FunMap &Map,
                               const llvm::StringRef &srcHash,
                               const llvm::StringRef &destName) {
    for (auto &Fun : Map) {
        if (Fun.second->getName() == destName) {
            auto tmp = Fun.second->getName();
            Fun.second->setName(Map.find(srcHash)->second->getName());
            Map.find(srcHash)->second->setName(tmp);
            return true;
        }
    }
    return false;
}

uint64_t DifferentialGlobalNumberState::getNumber(llvm::GlobalValue *value) {
    auto number = GlobalNumbers.find(value);
    u_int64_t result;
    if (number == GlobalNumbers.end()) {
        result = nextNumber;
        GlobalNumbers.insert({value, nextNumber});

        auto otherModule = value->getParent() == First ? Second : First;
        llvm::GlobalValue *otherValue;
        otherValue = otherModule->getGlobalVariable(value->getName());
        if (!otherValue)
            otherValue = otherModule->getFunction(value->getName());

        if (otherValue)
            GlobalNumbers.insert({otherValue, nextNumber});

        nextNumber++;
    } else {
        result = number->second;
    }

    return result;
}
