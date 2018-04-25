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
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Passes/PassBuilder.h>
#include <Opts.h>

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
                } else if (CalledFun && (CalledFun->getName() == "_dev_info" ||
                                         CalledFun->getName() == "dev_warn" ||
                                         CalledFun->getName() == "dev_err" ||
                                         CalledFun->getName() == "sprintf")) {
                    auto Op0Type = llvm::dyn_cast<llvm::PointerType>(
                            CallInst->getOperand(0)->getType());
                    auto Op1Type = llvm::dyn_cast<llvm::PointerType>(
                            CallInst->getOperand(1)->getType());
                    auto newCall = llvm::CallInst::Create(
                            CalledFun,
                            {llvm::ConstantPointerNull::get(Op0Type),
                             llvm::ConstantPointerNull::get(Op1Type)},
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

std::set<MonoPair<llvm::Function *>> ModuleSimplifier::simplifyModules() {
    runIndependentPasses(First);
    runIndependentPasses(Second);

    llvm::ModuleAnalysisManager mam(false);
    mam.registerPass([] { return FunctionAbstractionsGenerator(); });

    auto abstractionCouples = unifyFunctionAbstractions(
            mam.getResult<FunctionAbstractionsGenerator>(First),
            mam.getResult<FunctionAbstractionsGenerator>(Second));

    for (auto &FunFirst : First) {
        auto FunSecond = Second.getFunction(FunFirst.getName());
        if (!FunSecond) continue;

        if (FunFirst.isDeclaration() || FunSecond->isDeclaration()) {
            if (!FunFirst.isDeclaration())
                FunFirst.deleteBody();
            if (!FunSecond->isDeclaration())
                FunSecond->deleteBody();
            continue;
        }

        DifferentialGlobalNumberState gs(&First, &Second);
        llvm::FunctionComparator fComp(&FunFirst, FunSecond, &gs);
        if (fComp.compare() == 0) {
#ifdef DEBUG
            llvm::errs() << "Function " << FunFirst.getName()
                         << " is same in both modules\n";
#endif
            FunFirst.deleteBody();
            FunSecond->deleteBody();
        }
    }

    // Inline functions called by main in both modules - this will simplify the
    // solving
    inlineCalled(First, FirstMain);
    inlineCalled(Second, SecondMain);

    // This function returns pairs of new functions that must be coupled. It is
    // necessary when using function couplings defined at the command line
    return abstractionCouples;
}

/*
 * Makes sure that functions implementing same abstractions are called the same
 * in both modules.
 */
std::set<MonoPair<llvm::Function *>>
ModuleSimplifier::unifyFunctionAbstractions(
        FunctionAbstractionsGenerator::FunMap &FirstMap,
        FunctionAbstractionsGenerator::FunMap &SecondMap) {
    std::set<MonoPair<llvm::Function *>> coupledFuns;
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

        coupledFuns.insert({FirstFun.second, SecondFun->second});
    }
    return coupledFuns;
}

bool ModuleSimplifier::trySwap(FunctionAbstractionsGenerator::FunMap &Map,
                               const std::string srcHash,
                               const std::string destName) {
    for (auto &Fun : Map) {
        if (Fun.second->getName() == destName) {
            const std::string srcName = Map.find(srcHash)->second->getName();
            Map.find(srcHash)->second->setName("$tmpName");
            Fun.second->setName(srcName);
            Map.find(srcHash)->second->setName(destName);
            return true;
        }
    }
    return false;
}

void ModuleSimplifier::runIndependentPasses(llvm::Module &Module) {
    llvm::FunctionPassManager fpm(false);
    llvm::FunctionAnalysisManager fam(false);
    llvm::PassBuilder pb;
    pb.registerFunctionAnalyses(fam);
    // Register and run independent pass
    fpm.addPass(IndependentSimplifyPass {});
    fpm.addPass(llvm::DCEPass {});
    for (auto &Fun : Module)
        fpm.run(Fun, fam);
}

void ModuleSimplifier::inlineCalled(llvm::Module &Mod, llvm::Function &Fun) {
    markCalleesAlwaysInline(Fun);

    llvm::ModulePassManager mpm(false);
    llvm::ModuleAnalysisManager mam(false);
    llvm::PassBuilder pb;
    pb.registerModuleAnalyses(mam);
    mpm.addPass(llvm::AlwaysInlinerPass {});
    mpm.run(Mod, mam);

    llvm::errs() << "Function " << Fun.getName() << " after inlining:\n";
    Fun.dump();
}

void ModuleSimplifier::markCalleesAlwaysInline(llvm::Function &Fun) {
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (auto CallInstr = llvm::dyn_cast<llvm::CallInst>(&Instr)) {
                auto CalledFun = CallInstr->getCalledFunction();
                CallInstr->dump();
                if (!CalledFun || CalledFun->isDeclaration() ||
                    llreve::opts::isLlreveIntrinsic(*CalledFun))
                    continue;

                if (!CalledFun->hasFnAttribute(
                        llvm::Attribute::AttrKind::AlwaysInline)) {
                    CalledFun->addFnAttr(
                            llvm::Attribute::AttrKind::AlwaysInline);
                    markCalleesAlwaysInline(*CalledFun);
                }
            }
        }
    }
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
