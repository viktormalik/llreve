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
#include <Helper.h>

llvm::PreservedAnalyses IndependentSimplifyPass::run(
        llvm::Function &Fun,
        llvm::FunctionAnalysisManager &fam) {
    std::vector<llvm::Instruction *> toRemove;
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (auto CallInst = llvm::dyn_cast<llvm::CallInst>(&Instr)) {
                auto CalledFun = CallInst->getCalledFunction();

                // Remove arguments of printk, _dev_info, dev_warn, dev_err,
                // sprintf
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
        llvm::AnalysisManager<llvm::Module, llvm::Function *> &mam,
        llvm::Function *Main) {
    FunMap funAbstractions;
    int i = 0;
    std::vector<llvm::Instruction *> toErase;

    for (auto &Fun : Module) {
        if (!(&Fun == Main || callsTransitively(*Main, Fun)))
            continue;
        for (auto &BB : Fun) {
            for (auto &Instr : BB) {
                if (auto CallInstr = llvm::dyn_cast<llvm::CallInst>(&Instr)) {
                    auto funCalled = getCalledFunction(CallInstr);
                    if (funCalled) continue;
                    auto CalledType = CallInstr->getCalledValue()->getType();
                    if (!CalledType->isPointerTy()) continue;

                    auto FunType = llvm::dyn_cast<llvm::FunctionType>(
                            llvm::dyn_cast<llvm::PointerType>(
                                    CalledType)->getElementType());

                    std::string hash = funHash(CallInstr->getCalledValue());
                    auto funAbstr = funAbstractions.find(hash);
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
                        funAbstractions.try_emplace(hash, newFun);
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

/*
 * A hash that uniquely identifies indirect function or inline asm.
 * It contains the string representing the function type, and for inline asm
 * also assembly params and code.
 */
std::string FunctionAbstractionsGenerator::funHash(llvm::Value *Fun) {
    std::string result = typeName(Fun->getType());
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

std::set<MonoPair<llvm::Function *>> ModuleSimplifier::simplifyModules(
        MonoPair<llvm::Function *> &MainFunctions) {
    runIndependentPasses(First, FirstMain);
    runIndependentPasses(Second, SecondMain);

    llvm::AnalysisManager<llvm::Module, llvm::Function *> mam(false);
    mam.registerPass([] { return FunctionAbstractionsGenerator(); });

    auto abstractionCouples = unifyFunctionAbstractions(
            mam.getResult<FunctionAbstractionsGenerator>(First,
                                                         MainFunctions.first),
            mam.getResult<FunctionAbstractionsGenerator>(Second,
                                                         MainFunctions.second));

    for (auto &FunFirst : First) {
        if (!(&FunFirst == MainFunctions.first ||
              callsTransitively(*MainFunctions.first, FunFirst)))
            continue;
        auto FunSecond = Second.getFunction(FunFirst.getName());
        if (!FunSecond) continue;

        if (!(&*FunSecond == MainFunctions.second||
              callsTransitively(*MainFunctions.second, *FunSecond)))
            continue;

        if (FunFirst.isDeclaration() || FunSecond->isDeclaration()) {
            if (!FunFirst.isDeclaration())
                FunFirst.deleteBody();
            if (!FunSecond->isDeclaration())
                FunSecond->deleteBody();
            continue;
        }

        DifferentialGlobalNumberState gs(&First, &Second);
        DifferentialFunctionComparator fComp(&FunFirst, FunSecond, &gs);
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

        coupledFuns.insert({FirstFun.second, SecondFun->second});

        if (FirstFun.second->getName() != SecondFun->second->getName()) {
            if (!(trySwap(FirstMap, FirstFun.first(),
                          SecondFun->second->getName()) ||
                  trySwap(SecondMap, SecondFun->first(),
                          FirstFun.second->getName()))) {
                FirstFun.second->setName(SecondFun->second->getName());
            }
        }
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

void ModuleSimplifier::runIndependentPasses(llvm::Module &Module, llvm::Function &Main) {
    llvm::FunctionPassManager fpm(false);
    llvm::FunctionAnalysisManager fam(false);
    llvm::PassManager<llvm::Module, llvm::ModuleAnalysisManager,
        llvm::Function &> mpm;
    llvm::ModuleAnalysisManager mam(false);
    llvm::PassBuilder pb;
    pb.registerFunctionAnalyses(fam);
    pb.registerModuleAnalyses(mam);
    // Register and run independent pass
    mpm.addPass(RemoveUnusedReturnValuesPass {});
    fpm.addPass(IndependentSimplifyPass {});
    fpm.addPass(llvm::DCEPass {});
    mpm.run(Module, mam, Main);
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
    mpm.addPass(RemoveLifetimeCallsPass {});
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
        // Get new number for the global value
        result = nextNumber;
        GlobalNumbers.insert({value, nextNumber});

        // Try to find global value with the same name in the other module, if
        // it exists, assign it the same number
        auto otherModule = value->getParent() == First ? Second : First;
        llvm::GlobalValue *otherValue = otherModule->getNamedValue(
                value->getName());
        if (otherValue)
            GlobalNumbers.insert({otherValue, nextNumber});

        nextNumber++;
    } else {
        // If number for the global value exists, return it
        result = number->second;
    }

    return result;
}

llvm::PreservedAnalyses RemoveLifetimeCallsPass::run(
        llvm::Module &Mod,
        llvm::ModuleAnalysisManager &mam) {
    std::vector<llvm::Instruction *> toRemove;
    for (auto &Fun : Mod) {
        for (auto &BB : Fun) {
            for (auto &Instr : BB) {
                if (auto CallInstr = llvm::dyn_cast<llvm::CallInst>(&Instr)) {
                    auto fun = CallInstr->getCalledFunction();
                    if (!fun)
                        continue;
                    auto name = fun->getName();
                    // TODO: this should work with Instr.getIntrinsicID()
                    if (name.find("llvm.lifetime.start") != std::string::npos ||
                        name.find("llvm.lifetime.end") != std::string::npos) {
                        toRemove.push_back(&Instr);
                    }
                }
            }
        }
    }
    for (auto Instr : toRemove)
        Instr->eraseFromParent();
    return llvm::PreservedAnalyses();
}

llvm::PreservedAnalyses RemoveUnusedReturnValuesPass::run(
    llvm::Module &Mod,
    llvm::ModuleAnalysisManager &mam,
    llvm::Function &Main) {
    
    // These attributes are invalid for void functions
    llvm::Attribute::AttrKind badAttributes[] = {
        llvm::Attribute::AttrKind::ByVal, 
        llvm::Attribute::AttrKind::InAlloca,
        llvm::Attribute::AttrKind::Nest,
        llvm::Attribute::AttrKind::NoAlias,
        llvm::Attribute::AttrKind::NoCapture,
        llvm::Attribute::AttrKind::NonNull,
        llvm::Attribute::AttrKind::ReadNone,
        llvm::Attribute::AttrKind::ReadOnly,
        llvm::Attribute::AttrKind::SExt,
        llvm::Attribute::AttrKind::StructRet,
        llvm::Attribute::AttrKind::ZExt,
        llvm::Attribute::AttrKind::Dereferenceable,
        llvm::Attribute::AttrKind::DereferenceableOrNull
    };
    
    // Old functions ought to be deleted after iteration
    std::vector<llvm::Function *> functionsToDelete;
    
    for(llvm::Function &Fun : Mod) {
        if(Fun.getLinkage() != llvm::GlobalValue::LinkageTypes::ExternalLinkage)
            continue;
        
        if(Fun.getReturnType()->isVoidTy())
            continue;
        
        if (!callsTransitively(Main, Fun))
            continue;
        
        bool can_replace = true;
#ifdef DEBUG
        llvm::errs() << "Changing function: "<< Fun.getName() << " to void\n";
#endif
        for(llvm::Use &U : Fun.uses()) {
            // Figure out whether the return value is used after each call
            
            if(auto CI = llvm::dyn_cast<llvm::CallInst>(U.getUser())) {
                if(CI->getCalledFunction() != &Fun)
                    // Different function is called, Fun is an argument
                    can_replace = false; 
#ifdef DEBUG
                CI->print(llvm::errs(), false); llvm::errs() << "\n";
                for(llvm::Use &UU : CI->uses()) {
                    llvm::errs() << "  "; UU.getUser()->print(llvm::errs(), 
                                                              false); 
                    llvm::errs() << "\n";
                }
#endif
                if(!CI->use_empty())
                    // The return value is actually used
                    can_replace = false; 
            } else if(auto II = llvm::dyn_cast<llvm::InvokeInst>(U.getUser())) {
                if(II->getCalledFunction() != &Fun)
                    // Different function is called, Fun is an argument
                    can_replace = false; 
#ifdef DEBUG
                II->print(llvm::errs(), false); llvm::errs() << "\n";
                for(llvm::Use &UU : II->uses()) {
                    llvm::errs() << "  "; UU.getUser()->print(llvm::errs(), 
                                                              false); 
                    llvm::errs() << "\n";
                }
#endif
                if(!II->use_empty()) 
                    // The return value is actually used
                    can_replace = false;
            } else
                // The function is used somewhere as an argument, therefore
                // it ought not to be replaced
                can_replace = false;
        }

        if(can_replace) {  
            // Create the header of the new function
            std::vector<llvm::Type *> FAT_New (
                Fun.getFunctionType()->param_begin(),
                Fun.getFunctionType()->param_end());
            llvm::FunctionType *FT_New = llvm::FunctionType::get(
                llvm::Type::getVoidTy(Fun.getContext()),
                FAT_New, Fun.isVarArg());
            llvm::Function *Fun_New = llvm::Function::Create(FT_New,
                                                             Fun.getLinkage(),
                                                             Fun.getName(),
                                                             Fun.getParent());
            
            // Copy the attributes from the old function and delete the ones 
            // related to the return value
            Fun_New->copyAttributesFrom(&Fun);
            for(llvm::Attribute::AttrKind AK : badAttributes) {
                Fun_New->removeAttribute(llvm::AttributeList::ReturnIndex, AK);
                Fun_New->removeAttribute(llvm::AttributeList::FunctionIndex, 
                                         AK);
            }
            Fun_New->takeName(&Fun);
            
            // Set the names of all arguments of the new function
            for(llvm::Function::arg_iterator AI = Fun.arg_begin(), 
                AE = Fun.arg_end(), NAI = Fun_New->arg_begin();
                AI != AE; 
                ++AI, ++NAI) {
                NAI->takeName(AI); 
            }
            
            // Copy the function body (currently not used, because function with
            // a body are ignored)
            Fun_New->getBasicBlockList().splice(Fun_New->begin(),
                                                Fun.getBasicBlockList());
            
            // Replace return instructions on ends of basic blocks with ret void
            // (currently not used because function with a body are ignored)
            for(llvm::BasicBlock &B : *Fun_New)
                if(llvm::dyn_cast<llvm::ReturnInst>(B.getTerminator())) {
                    B.getInstList().pop_back();
                    llvm::ReturnInst *Term_New = llvm::ReturnInst::Create(
                        B.getContext());
                    B.getInstList().push_back(Term_New);
                }
            
            // Replace all uses of the old arguments
            for(llvm::Function::arg_iterator I = Fun.arg_begin(),
                E = Fun.arg_end(), NI = Fun_New->arg_begin();
                I != E;
                ++I, ++NI) {
                I->replaceAllUsesWith(NI); 
            }
            
            // For call or invoke instructions a new instruction has to be
            // created and the old one replaced
            for(llvm::Use &U : Fun.uses()) {
                if(llvm::CallInst *CI = 
                    llvm::dyn_cast<llvm::CallInst>(U.getUser())) {
                    // First copy all arguments to an array 
                    // and create the new instruction
                    std::vector<llvm::Value *> Args;
                    
                    for(llvm::Value *A : CI->arg_operands()) {
                        Args.push_back(A);
                    }
                    
                    llvm::ArrayRef<llvm::Value *> Args_AR(Args);
                    
                    // Insert the new instruction next to the old one
                    llvm::CallInst *CI_New = llvm::CallInst::Create(Fun_New,
                                                                    Args_AR, 
                                                                    "", CI);
                
                    // Copy additional properties
                    CI_New->setAttributes(CI->getAttributes());
                    for(llvm::Attribute::AttrKind AK : badAttributes) { 
                        // Remove incompatibile attributes
                        CI_New->removeAttribute(
                            llvm::AttributeList::ReturnIndex, AK);
                        CI_New->removeAttribute(
                            llvm::AttributeList::FunctionIndex, AK);
                    }
                    CI_New->setCallingConv(CI->getCallingConv());
                    if(CI->isTailCall())
                        CI_New->setTailCall();
#ifdef DEBUG
                    llvm::errs() << "Replacing :" << *CI << " with " << *CI_New;
                    llvm::errs() << "\n";
#endif
                     // Erase the old instruction
                    CI->eraseFromParent();
                } else if(llvm::InvokeInst *II = 
                    llvm::dyn_cast<llvm::InvokeInst>(U.getUser())) {
                    // First copy all arguments to an array and create 
                    // the new instruction
                    std::vector<llvm::Value *> Args;
                    
                    for(llvm::Value *A : II->arg_operands()) {
                        Args.push_back(A);
                    }
                    
                    llvm::ArrayRef<llvm::Value *> Args_AR(Args);
                    
                    // Insert the new instruction next to the old one
                    llvm::InvokeInst *II_New = llvm::InvokeInst::Create(
                                                        Fun_New,
                                                        II->getNormalDest(),
                                                        II->getUnwindDest(),
                                                        Args_AR, "", II);
                    
                    // Copy additional properties
                    II_New->setAttributes(II->getAttributes());
                    for(llvm::Attribute::AttrKind AK : badAttributes) {
                        // Remove incompatibile attributes
                        II_New->removeAttribute(
                            llvm::AttributeList::ReturnIndex, AK);
                        II_New->removeAttribute(
                            llvm::AttributeList::FunctionIndex, AK);
                    }
                    II_New->setCallingConv(II->getCallingConv());
#ifdef DEBUG
                    llvm::errs() << "Replacing :" << *II << " with " << *II_New;
                    llvm::errs() << "\n";
#endif        
                    // Erase the old instruction
                    II->eraseFromParent();
                }
            }
#ifdef DEBUG
            Fun_New->print(llvm::errs()); llvm::errs() << "\n";
#endif
            // Delete function after iteration
            functionsToDelete.push_back(&Fun);
        }
    }
    
    // Delete replaced functions
    for(llvm::Function *F : functionsToDelete)
        F->removeFromParent();
    
    return llvm::PreservedAnalyses();
}

int DifferentialFunctionComparator::cmpValues(const llvm::Value *L,
                                              const llvm::Value *R) const {
    const llvm::GEPOperator *GEPL = llvm::dyn_cast<llvm::GEPOperator>(L);
    const llvm::GEPOperator *GEPR = llvm::dyn_cast<llvm::GEPOperator>(R);
    if (GEPL && GEPR)
        return cmpGEPs(GEPL, GEPR);
    else
        return llvm::FunctionComparator::cmpValues(L, R);
}

int DifferentialFunctionComparator::cmpGEPs(
        const llvm::GEPOperator *GEPL,
        const llvm::GEPOperator *GEPR) const {
    unsigned int ASL = GEPL->getPointerAddressSpace();
    unsigned int ASR = GEPR->getPointerAddressSpace();

    if (int Res = cmpNumbers(ASL, ASR))
        return Res;

    // When we have target data, we can reduce the GEP down to the value in
    // bytes added to the address.
    const llvm::DataLayout &DL = FnL->getParent()->getDataLayout();
    unsigned BitWidth = DL.getPointerSizeInBits(ASL);
    llvm::APInt OffsetL(BitWidth, 0), OffsetR(BitWidth, 0);
    if (GEPL->accumulateConstantOffset(DL, OffsetL) &&
        GEPR->accumulateConstantOffset(DL, OffsetR)) {
        if (auto GEPLInstr = llvm::dyn_cast<llvm::GetElementPtrInst>(GEPL)) {
            auto GEPRInstr = llvm::dyn_cast<llvm::GetElementPtrInst>(GEPR);
            auto TypeItL = llvm::gep_type_begin(GEPLInstr);
            auto TypeItR = llvm::gep_type_begin(GEPRInstr);
            auto ix = GEPL->idx_begin();
            for (unsigned i = 0; i < GEPL->getNumIndices();
                 ++i, ++ix, TypeItL++, TypeItR++) {
                if (auto idxMD = GEPLInstr->getMetadata(
                        "idx_align_" + std::to_string(i))) {
                    // Add alignment of the offset computed from debug info
                    auto *ixAlign = llvm::dyn_cast<llvm::ConstantInt>(
                            llvm::dyn_cast<llvm::ConstantAsMetadata>(
                                    idxMD->getOperand(0))->getValue());

                    // Get value of the old offset from the current index
                    auto oldOffset = DL.getStructLayout(
                            TypeItL.getStructType())->getElementOffset(
                            llvm::dyn_cast<llvm::ConstantInt>(
                                    ix)->getZExtValue());
                    // Get value of the new offset (from metadata)
                    auto newOffset = DL.getStructLayout(
                            TypeItR.getStructType())->getElementOffset(
                            ixAlign->getValue().getZExtValue());
                    OffsetL += llvm::APInt(BitWidth, newOffset - oldOffset,
                                           true);
                }
            }
        }
        return cmpAPInts(OffsetL, OffsetR);
    }
    if (int Res = cmpTypes(GEPL->getSourceElementType(),
                           GEPR->getSourceElementType()))
        return Res;

    if (int Res = cmpNumbers(GEPL->getNumOperands(), GEPR->getNumOperands()))
        return Res;

    for (unsigned i = 0, e = GEPL->getNumOperands(); i != e; ++i) {
        if (int Res = cmpValues(GEPL->getOperand(i), GEPR->getOperand(i)))
            return Res;
    }

    return 0;
}
