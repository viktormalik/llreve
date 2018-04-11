/*
 * Contains classes used to simplify modules to be compared.
 *
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#pragma once

#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Transforms/Utils/FunctionComparator.h>
#include <set>
#include "MonoPair.h"

/*
 * Pass doing simplification that is independent for each function.
 * Currently removes arguments of printk function calls.
 */
class IndependentSimplifyPass
        : public llvm::PassInfoMixin<IndependentSimplifyPass> {
public:
    llvm::PreservedAnalyses run(llvm::Function &Fun,
                                llvm::FunctionAnalysisManager &fam);
};

/*
 * Generates abstractions for indirect function calls and for inline assemblies.
 */
class FunctionAbstractionsGenerator
        : public llvm::AnalysisInfoMixin<FunctionAbstractionsGenerator> {
public:
    typedef llvm::StringMap<llvm::Function *> FunMap;
    using Result = FunMap;

    Result run(llvm::Module &Module, llvm::ModuleAnalysisManager &mam);

protected:
    std::string FunHash(llvm::Value *Fun);
    std::string abstractionPrefix(llvm::Value *Fun);

private:
    friend llvm::AnalysisInfoMixin<FunctionAbstractionsGenerator>;
    static llvm::AnalysisKey Key;
};

/*
 * Extension of llvm::GlobalNumberState.
 * Makes sure that globals in different modules with same name get the same
 * number.
 */
class DifferentialGlobalNumberState : public llvm::GlobalNumberState {
    using ValueNumberMap = llvm::ValueMap<llvm::GlobalValue *, uint64_t>;
    ValueNumberMap GlobalNumbers;

    llvm::Module *First;
    llvm::Module *Second;

    u_int64_t nextNumber = 0;
public:
    DifferentialGlobalNumberState(llvm::Module *first, llvm::Module *second) :
            First(first), Second(second) {
    }

    uint64_t getNumber(llvm::GlobalValue *value);

    void clear() {
        GlobalNumbers.clear();
    }
};

/*
 * Simplification of modules.
 * Removes bodies of functions that are exactly the same between modules.
 * Also unifies abstractions of indirect calls and inline assemblies.
 */
class ModuleSimplifier {
public:
    ModuleSimplifier(llvm::Module &First, llvm::Module &Second,
                     llvm::Function &FirstMain, llvm::Function &SecondMain)
            : First(First), Second(Second),
              FirstMain(FirstMain), SecondMain(SecondMain) {}

    std::set<MonoPair<llvm::Function *>> simplifyModules();

protected:
    llvm::Module &First;
    llvm::Module &Second;

    llvm::Function &FirstMain;
    llvm::Function &SecondMain;

    void runIndependentPasses(llvm::Module &Module);

    void inlineCalled(llvm::Module &Mod, llvm::Function &Fun);
    void markCalleesAlwaysInline(llvm::Function &Fun);

    std::set<MonoPair<llvm::Function *>> unifyFunctionAbstractions(
            FunctionAbstractionsGenerator::FunMap &FirstMap,
            FunctionAbstractionsGenerator::FunMap &SecondMap);

    bool trySwap(FunctionAbstractionsGenerator::FunMap &Map,
                 const std::string srcHash,
                 const std::string destName);
};

const std::string typeToStr(const llvm::Type *Type);