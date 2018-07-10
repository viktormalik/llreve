#include "FixedAbstraction.h"

#include "Compat.h"
#include "Helper.h"
#include "Invariant.h"
#include "MarkAnalysis.h"
#include "Opts.h"

#include <set>
#include <DebugInfo.h>

using std::make_unique;
using std::set;
using std::string;
using std::vector;

using namespace smt;
using namespace llreve::opts;

std::set<VarArgs> getVarArgLists(const llvm::Function &fun) {
    std::set<VarArgs> varArgLists;
    for (auto User : fun.users()) {
        if (const auto callInst = llvm::dyn_cast<llvm::CallInst>(User)) {
            VarArgs varArgs;
            for (unsigned i = fun.getFunctionType()->getNumParams();
                 i < callInst->getNumArgOperands(); ++i) {
                varArgs.argTypes.push_back(
                        callInst->getArgOperand(i)->getType());
            }
            varArgLists.insert(varArgs);
        } else {
            logWarningData("Unsupported use of function\n", *User);
        }
    }
    return varArgLists;
}

static void appendExternInputArgs(const llvm::Function &fun, Program progIndex,
                                 const VarArgs &varArgs,
                                 std::vector<SortedVar> &args) {
    auto funArgs = functionArgs(fun);
    auto varArgVars = varArgs.toSortedVars();
    funArgs.insert(funArgs.end(), varArgVars.begin(), varArgVars.end());
    args.insert(args.end(), funArgs.begin(), funArgs.end());
    if (SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
        args.emplace_back(heapName(progIndex), memoryType());
    }
    if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
        args.emplace_back(stackPointerName(progIndex), pointerType());
        args.emplace_back(stackName(progIndex), memoryType());
    }
}

static std::vector<SortedVar> externDeclArgs(
        const llvm::Function &fun1,
        const llvm::Function &fun2,
        const VarArgs& varArgs) {
    std::vector<SortedVar> args;
    appendExternInputArgs(fun1, Program::First, varArgs, args);
    appendExternInputArgs(fun2, Program::Second, varArgs, args);
    auto resultValues = getMutualResultValues(
        resultName(Program::First), fun1, resultName(Program::Second), fun2);
    args.insert(args.end(), resultValues.begin(), resultValues.end());
    return args;
}

void externDeclarations(const llvm::Module &mod1, const llvm::Module &mod2,
                        std::vector<SharedSMTRef> &declarations,
                        std::multimap<string, string> funCondMap) {
    for (const auto &functionPair :
         SMTGenerationOpts::getInstance().CoupledFunctions) {
        if (hasMutualFixedAbstraction(functionPair)) {
            auto isCalledFromMain =
                    callsTransitively(
                        *SMTGenerationOpts::getInstance().MainFunctions.first,
                        *functionPair.first) &&
                    callsTransitively(
                        *SMTGenerationOpts::getInstance().MainFunctions.second,
                        *functionPair.second);
            if (!isCalledFromMain) continue;

            if (SMTGenerationOpts::getInstance().DisableAutoAbstraction) {
                const auto assumeEquivalent =
                    SMTGenerationOpts::getInstance().AssumeEquivalent;
                if (assumeEquivalent.find(functionPair) !=
                    assumeEquivalent.end()) {
                    auto decls = equivalentExternDecls(
                        *functionPair.first, *functionPair.second, funCondMap);
                    declarations.insert(declarations.end(),
                                        std::make_move_iterator(decls.begin()),
                                        std::make_move_iterator(decls.end()));
                } else {
                    auto decls = notEquivalentExternDecls(*functionPair.first,
                                                          *functionPair.second);
                    declarations.insert(declarations.end(),
                                        std::make_move_iterator(decls.begin()),
                                        std::make_move_iterator(decls.end()));
                }
            } else {
                auto decls = equivalentExternDecls(
                    *functionPair.first, *functionPair.second, funCondMap);
                declarations.insert(declarations.end(),
                                    std::make_move_iterator(decls.begin()),
                                    std::make_move_iterator(decls.end()));
            }
        }
    }
    for (auto &fun1 : mod1) {
        if (hasFixedAbstraction(fun1) && !isLlreveIntrinsic(fun1) &&
            !isDebugInfo(fun1)) {
            auto decls = externFunDecl(fun1, Program::First);
            declarations.insert(declarations.end(),
                                std::make_move_iterator(decls.begin()),
                                std::make_move_iterator(decls.end()));
        }
    }
    for (auto &fun2 : mod2) {
        if (hasFixedAbstraction(fun2) && !isLlreveIntrinsic(fun2) &&
            !isDebugInfo(fun2)) {
            auto decls = externFunDecl(fun2, Program::Second);
            declarations.insert(declarations.end(),
                                std::make_move_iterator(decls.begin()),
                                std::make_move_iterator(decls.end()));
        }
    }
}

static SMTRef equalOutputs(const llvm::Function &Fun1,
                           const llvm::Function &Fun2,
                           std::multimap<string, string> funCondMap) {
    auto return1 = SortedVar(resultName(Program::First),
                                 llvmType(Fun1.getReturnType()));
    auto return2 = SortedVar(resultName(Program::Second),
                                 llvmType(Fun2.getReturnType()));
    std::vector<SharedSMTRef> equalClauses;
    equalClauses.emplace_back(
            resultEquality(typedVariableFromSortedVar(return1),
                           typedVariableFromSortedVar(return2)));
    if (SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
        equalClauses.emplace_back(
            makeOp("=", memoryVariable(heapResultName(Program::First)),
                   memoryVariable(heapResultName(Program::Second))));
    }
    if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
        equalClauses.emplace_back(
            makeOp("=", memoryVariable(stackResultName(Program::First)),
                   memoryVariable(stackResultName(Program::Second))));
    }
    SMTRef body = make_unique<Op>("and", std::move(equalClauses));

    std::vector<SharedSMTRef> equalOut;
    // TODO remove dependency on a single name
    auto range = funCondMap.equal_range(Fun1.getName());
    for (auto i = range.first; i != range.second; ++i) {
        equalOut.push_back(stringExpr(i->second));
    }
    if (!equalOut.empty()) {
        equalOut.push_back(std::move(body));
        body = make_unique<Op>("and", equalOut);
    }
    return body;
}

static SMTRef equalInputs(const llvm::Function &fun1,
                          const llvm::Function &fun2,
                          const VarArgs &varArgs) {
    std::vector<SharedSMTRef> equal;
    auto funArgs1 = functionArgs(fun1);
    auto varArgs1 = varArgs.toSortedVars();
    funArgs1.insert(funArgs1.end(), varArgs1.begin(), varArgs1.end());
    auto funArgs2 = functionArgs(fun2);
    auto varArgs2 = varArgs.toSortedVars();
    funArgs2.insert(funArgs2.end(), varArgs2.begin(), varArgs2.end());
    assert(funArgs1.size() == funArgs2.size());
    std::transform(funArgs1.begin(), funArgs1.end(), funArgs2.begin(),
                   std::back_inserter(equal),
                   [](const auto &var1, const auto &var2) {
                       return makeOp("=", typedVariableFromSortedVar(var1),
                                     typedVariableFromSortedVar(var2));
                   });
    if (SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
        SharedSMTRef heapInEqual =
            makeOp("=", memoryVariable(heapName(Program::First)),
                   memoryVariable(heapName(Program::Second)));
        equal.push_back(heapInEqual);
    }
    if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
        SharedSMTRef stackPtrEqual =
            makeOp("=", make_unique<TypedVariable>(
                            stackPointerName(Program::First), pointerType()),
                   make_unique<TypedVariable>(stackPointerName(Program::Second),
                                              pointerType()));
        SharedSMTRef stackEqual =
            makeOp("=", memoryVariable(stackName(Program::First)),
                   memoryVariable(stackName(Program::Second)));
        equal.emplace_back(std::move(stackPtrEqual));
        equal.emplace_back(std::move(stackEqual));
    }
    return make_unique<Op>("and", equal);
}

static SMTRef equalHeap() {
    std::vector<SharedSMTRef> equal;
    equal.push_back(makeOp("=", memoryVariable(heapName(Program::First)),
                           memoryVariable(heapResultName(Program::First))));
    equal.push_back(makeOp("=", memoryVariable(heapName(Program::Second)),
                           memoryVariable(heapResultName(Program::Second))));
    return make_unique<Op>("and", equal);
}

std::vector<std::unique_ptr<smt::SMTExpr>>
equivalentExternDecls(const llvm::Function &fun1, const llvm::Function &fun2,
                      std::multimap<string, string> funCondMap) {
    vector<std::unique_ptr<smt::SMTExpr>> declarations;
    auto varArgSets1 = getVarArgLists(fun1);
    auto varArgSets2 = getVarArgLists(fun2);
    for (const auto varArgs : varArgSets1) {
        if (varArgSets2.find(varArgs) == varArgSets2.end())
            continue;

        vector<SortedVar> args = externDeclArgs(fun1, fun2, varArgs);
        std::string funName =
            invariantName(ENTRY_MARK, ProgramSelection::Both,
                          fun1.getName().str() + "^" + fun2.getName().str(),
                          InvariantAttr::NONE, &varArgs);

        SMTRef eqOutputs = equalOutputs(fun1, fun2, funCondMap);
        SMTRef eqInputs = equalInputs(fun1, fun2, varArgs);
        SMTRef body = makeOp("=>", std::move(eqInputs), std::move(eqOutputs));

        if (fun1.getMetadata("is_undef")) {
            SMTRef eqHeap = equalHeap();
            body = makeOp("and", std::move(body), std::move(eqHeap));
        }

        auto mainInv =
            make_unique<FunDef>(funName, args, boolType(), std::move(body));
        declarations.push_back(std::move(mainInv));
    }
    return declarations;
}

std::vector<std::unique_ptr<smt::SMTExpr>>
notEquivalentExternDecls(const llvm::Function &fun1,
                         const llvm::Function &fun2) {
    vector<std::unique_ptr<smt::SMTExpr>> declarations;
    auto varArgLists1 = getVarArgLists(fun1);
    auto varArgLists2 = getVarArgLists(fun2);
    for (const auto varArgs : varArgLists1) {
        if (varArgLists2.find(varArgs) == varArgLists2.end())
            continue;

        vector<SortedVar> args = externDeclArgs(fun1, fun2, varArgs);
        std::string funName =
            invariantName(ENTRY_MARK, ProgramSelection::Both,
                          fun1.getName().str() + "^" + fun2.getName().str(),
                          InvariantAttr::NONE, &varArgs);
        SMTRef body;
        if (fun1.getMetadata("is_undef")) {
            body = equalHeap();
        } else {
            body = make_unique<ConstantBool>(true);
        }
        auto mainInv = make_unique<FunDef>(funName, args, boolType(),
                                           std::move(body));
        declarations.push_back(std::move(mainInv));
    }
    return declarations;
}

std::vector<std::unique_ptr<smt::SMTExpr>>
externFunDecl(const llvm::Function &fun, Program program) {
    std::vector<std::unique_ptr<smt::SMTExpr>> decls;
    auto varArgLists = getVarArgLists(fun);
    for (auto varArgList : varArgLists) {
        std::vector<SortedVar> args = functionArgs(fun);
        std::vector<SortedVar> varArgs = varArgList.toSortedVars();
        args.insert(args.end(), varArgs.begin(), varArgs.end());
        if (SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
            args.push_back(SortedVar("HEAP", memoryType()));
        }
        if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
            args.emplace_back("SP", pointerType());
            args.emplace_back("STACK", memoryType());
        }
        auto resultValues = getResultValues(program, resultName(program), fun);
        args.insert(args.end(), resultValues.begin(), resultValues.end());
        std::string funName =
            invariantName(ENTRY_MARK, asSelection(program), fun.getName().str(),
                          InvariantAttr::NONE, &varArgList);
        SMTRef body;
        if (fun.getMetadata("is_undef") &&
            SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
            body = makeOp("=",
                          memoryVariable("HEAP"),
                          memoryVariable(heapResultName(program)));
        } else {
            body = make_unique<ConstantBool>(true);
        }
        decls.push_back(
            make_unique<FunDef>(funName, args, boolType(), std::move(body)));
    }
    return decls;
}
