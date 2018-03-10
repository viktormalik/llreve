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

std::set<uint32_t> getVarArgs(const llvm::Function &fun) {
    std::set<uint32_t> varArgs;
    for (auto User : fun.users()) {
        if (const auto callInst = llvm::dyn_cast<llvm::CallInst>(User)) {
            varArgs.insert(callInst->getNumArgOperands() -
                           fun.getFunctionType()->getNumParams());
        } else {
            logWarningData("Unsupported use of function\n", *User);
        }
    }
    return varArgs;
}

static void appendExternInputArgs(const llvm::Function &fun, Program progIndex,
                                  std::vector<SortedVar> &args) {
    auto funArgs = functionArgs(fun);
    args.insert(args.end(), funArgs.begin(), funArgs.end());
    if (SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
        args.emplace_back(heapName(progIndex), memoryType());
    }
    if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
        args.emplace_back(stackPointerName(progIndex), pointerType());
        args.emplace_back(stackName(progIndex), memoryType());
    }
}

static std::vector<SortedVar> externDeclArgs(const llvm::Function &fun1,
                                             const llvm::Function &fun2,
                                             unsigned numberOfArguments) {
    std::vector<SortedVar> args;
    appendExternInputArgs(fun1, Program::First, args);
    appendExternInputArgs(fun2, Program::Second, args);
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
            !isIntrinsicSupported(fun1) && !isDebugInfo(fun1)) {
            auto decls = externFunDecl(fun1, Program::First);
            declarations.insert(declarations.end(),
                                std::make_move_iterator(decls.begin()),
                                std::make_move_iterator(decls.end()));
        }
    }
    for (auto &fun2 : mod2) {
        if (hasFixedAbstraction(fun2) && !isLlreveIntrinsic(fun2) &&
            !isIntrinsicSupported(fun2) && !isDebugInfo(fun2)) {
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
                          unsigned numberOfArguments) {
    std::vector<SharedSMTRef> equal;
    auto funArgs1 = functionArgs(fun1);
    auto funArgs2 = functionArgs(fun2);
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
    set<uint32_t> varArgs = getVarArgs(fun1);
    set<uint32_t> varArgs2 = getVarArgs(fun2);
    for (auto el : varArgs2) {
        varArgs.insert(el);
    }
    for (const auto argNum : varArgs) {
        vector<SortedVar> args = externDeclArgs(fun1, fun2, argNum);
        std::string funName =
            invariantName(ENTRY_MARK, ProgramSelection::Both,
                          fun1.getName().str() + "^" + fun2.getName().str(),
                          InvariantAttr::NONE, argNum);

        SMTRef eqOutputs = equalOutputs(fun1, fun2, funCondMap);
        SMTRef eqInputs = equalInputs(fun1, fun2, argNum);
        SMTRef body = makeOp("=>", std::move(eqInputs), std::move(eqOutputs));

        if (fun1.getMetadata("is_nondet")) {
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
    set<uint32_t> varArgs = getVarArgs(fun1);
    set<uint32_t> varArgs2 = getVarArgs(fun2);
    for (auto el : varArgs2) {
        varArgs.insert(el);
    }
    for (const auto argNum : varArgs) {
        vector<SortedVar> args = externDeclArgs(fun1, fun2, argNum);
        std::string funName =
            invariantName(ENTRY_MARK, ProgramSelection::Both,
                          fun1.getName().str() + "^" + fun2.getName().str(),
                          InvariantAttr::NONE, argNum);
        SMTRef body;
        if (fun1.getMetadata("is_nondet")) {
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
    set<uint32_t> varArgs = getVarArgs(fun);
    for (auto argNum : varArgs) {
        std::vector<SortedVar> args = functionArgs(fun);
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
                          InvariantAttr::NONE, argNum);
        SMTRef body;
        if (fun.getMetadata("is_nondet") &&
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
