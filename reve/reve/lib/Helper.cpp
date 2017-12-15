/*
 * This file is part of
 *    llreve - Automatic regression verification for LLVM programs
 *
 * Copyright (C) 2016 Karlsruhe Institute of Technology
 *
 * The system is published under a BSD license.
 * See LICENSE (distributed with this file) for details.
 */

#include "Helper.h"

#include "Memory.h"
#include "Opts.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/Operator.h"

using std::make_unique;
using std::set;
using std::string;
using std::unique_ptr;
using std::vector;

using namespace smt;
using namespace llreve::opts;

SMTRef instrLocation(const llvm::Value *val) {
    if (const auto ConstVal = llvm::dyn_cast<llvm::Constant>(val)) {
        return std::make_unique<ConstantBool>(true);
    }
    if (!val->getName().empty()) {
        return stringExpr(string(val->getName()) + "_OnStack");
    }
    return stringExpr("UnknownLocation");
}

SMTRef instrNameOrVal(const llvm::Value *val) {
    return instrNameOrVal(val, val->getType());
}

/// Get the name of the instruction or a string representation of the value if
/// it's a constant
SMTRef instrNameOrVal(const llvm::Value *val, const llvm::Type *ty) {
    if (const auto constInt = llvm::dyn_cast<llvm::ConstantInt>(val)) {
        const auto apInt = constInt->getValue();
        if (apInt.isIntN(1) && ty->isIntegerTy(1)) {
            return make_unique<ConstantBool>(apInt.getBoolValue());
        }
        return std::make_unique<ConstantInt>(apInt);
    }
    if (const auto constFP = llvm::dyn_cast<llvm::ConstantFP>(val)) {
        return std::make_unique<ConstantFP>(constFP->getValueAPF());
    }
    if (llvm::isa<llvm::ConstantPointerNull>(val)) {
        if (SMTGenerationOpts::getInstance().BitVect) {
            return makeOp("_", "bv0", "64");
        } else {
            return std::make_unique<ConstantInt>(llvm::APInt(64, 0));
        }
    }
    if (const auto gep = llvm::dyn_cast<llvm::GEPOperator>(val)) {
        if (!llvm::isa<llvm::Instruction>(val)) {
            return resolveGEP(*gep);
        }
    }

    if (const auto constExpr = llvm::dyn_cast<llvm::ConstantExpr>(val)) {
        if (constExpr->getOpcode() == llvm::Instruction::IntToPtr) {
            return instrNameOrVal(constExpr->getOperand(0));
        }
    }
    if (llvm::isa<llvm::GlobalValue>(val)) {
        return stringExpr(val->getName());
    }
    if (val->getName().empty()) {
        logErrorData("Unnamed variable\n", *val);
        exit(1);
    }
    return stringExpr(val->getName());
}

int typeSize(llvm::Type *Ty, const llvm::DataLayout &layout) {
    if (SMTGenerationOpts::getInstance().ByteHeap == ByteHeapOpt::Enabled) {
        return static_cast<int>(layout.getTypeAllocSize(Ty));
    }
    if (auto IntTy = llvm::dyn_cast<llvm::IntegerType>(Ty)) {
        if (IntTy->getBitWidth() <= 64) {
            return 1;
        }
        logError("Unsupported integer bitwidth: " +
                 std::to_string(IntTy->getBitWidth()) + "\n");
    }
    if (Ty->isDoubleTy()) {
        return 1;
    }
    if (auto structTy = llvm::dyn_cast<llvm::StructType>(Ty)) {
        int size = 0;
        for (auto elTy : structTy->elements()) {
            size += typeSize(elTy, layout);
        }
        return size;
    }
    if (auto arrayTy = llvm::dyn_cast<llvm::ArrayType>(Ty)) {
        return static_cast<int>(arrayTy->getNumElements()) *
               typeSize(arrayTy->getElementType(), layout);
    }
    if (llvm::isa<llvm::PointerType>(Ty)) {
        logWarning("pointer size: " +
                   std::to_string(Ty->getPrimitiveSizeInBits()) + "\n");
        // TODO: This should come from a DataLayout
        return 4;
    }
    logErrorData("Couldn’t calculate size of type\n", *Ty);
    return 0;
}

/// Filter vars to only include the ones from Program
std::vector<SortedVar> filterVars(int program,
                                  const std::vector<SortedVar> &vars) {
    std::vector<SortedVar> filteredVars;
    for (const auto &var : vars) {
        if (varBelongsTo(var.name, program)) {
            filteredVars.push_back(var);
        }
    }
    return filteredVars;
}

bool varBelongsTo(std::string varName, int program) {
    const std::string programName = std::to_string(program);
    const auto pos = varName.rfind("$");
    return varName.substr(pos + 1, programName.length()) == programName;
}

SortedVar llvmValToSortedVar(const llvm::Value *val) {
    return SortedVar(val->getName(), llvmType(val->getType()));
}

std::string heapName(Program prog) { return heapName(programIndex(prog)); }
std::string heapName(int progIndex) {
    return "HEAP$" + std::to_string(progIndex);
}
std::string heapResultName(Program prog) { return heapName(prog) + "_res"; }

std::string stackName(Program prog) { return stackName(programIndex(prog)); }
std::string stackName(int progIndex) {
    return "STACK$" + std::to_string(progIndex);
}
std::string stackResultName(Program prog) { return stackName(prog) + "_res"; }

std::string stackPointerName(Program prog) {
    return stackPointerName(programIndex(prog));
}
std::string stackPointerName(int progIndex) {
    return "SP$" + std::to_string(progIndex);
}

std::vector<std::string> &split(const std::string &s, char delim,
                                std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, elems);
    return elems;
}

vector<SortedVar> functionArgs(const llvm::Function &fun) {
    vector<SortedVar> args;
    for (auto &arg : fun.args()) {
        auto sVar = llvmValToSortedVar(&arg);
        args.push_back(sVar);
        if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled &&
            arg.getType()->isPointerTy()) {
            args.push_back({sVar.name + "_OnStack", boolType()});
        }
    }
    return args;
}

std::vector<SortedVar> getMutualResultValues(const llvm::StringRef assignedTo1,
                                             const llvm::Function &function1,
                                             const llvm::StringRef assignedTo2,
                                             const llvm::Function &function2) {
    std::vector<SortedVar> resultValues;
    resultValues.emplace_back(assignedTo1, llvmType(function1.getReturnType()));
    resultValues.emplace_back(assignedTo2, llvmType(function2.getReturnType()));
    if (SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
        resultValues.emplace_back(heapResultName(Program::First), memoryType());
        resultValues.emplace_back(heapResultName(Program::Second),
                                  memoryType());
    }
    if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
        resultValues.emplace_back(stackResultName(Program::First),
                                  memoryType());
        resultValues.emplace_back(stackResultName(Program::Second),
                                  memoryType());
    }
    return resultValues;
}

std::vector<SortedVar> getResultValues(Program prog,
                                       const llvm::StringRef assignedTo,
                                       const llvm::Function &function) {
    std::vector<SortedVar> resultValues;
    resultValues.emplace_back(assignedTo, llvmType(function.getReturnType()));
    if (SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
        resultValues.emplace_back(heapResultName(prog), memoryType());
    }
    if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
        resultValues.emplace_back(stackResultName(prog), memoryType());
    }
    return resultValues;
}

auto callsTransitively(const llvm::Function &caller,
                       const llvm::Function &callee) -> bool {
    set<const llvm::Function *> visited;
    set<const llvm::Function *> toProcess;
    toProcess.insert(&caller);
    while (!toProcess.empty()) {
        const auto called = calledFunctions(**toProcess.begin());

        if (called.find(&callee) != called.end()) {
            return true;
        }

        visited.insert(*toProcess.begin());
        toProcess.erase(toProcess.begin());
        std::set_difference(called.begin(), called.end(), visited.begin(),
                            visited.end(),
                            std::inserter(toProcess, toProcess.begin()));
    }
    return false;
}
auto calledFunctions(const llvm::Function &f) -> set<const llvm::Function *> {
    set<const llvm::Function *> called;
    for (const auto &bb : f) {
        for (const auto &instr : bb) {
            if (auto call = llvm::dyn_cast<llvm::CallInst>(&instr)) {
                auto fun = call->getCalledFunction();
                if (fun)
                    called.insert(fun);
            }
        }
    }
    return called;
}

auto dropSuffixFromName(string name) -> string {
    size_t suffix = name.find('$');
    if (suffix != string::npos) {
        return name.substr(0, suffix);
    }
    return name;
}
