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

    if (!llvm::isa<llvm::Instruction>(val)) {
        if (llvm::Operator::getOpcode(val) == llvm::Instruction::BitCast)
            return instrNameOrVal(
                    llvm::dyn_cast<llvm::Operator>(val)->getOperand(0));
        if (llvm::Operator::getOpcode(val) == llvm::Instruction::PtrToInt) {
            auto op = llvm::dyn_cast<llvm::PtrToIntOperator>(val);
            if (op->getType()->isIntegerTy(64))
                return instrNameOrVal(op->getPointerOperand());
        }
    }

    if (val->getName().empty()) {
        logErrorData("Unnamed variable\n", *val);
        exit(1);
    }
    return stringExpr(val->getName());
}

int typeSize(llvm::Type *Ty, const llvm::DataLayout &layout) {
    if (auto StructTy = llvm::dyn_cast<llvm::StructType>(Ty)) {
        if (StructTy->isOpaque())
            return 1;
    }
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

SMTRef argEquality(std::unique_ptr<TypedVariable> &arg1,
                   std::unique_ptr<TypedVariable> &arg2) {
    if (arg1->type.getTag() == TypeTag::Int &&
        arg2->type.getTag() == TypeTag::Int &&
        arg1->type.unsafeBitWidth() < arg2->type.unsafeBitWidth()) {
        // Size of an integer argument has increased
        string opName = "(_ sign_extend " + std::to_string(
                arg2->type.unsafeBitWidth() - arg1->type.unsafeBitWidth()) +
                        ")";
        SMTRef arg1Resized = makeOp(opName, std::move(arg1));
        return makeOp("=", std::move(arg1Resized), std::move(arg2));
    }
    return makeOp("=", std::move(arg1), std::move(arg2));
}

smt::SMTRef resultEquality(std::unique_ptr<smt::TypedVariable> arg1,
                           std::unique_ptr<smt::TypedVariable> arg2) {
    if (arg1->type.getTag() == TypeTag::Int &&
        arg2->type.getTag() == TypeTag::Int &&
        arg1->type.unsafeBitWidth() != arg2->type.unsafeBitWidth()) {
        // Size of an integer argument has increased
        string opName = "(_ sign_extend " + std::to_string(
                abs(arg2->type.unsafeBitWidth() -
                    arg1->type.unsafeBitWidth())) + ")";
        if (arg1->type.unsafeBitWidth() < arg2->type.unsafeBitWidth()) {
            SMTRef arg1Resized = makeOp(opName, std::move(arg1));
            return makeOp("=", std::move(arg1Resized), std::move(arg2));
        } else {
            SMTRef arg2Resized = makeOp(opName, std::move(arg2));
            return makeOp("=", std::move(arg1), std::move(arg2Resized));
        }
    }
    return makeOp("=", std::move(arg1), std::move(arg2));
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

llvm::Type *globalType(const llvm::GlobalVariable &global) {
    if (const auto pointerTy =
            llvm::dyn_cast<llvm::PointerType>(global.getType())) {
        return pointerTy->getElementType();
    } else {
        return global.getType();
    }
}

int globalSize(const llvm::GlobalVariable &global) {
    auto mod = global.getParent();
    return typeSize(globalType(global), mod->getDataLayout());
}

std::string heapPtrName(string allocSiteSuffix, int progIndex) {
    return "$heap_ptr_" + allocSiteSuffix + "$" + std::to_string(progIndex);
}

std::string heapPtrName(int allocSiteIndex, Program prog) {
    return heapPtrName(std::to_string(allocSiteIndex), programIndex(prog));
}

std::string heapPtrName(std::string allocSiteSuffix, Program prog) {
    return heapPtrName(allocSiteSuffix, programIndex(prog));
}

bool isPassedAsArgument(const llvm::Function &fun, const Program prog) {
    auto MainFunction = prog == Program::First
                        ? SMTGenerationOpts::getInstance().MainFunctions.first
                        : SMTGenerationOpts::getInstance().MainFunctions.second;
    for (auto user : fun.users()) {
        if (auto Inst = llvm::dyn_cast<llvm::Instruction>(user)) {
            auto UserFun = Inst->getParent()->getParent();
            if (!(MainFunction == UserFun ||
                  callsTransitively(*MainFunction, *UserFun)))
                continue;

            if (auto CallInst = llvm::dyn_cast<llvm::CallInst>(user)) {
                for (auto &arg : CallInst->arg_operands()) {
                    if (auto argFun = llvm::dyn_cast<llvm::Function>(&arg)) {
                        if (argFun == &fun)
                            return true;
                    }
                }
            } else
                return true;
        }
    }
    return false;
}

std::string typeName(const llvm::Type *Type) {
    std::string result;
    llvm::raw_string_ostream rso(result);
    Type->print(rso);
    rso.str();
    // We must do some modifications to the type name so that is is usable as
    // a Z3 variable
    result.erase(std::remove(result.begin(), result.end(), ' '), result.end());
    std::replace(result.begin(), result.end(), '(', '$');
    std::replace(result.begin(), result.end(), ')', '$');
    std::replace(result.begin(), result.end(), ',', '_');
    return result;
}
