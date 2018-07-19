/*
 * This file is part of
 *    llreve - Automatic regression verification for LLVM programs
 *
 * Copyright (C) 2016 Karlsruhe Institute of Technology
 *
 * The system is published under a BSD license.
 * See LICENSE (distributed with this file) for details.
 */

#include <FreeVariables.h>
#include "Assignment.h"

#include "Helper.h"
#include "Opts.h"

#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"

using std::vector;
using std::make_unique;
using std::unique_ptr;
using std::string;
using llvm::Instruction;
using llvm::CmpInst;

using namespace smt;
using namespace llreve::opts;

/// Convert a basic block to a list of assignments
vector<DefOrCallInfo> blockAssignments(const llvm::BasicBlock &BB,
                                       const llvm::BasicBlock *prevBb,
                                       bool onlyPhis, Program prog) {
    const int progIndex = programIndex(prog);
    vector<DefOrCallInfo> definitions;
    definitions.reserve(BB.size());
    assert(BB.size() >= 1); // There should be at least a terminator instruction
    bool ignorePhis = prevBb == nullptr;
    for (auto instr = BB.begin(), e = std::prev(BB.end(), 1); instr != e;
         ++instr) {
        // Ignore phi nodes if we are in a loop as they're bound in a
        // forall quantifier
        if (!ignorePhis && llvm::isa<llvm::PHINode>(instr)) {
            auto assignments = instrAssignment(*instr, prevBb, prog);
            for (auto &assignment : assignments) {
                definitions.emplace_back(std::move(assignment));
            }
        }
        if (!onlyPhis && !llvm::isa<llvm::PHINode>(instr)) {
            if (const auto CallInst = llvm::dyn_cast<llvm::CallInst>(instr)) {
                const auto fun = getCalledFunction(CallInst);
                if (!fun) {
                    logErrorData("Call to undeclared function\n", *CallInst);
                    exit(1);
                }
                auto defs = processIntrinsic(CallInst, prog);
                if (!defs.empty()) {
                    for (auto &def : defs) {
                        definitions.emplace_back(std::move(def));
                    }
                } else {
                    if (SMTGenerationOpts::getInstance().Heap ==
                        HeapOpt::Enabled) {
                        definitions.emplace_back(makeAssignment(
                            heapName(progIndex),
                            memoryVariable(heapName(progIndex))));
                    }
                    definitions.emplace_back(
                        toCallInfo(CallInst->getName(), prog, *CallInst));
                    if (CallInst->getType()->isPointerTy()
                        && SMTGenerationOpts::getInstance().Stack ==
                           StackOpt::Enabled) {
                       definitions.emplace_back(makeAssignment(
                               string(CallInst->getName()) + "_OnStack",
                               std::make_unique<ConstantBool>(true)));
                    }
                    if (SMTGenerationOpts::getInstance().Heap ==
                        HeapOpt::Enabled) {
                        definitions.emplace_back(makeAssignment(
                            heapName(progIndex),
                            memoryVariable(heapResultName(prog))));
                    }
                    if (SMTGenerationOpts::getInstance().Stack ==
                        StackOpt::Enabled) {
                        definitions.emplace_back(makeAssignment(
                            stackName(progIndex),
                            memoryVariable(stackResultName(prog))));
                    }
                }
            } else {
                auto assignments = instrAssignment(*instr, prevBb, prog);
                for (auto &assignment : assignments) {
                    definitions.push_back(std::move(assignment));
                }
            }
        }
    }
    if (const auto retInst =
            llvm::dyn_cast<llvm::ReturnInst>(BB.getTerminator())) {
        // TODO (moritz): use a more clever approach for void functions
        unique_ptr<SMTExpr> retName =
            std::make_unique<ConstantInt>(llvm::APInt(64, 0));
        if (retInst->getReturnValue() != nullptr) {
            retName =
                instrNameOrVal(retInst->getReturnValue(), retInst->getType());
        }
        definitions.push_back(DefOrCallInfo(
            makeAssignment(resultName(prog), std::move(retName))));
        if (SMTGenerationOpts::getInstance().Heap == HeapOpt::Enabled) {
            definitions.push_back(DefOrCallInfo(makeAssignment(
                heapResultName(prog), memoryVariable(heapName(progIndex)))));
        }
        if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
            definitions.push_back(DefOrCallInfo(makeAssignment(
                stackResultName(prog), memoryVariable(stackName(progIndex)))));
        }
    }
    return definitions;
}

/// Convert a single instruction to an assignment
llvm::SmallVector<unique_ptr<Assignment>, 1>
instrAssignment(const llvm::Instruction &Instr, const llvm::BasicBlock *prevBb,
                Program prog) {
    const int progIndex = programIndex(prog);
    if (const auto BinOp = llvm::dyn_cast<llvm::BinaryOperator>(&Instr)) {
        if (BinOp->getType()->isFloatingPointTy()) {
            auto op = std::make_unique<BinaryFPOperator>(
                binaryFPOpcode(BinOp->getOpcode()), llvmType(BinOp->getType()),
                instrNameOrVal(BinOp->getOperand(0)),
                instrNameOrVal(BinOp->getOperand(1)));
            return vecSingleton(
                makeAssignment(BinOp->getName(), std::move(op)));
        }
        if (SMTGenerationOpts::getInstance().ByteHeap ==
                ByteHeapOpt::Disabled &&
            BinOp->getOpcode() == Instruction::SDiv) {
            // This is a heuristic to remove divisions by 4 of pointer
            // subtractions
            // Since we treat every int as a single value, we expect the
            // division to return the number of elements and not the number
            // of
            // bytes
            if (const auto instr =
                    llvm::dyn_cast<llvm::Instruction>(BinOp->getOperand(0))) {
                if (const auto ConstInt = llvm::dyn_cast<llvm::ConstantInt>(
                        BinOp->getOperand(1))) {
                    if (ConstInt->getSExtValue() == 4 && isPtrDiff(*instr)) {
                        return vecSingleton(makeAssignment(
                            BinOp->getName(),
                            instrNameOrVal(BinOp->getOperand(0))));
                    } else {
                        logWarning("Division of pointer difference by " +
                                   std::to_string(ConstInt->getSExtValue()) +
                                   "\n");
                    }
                }
            }
        }
        if (!SMTGenerationOpts::getInstance().BitVect &&
            (BinOp->getOpcode() == Instruction::Or ||
             BinOp->getOpcode() == Instruction::And ||
             BinOp->getOpcode() == Instruction::Xor)) {
            if (!(BinOp->getOperand(0)->getType()->isIntegerTy(1) &&
                  BinOp->getOperand(1)->getType()->isIntegerTy(1))) {
                logWarningData(
                    "Bitwise operators of bitwidth > 1 is not supported\n",
                    *BinOp);
            }
        }
        return vecSingleton(makeAssignment(
            BinOp->getName(), combineOp(*BinOp,
                                        opName(*BinOp, *BinOp->getType()),
                                        instrNameOrVal(BinOp->getOperand(0)),
                                        instrNameOrVal(BinOp->getOperand(1)))));
    }
    if (const auto cmpInst = llvm::dyn_cast<llvm::CmpInst>(&Instr)) {
        SMTRef cmp = makeOp(
            predicateName(cmpInst->getPredicate()),
            predicateFun(*cmpInst, instrNameOrVal(cmpInst->getOperand(0))),
            predicateFun(*cmpInst, instrNameOrVal(cmpInst->getOperand(1))));
        return vecSingleton(makeAssignment(cmpInst->getName(), std::move(cmp)));
    }
    if (const auto phiInst = llvm::dyn_cast<llvm::PHINode>(&Instr)) {
        const auto val = phiInst->getIncomingValueForBlock(prevBb);
        assert(val);
        auto assgn = makeAssignment(phiInst->getName(), instrNameOrVal(val));
        if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled &&
            phiInst->getType()->isPointerTy()) {
            auto locAssgn = makeAssignment(
                string(phiInst->getName()) + "_OnStack", instrLocation(val));
            llvm::SmallVector<unique_ptr<Assignment>, 1> result;
            result.push_back(std::move(assgn));
            result.push_back(std::move(locAssgn));
            return result;
        } else {
            return vecSingleton(std::move(assgn));
        }
    }
    if (const auto selectInst = llvm::dyn_cast<llvm::SelectInst>(&Instr)) {
        const auto cond = selectInst->getCondition();
        const auto trueVal = selectInst->getTrueValue();
        const auto falseVal = selectInst->getFalseValue();
        const vector<SharedSMTRef> args = {instrNameOrVal(cond),
                                           instrNameOrVal(trueVal),
                                           instrNameOrVal(falseVal)};
        auto assgn = makeAssignment(selectInst->getName(),
                                    std::make_unique<Op>("ite", args));
        if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled &&
            trueVal->getType()->isPointerTy()) {
            assert(falseVal->getType()->isPointerTy());
            auto location =
                makeOp("ite", instrNameOrVal(cond), instrLocation(trueVal),
                       instrLocation(falseVal));
            llvm::SmallVector<unique_ptr<Assignment>, 1> result;
            result.push_back(std::move(assgn));
            result.push_back(
                makeAssignment(string(selectInst->getName()) + "_OnStack",
                               std::move(location)));
            return result;
        } else {
            return vecSingleton(std::move(assgn));
        }
    }
    if (const auto ptrToIntInst = llvm::dyn_cast<llvm::PtrToIntInst>(&Instr)) {
        return vecSingleton(
            makeAssignment(ptrToIntInst->getName(),
                           instrNameOrVal(ptrToIntInst->getPointerOperand(),
                                          ptrToIntInst->getType())));
    }
    if (const auto getElementPtrInst =
            llvm::dyn_cast<llvm::GetElementPtrInst>(&Instr)) {
        auto assgn = makeAssignment(getElementPtrInst->getName(),
                                    resolveGEP(*getElementPtrInst));
        if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
            llvm::SmallVector<unique_ptr<Assignment>, 1> result;
            result.push_back(std::move(assgn));
            result.push_back(makeAssignment(
                string(getElementPtrInst->getName()) + "_OnStack",
                instrLocation(getElementPtrInst->getPointerOperand())));
            return result;
        } else {
            return vecSingleton(std::move(assgn));
        }
    }
    if (const auto loadInst = llvm::dyn_cast<llvm::LoadInst>(&Instr)) {
        SharedSMTRef pointer = instrNameOrVal(loadInst->getOperand(0));
        if (SMTGenerationOpts::getInstance().BitVect) {
            llvm::SmallVector<unique_ptr<Assignment>, 1> result;
            // We load single bytes
            unsigned bytes = loadInst->getType()->getIntegerBitWidth() / 8;
            auto load =
                makeOp("select", memoryVariable(heapName(progIndex)), pointer);
            for (unsigned i = 1; i < bytes; ++i) {
                load =
                    makeOp("concat", std::move(load),
                           makeOp("select", memoryVariable(heapName(progIndex)),
                                  makeOp("bvadd", pointer,
                                         std::make_unique<ConstantInt>(
                                             llvm::APInt(64, i)))));
            }
            if (bytes == 0)
                bytes = 1;
            if (bytes * 8 < llvmType(loadInst->getType()).unsafeBitWidth()) {
                load = smt::makeOp("(_ sign_extend " + std::to_string(
                        llvmType(loadInst->getType()).unsafeBitWidth() -
                        (bytes * 8)) + ")",
                                   std::move(load));
            }
            result.push_back(
                makeAssignment(loadInst->getName(), std::move(load)));
            if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled &&
                loadInst->getType()->isPointerTy()) {
                result.push_back(makeAssignment(
                        string(loadInst->getName()) + "_OnStack",
                        instrLocation(loadInst->getPointerOperand())));
            }
            return result;
        } else {
            if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
                llvm::SmallVector<unique_ptr<Assignment>, 1> result;
                SMTRef load =
                    makeOp("select_", memoryVariable(heapName(progIndex)),
                           memoryVariable(stackName(progIndex)), pointer,
                           instrLocation(loadInst->getPointerOperand()));
                result.push_back(makeAssignment(loadInst->getName(),
                                                std::move(load)));

                result.push_back(makeAssignment(
                    string(loadInst->getName()) + "_OnStack",
                    instrLocation(loadInst->getPointerOperand())));
                return result;
            } else {
                SMTRef load = makeOp(
                    "select", memoryVariable(heapName(progIndex)), pointer);
                return vecSingleton(
                    makeAssignment(loadInst->getName(), std::move(load)));
            }
        }
    }
    if (const auto storeInst = llvm::dyn_cast<llvm::StoreInst>(&Instr)) {
        string heap = heapName(progIndex);
        SharedSMTRef pointer = instrNameOrVal(storeInst->getPointerOperand());
        SharedSMTRef val = instrNameOrVal(storeInst->getValueOperand());
        if (SMTGenerationOpts::getInstance().BitVect) {
            int bytes =
                storeInst->getValueOperand()->getType()->getIntegerBitWidth() /
                8;
            auto newHeap = memoryVariable(heap);
            for (int i = 0; i < bytes; ++i) {
                SharedSMTRef offset =
                    makeOp("bvadd", pointer,
                           std::make_unique<ConstantInt>(llvm::APInt(64, i)));
                SharedSMTRef elem = makeOp(
                    "(_ extract " + std::to_string(8 * (bytes - i - 1) + 7) +
                        " " + std::to_string(8 * (bytes - i - 1)) + ")",
                    val);
                std::vector<SharedSMTRef> args = {std::move(newHeap), offset,
                                                  elem};
                newHeap = make_unique<Op>("store", std::move(args));
            }
            return vecSingleton(
                makeAssignment(heapName(progIndex), std::move(newHeap)));
        } else {
            if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
                const std::vector<SharedSMTRef> args = {
                    memoryVariable(heap), memoryVariable(stackName(progIndex)),
                    pointer, instrLocation(storeInst->getPointerOperand()),
                    val};
                auto store = make_unique<Op>("store_", args);
                return vecSingleton(
                    makeAssignment(heapName(progIndex), std::move(store)));
            } else {
                const std::vector<SharedSMTRef> args = {memoryVariable(heap),
                                                        pointer, val};
                auto store = make_unique<Op>("store", args);
                return vecSingleton(
                    makeAssignment(heapName(progIndex), std::move(store)));
            }
        }
    }
    if (const auto bitCast = llvm::dyn_cast<llvm::CastInst>(&Instr)) {
        llvm::SmallVector<unique_ptr<Assignment>, 1> result;

        auto cast = std::make_unique<TypeCast>(
            bitCast->getOpcode(), llvmType(bitCast->getSrcTy()),
            llvmType(bitCast->getDestTy()),
            instrNameOrVal(bitCast->getOperand(0)));
        result.push_back(makeAssignment(bitCast->getName(), std::move(cast)));

        if (bitCast->getDestTy()->isPointerTy() &&
            SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
            if (bitCast->getSrcTy()->isPointerTy()) {
                result.push_back(
                        makeAssignment(string(bitCast->getName()) + "_OnStack",
                                       instrLocation(bitCast->getOperand(0))));
            } else {
                result.push_back(
                        makeAssignment(string(bitCast->getName()) + "_OnStack",
                                       make_unique<ConstantBool>(true)));
            }
        }
        return result;
    }
    if (const auto allocaInst = llvm::dyn_cast<llvm::AllocaInst>(&Instr)) {
        unsigned allocatedSize =
            typeSize(allocaInst->getAllocatedType(),
                     allocaInst->getModule()->getDataLayout());
        std::string sp = stackPointerName(progIndex);
        llvm::SmallVector<unique_ptr<Assignment>, 1> result;
        auto subOp = SMTGenerationOpts::getInstance().BitVect ? "bvsub" : "-";
        result.push_back(makeAssignment(
            sp, makeOp(subOp, sp, std::make_unique<ConstantInt>(
                                    llvm::APInt(64, allocatedSize)))));
        result.push_back(
            makeAssignment(allocaInst->getName(),
                           make_unique<TypedVariable>(sp, pointerType())));
        result.push_back(
            makeAssignment(string(allocaInst->getName()) + "_OnStack",
                           make_unique<ConstantBool>(true)));
        return result;
    }
    if (const auto fcmpInst = llvm::dyn_cast<llvm::FCmpInst>(&Instr)) {
        auto cmp = std::make_unique<FPCmp>(
            fpPredicate(fcmpInst->getPredicate()),
            llvmType(fcmpInst->getOperand(0)->getType()),
            instrNameOrVal(fcmpInst->getOperand(0)),
            instrNameOrVal(fcmpInst->getOperand(1)));
        return vecSingleton(
            makeAssignment(fcmpInst->getName(), std::move(cmp)));
    }
    if (const auto extractValInst =
            llvm::dyn_cast<llvm::ExtractValueInst>(&Instr)) {
        llvm::SmallVector<unique_ptr<Assignment>, 1> result;
        auto value = instrNameOrVal(extractValInst->getAggregateOperand());
        for (const auto &i : extractValInst->indices()) {
            value = makeOp("elem" + std::to_string(i), std::move(value));
        }
        result.push_back(makeAssignment(extractValInst->getName(),
                                        std::move(value)));
        if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled &&
            extractValInst->getType()->isPointerTy()) {
            result.push_back(makeAssignment(
                    string(extractValInst->getName()) + "_OnStack",
                    std::make_unique<ConstantBool>(true)));
        }
        return result;
    }
    logErrorData("Couldn’t convert instruction to def\n", Instr);
    return vecSingleton(
        makeAssignment("UNKNOWN INSTRUCTION", stringExpr("UNKNOWN ARGS")));
}

/// Convert an LLVM predicate to an SMT predicate
string predicateName(llvm::CmpInst::Predicate pred) {
    if (SMTGenerationOpts::getInstance().BitVect) {
        switch (pred) {
        case CmpInst::ICMP_SLT:
            return "bvslt";
        case CmpInst::ICMP_ULT:
            return "bvult";
        case CmpInst::ICMP_SLE:
            return "bvsle";
        case CmpInst::ICMP_ULE:
            return "bvule";
        case CmpInst::ICMP_EQ:
            return "=";
        case CmpInst::ICMP_SGE:
            return "bvsge";
        case CmpInst::ICMP_UGE:
            return "bvuge";
        case CmpInst::ICMP_SGT:
            return "bvsgt";
        case CmpInst::ICMP_UGT:
            return "bvugt";
        case CmpInst::ICMP_NE:
            return "distinct";
        default:
            return "unsupported predicate";
        }

    } else {
        switch (pred) {
        case CmpInst::ICMP_SLT:
        case CmpInst::ICMP_ULT:
            return "<";
        case CmpInst::ICMP_SLE:
        case CmpInst::ICMP_ULE:
            return "<=";
        case CmpInst::ICMP_EQ:
            return "=";
        case CmpInst::ICMP_SGE:
        case CmpInst::ICMP_UGE:
            return ">=";
        case CmpInst::ICMP_SGT:
        case CmpInst::ICMP_UGT:
            return ">";
        case CmpInst::ICMP_NE:
            return "distinct";
        default:
            return "unsupported predicate";
        }
    }
}
FPCmp::Predicate fpPredicate(llvm::CmpInst::Predicate pred) {
    switch (pred) {
    case CmpInst::FCMP_FALSE:
        return FPCmp::Predicate::False;
    case CmpInst::FCMP_OEQ:
        return FPCmp::Predicate::OEQ;
    case CmpInst::FCMP_OGT:
        return FPCmp::Predicate::OGT;
    case CmpInst::FCMP_OGE:
        return FPCmp::Predicate::OGE;
    case CmpInst::FCMP_OLT:
        return FPCmp::Predicate::OLT;
    case CmpInst::FCMP_OLE:
        return FPCmp::Predicate::OLE;
    case CmpInst::FCMP_ONE:
        return FPCmp::Predicate::ONE;
    case CmpInst::FCMP_ORD:
        return FPCmp::Predicate::ORD;
    case CmpInst::FCMP_UNO:
        return FPCmp::Predicate::UNO;
    case CmpInst::FCMP_UEQ:
        return FPCmp::Predicate::UEQ;
    case CmpInst::FCMP_UGT:
        return FPCmp::Predicate::UGT;
    case CmpInst::FCMP_UGE:
        return FPCmp::Predicate::UGE;
    case CmpInst::FCMP_ULT:
        return FPCmp::Predicate::ULT;
    case CmpInst::FCMP_ULE:
        return FPCmp::Predicate::ULE;
    case CmpInst::FCMP_UNE:
        return FPCmp::Predicate::UNE;
    case CmpInst::FCMP_TRUE:
        return FPCmp::Predicate::True;
    default:
        logError("No floating point predicate\n");
        exit(1);
    }
}

BinaryFPOperator::Opcode binaryFPOpcode(llvm::Instruction::BinaryOps op) {
    switch (op) {
    case Instruction::FAdd:
        return BinaryFPOperator::Opcode::FAdd;
    case Instruction::FSub:
        return BinaryFPOperator::Opcode::FSub;
    case Instruction::FMul:
        return BinaryFPOperator::Opcode::FMul;
    case Instruction::FDiv:
        return BinaryFPOperator::Opcode::FDiv;
    case Instruction::FRem:
        return BinaryFPOperator::Opcode::FRem;
    default:
        logError("Not a floating point binary operator\n");
        exit(1);
    }
}

/// A function that is abblied to the arguments of a predicate
SMTRef predicateFun(const llvm::CmpInst &cmp, SMTRef expr) {
    if (!SMTGenerationOpts::getInstance().BitVect && cmp.isUnsigned() &&
        !SMTGenerationOpts::getInstance().EverythingSigned) {
        return makeOp("abs", std::move(expr));
    }
    return expr;
}

/// Convert an LLVM op to an SMT op
string opName(const llvm::BinaryOperator &Op, const llvm::Type &Type) {
    if (SMTGenerationOpts::getInstance().BitVect) {
        switch (Op.getOpcode()) {
        case Instruction::Add:
            return "bvadd";
        case Instruction::Sub:
            return "bvsub";
        case Instruction::Mul:
            return "bvmul";
        case Instruction::SDiv:
            return "bvsdiv";
        case Instruction::UDiv:
            return "bvudiv";
        case Instruction::SRem:
            return "bvsrem";
        case Instruction::URem:
            return "bvurem";
        // Bit operations on boolean values must yield a bool
        case Instruction::Or:
            return Type.isIntegerTy(1) ? "or" : "bvor";
        case Instruction::And:
            return Type.isIntegerTy(1) ? "and" : "bvand";
        case Instruction::Xor:
            return Type.isIntegerTy(1) ? "xor" : "bvxor";
        case Instruction::AShr:
            return "bvashr";
        case Instruction::LShr:
            return "bvlshr";
        case Instruction::Shl:
            return "bvshl";
        default:
            logError("Unknown opcode: " + std::string(Op.getOpcodeName()) +
                     "\n");
            return Op.getOpcodeName();
        }
    } else {
        switch (Op.getOpcode()) {
        case Instruction::Add:
            return "+";
        case Instruction::Sub:
            return "-";
        case Instruction::Mul:
            return "*";
        case Instruction::SDiv:
            return "div";
        case Instruction::UDiv:
            return "div";
        case Instruction::SRem:
            return "mod";
        case Instruction::URem:
            return "mod";
        case Instruction::Or:
            return "or";
        case Instruction::And:
            return "and";
        case Instruction::Xor:
            return "xor";
        case Instruction::AShr:
        case Instruction::LShr:
            return "div";
        case Instruction::Shl:
            return "*";
        default:
            logError("Unknown opcode: " + std::string(Op.getOpcodeName()) +
                     "\n");
            return Op.getOpcodeName();
        }
    }
}

SMTRef combineOp(const llvm::BinaryOperator &Op, std::string opName,
                 SMTRef firstArg, SMTRef secondArg) {
    if (!SMTGenerationOpts::getInstance().BitVect &&
        (Op.getOpcode() == Instruction::AShr ||
         Op.getOpcode() == Instruction::LShr ||
         Op.getOpcode() == Instruction::Shl)) {
        // We can only do that if there is a constant on the right side
        if (const auto constInt =
                llvm::dyn_cast<llvm::ConstantInt>(Op.getOperand(1))) {
            // rounding conversion to guard for floating point errors
            uint64_t divisor =
                static_cast<uint64_t>(pow(2, constInt->getZExtValue()) + 0.5);
            return makeOp(
                std::move(opName), std::move(firstArg),
                std::make_unique<ConstantInt>(llvm::APInt(64, divisor)));
        } else {
            logErrorData("Only shifts by a constant are supported\n", Op);
        }
    }

    return makeOp(std::move(opName), std::move(firstArg), std::move(secondArg));
}

vector<DefOrCallInfo> memcpyIntrinsic(const llvm::CallInst *callInst,
                                      Program prog) {
    vector<DefOrCallInfo> definitions;
    llvm::PointerType *ty0 = nullptr;
    const auto castInst0 =
        llvm::dyn_cast<llvm::CastInst>(callInst->getArgOperand(0));
    if (castInst0) {
        ty0 = llvm::dyn_cast<llvm::PointerType>(castInst0->getSrcTy());
    } else {
        if (const auto castOp0 = llvm::dyn_cast<llvm::BitCastOperator>(
                callInst->getArgOperand(0))) {
            ty0 = llvm::dyn_cast<llvm::PointerType>(castOp0->getSrcTy());
        }
    }
    llvm::PointerType *ty1 = nullptr;
    const auto castInst1 =
        llvm::dyn_cast<llvm::CastInst>(callInst->getArgOperand(1));
    if (castInst1) {
        ty1 = llvm::dyn_cast<llvm::PointerType>(castInst1->getSrcTy());
    } else {
        if (const auto castOp1 = llvm::dyn_cast<llvm::BitCastOperator>(
                callInst->getArgOperand(1))) {
            ty1 = llvm::dyn_cast<llvm::PointerType>(castOp1->getSrcTy());
        }
    }
    if (ty0 && ty1) {
        const auto StructTy0 =
            llvm::dyn_cast<llvm::StructType>(ty0->getElementType());
        const auto StructTy1 =
            llvm::dyn_cast<llvm::StructType>(ty1->getElementType());
        if (StructTy0 && StructTy1) {
            assert(StructTy0->isLayoutIdentical(StructTy1));
            SharedSMTRef basePointerDest =
                instrNameOrVal(callInst->getArgOperand(0));
            SharedSMTRef basePointerSrc =
                instrNameOrVal(callInst->getArgOperand(1));
            string heapNameSelect = heapName(prog);
            string heapNameStore = heapName(prog);
            string addOp = SMTGenerationOpts::getInstance().BitVect ? "bvadd"
                                                                    : "+";
            int i = 0;
            for (const auto elTy : StructTy0->elements()) {
                SharedSMTRef heapSelect = memoryVariable(heapNameSelect);
                SharedSMTRef heapStore = memoryVariable(heapNameStore);
                for (int j = 0;
                     j < typeSize(elTy, callInst->getModule()->getDataLayout());
                     ++j) {
                    SMTRef select = makeOp("select", heapSelect,
                                           makeOp(addOp, basePointerSrc,
                                                  std::make_unique<ConstantInt>(
                                                      llvm::APInt(64, i))));
                    const vector<SharedSMTRef> args = {
                        heapStore, makeOp(addOp, basePointerDest,
                                          std::make_unique<ConstantInt>(
                                              llvm::APInt(64, i))),
                        std::move(select)};
                    definitions.push_back(makeAssignment(
                        heapNameStore, make_unique<Op>("store", args)));
                    ++i;
                }
            }
        } else {
            logError("currently only memcpy of structs is "
                     "supported\n");
            exit(1);
        }
    } else {
        logError("currently only memcpy of "
                 "bitcasted pointers is supported\n");
        exit(1);
    }
    return definitions;
}

unique_ptr<CallInfo> toCallInfo(string assignedTo, Program prog,
                                const llvm::CallInst &callInst) {
    vector<SharedSMTRef> args;
    if (assignedTo.empty()) {
        assignedTo = "res" + std::to_string(programIndex(prog));
    }
    uint32_t i = 0;
    const auto &funTy = *callInst.getFunctionType();
    const llvm::Function *fun = getCalledFunction(&callInst);
    VarArgs varArgs;
    for (auto &arg : callInst.arg_operands()) {
        args.push_back(instrNameOrVal(arg, arg->getType()));
        if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled &&
            arg->getType()->isPointerTy()) {
            args.push_back(instrLocation(arg));
        }

        if (i >= funTy.getNumParams())
            varArgs.argTypes.push_back(arg->getType());
        ++i;
    }
    return std::make_unique<CallInfo>(assignedTo, fun->getName(), args, varArgs,
                                      *fun);
}

bool isPtrDiff(const llvm::Instruction &instr) {
    if (const auto binOp = llvm::dyn_cast<llvm::BinaryOperator>(&instr)) {
        return binOp->getOpcode() == Instruction::Sub &&
               llvm::isa<llvm::PtrToIntInst>(binOp->getOperand(0)) &&
               llvm::isa<llvm::PtrToIntInst>(binOp->getOperand(1));
    }
    return false;
}

auto coupledCalls(const CallInfo &call1, const CallInfo &call2) -> bool {
    const auto &coupledFunctions =
        SMTGenerationOpts::getInstance().CoupledFunctions;
    bool coupledNames = false;
    // The const cast here is safe because we are only comparing pointers
    coupledNames =
        coupledFunctions.find({const_cast<llvm::Function *>(&call1.fun),
                               const_cast<llvm::Function *>(&call2.fun)}) !=
        coupledFunctions.end();
    if (!hasFixedAbstraction(call1.fun) || !hasFixedAbstraction(call2.fun)) {
        return coupledNames;
    }
    return coupledNames &&
           call1.fun.getFunctionType()->getNumParams() ==
               call2.fun.getFunctionType()->getNumParams();
}

std::vector<DefOrCallInfo> allocOnHeap(
        const llvm::CallInst *callInst,
        Program prog) {
    std::vector<DefOrCallInfo> definitions;
    auto N = callInst->getMetadata("alloc_site_suffix");
    auto allocSiteSuffix =
            llvm::cast<llvm::MDString>(N->getOperand(0))->getString();
    definitions.push_back(makeAssignment(callInst->getName(),
                                         stringExpr(heapPtrName(allocSiteSuffix,
                                                                prog))));
    if (SMTGenerationOpts::getInstance().Stack == StackOpt::Enabled) {
        definitions.push_back(
                makeAssignment(string(callInst->getName()) + "_OnStack",
                               std::make_unique<ConstantBool>(false)));
    }
    return definitions;
}

bool isBitcast(const llvm::Value *val) {
    return llvm::isa<llvm::BitCastOperator>(val) ||
           llvm::isa<llvm::BitCastInst>(val);
}

std::vector<DefOrCallInfo> processIntrinsic(const llvm::CallInst *callInst,
                                            Program prog) {
    auto *fun = getCalledFunction(callInst);
    if (fun->getIntrinsicID() == llvm::Intrinsic::memcpy &&
        isBitcast(callInst->getOperand(0)) &&
        isBitcast(callInst->getOperand(1)))
        return memcpyIntrinsic(callInst, prog);

    if (isHeapAllocation(*fun))
        return allocOnHeap(callInst, prog);

    return {};
}

