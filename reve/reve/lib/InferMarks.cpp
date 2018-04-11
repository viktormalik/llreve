/*
 * This file is part of
 *    llreve - Automatic regression verification for LLVM programs
 *
 * Copyright (C) 2016 Karlsruhe Institute of Technology
 *
 * The system is published under a BSD license.
 * See LICENSE (distributed with this file) for details.
 */

#include "InferMarks.h"

#include "UnifyFunctionExitNodes.h"

#include <iostream>

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"

using std::make_pair;
using std::set;

llvm::AnalysisKey InferMarksAnalysis::Key;

BidirBlockMarkMap InferMarksAnalysis::run(llvm::Function &Fun,
                                          llvm::FunctionAnalysisManager &am) {
    MarkedBlocks[ENTRY_MARK].insert(&Fun.getEntryBlock());
    BlockedMarks[&Fun.getEntryBlock()].insert(ENTRY_MARK);
    MarkedBlocks[EXIT_MARK].insert(
        am.getResult<FunctionExitNodeAnalysis>(Fun).returnBlock);
    BlockedMarks[am.getResult<FunctionExitNodeAnalysis>(Fun).returnBlock]
        .insert(EXIT_MARK);
    llvm::LoopInfo &loopInfo = am.getResult<llvm::LoopAnalysis>(Fun);
    for (auto loop : loopInfo) {
        markLoop(loop);
    }

    BlockMarkMap = BidirBlockMarkMap(BlockedMarks, MarkedBlocks);
    return BlockMarkMap;
}

void InferMarksAnalysis::markLoop(llvm::Loop *loop) {
    for (auto subLoop : loop->getSubLoops()) {
        markLoop(subLoop);
    }
    MarkedBlocks.insert({Mark(nextMark), {loop->getHeader()}});
    BlockedMarks.insert({loop->getHeader(), {Mark(nextMark)}});
    nextMark++;
}
