/*
 * This file is part of
 *    llreve - Automatic regression verification for LLVM programs
 *
 * Copyright (C) 2016 Karlsruhe Institute of Technology
 *
 * The system is published under a BSD license.
 * See LICENSE (distributed with this file) for details.
 */

#pragma once

#include "MarkAnalysis.h"

#include "llvm/IR/LegacyPassManager.h"

#include <map>
#include <set>
#include <llvm/Analysis/LoopInfo.h>

class InferMarksAnalysis : public llvm::AnalysisInfoMixin<InferMarksAnalysis> {
  public:
    using Result = BidirBlockMarkMap;
    Result run(llvm::Function &Fun, llvm::FunctionAnalysisManager &am);
    // it’s not possible to have non default constructors with the legacy
    // passmanager so we can’t just pass a pointer there to escape this
    BidirBlockMarkMap BlockMarkMap;

  protected:
    std::map<Mark, std::set<llvm::BasicBlock *>> MarkedBlocks;
    std::map<llvm::BasicBlock *, std::set<Mark>> BlockedMarks;
    int nextMark = 1;

    void markLoop(llvm::Loop *loop);

  private:
    friend llvm::AnalysisInfoMixin<InferMarksAnalysis>;
    static llvm::AnalysisKey Key;
};
