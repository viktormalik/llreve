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

#include <regex>
#include "llvm/IR/PassManager.h"

const std::regex HEAP_REGEX =
    std::regex("^(HEAP|STACK)\\$(1|2)(.*?)$", std::regex::ECMAScript);
const std::regex INDEX_REGEX =
    std::regex("^i(1|2)(_res|_old|_stack)?$", std::regex::ECMAScript);

/*
 * Analysis of allocation sites of a function. These sites are then synchronised
 * between modules to return the same pointer.
 */
class AllocationSiteAnalysis
        : public llvm::AnalysisInfoMixin<AllocationSiteAnalysis> {
  public:
    using Result = std::vector<const llvm::CallInst *>;
    Result run(llvm::Function &Fun, llvm::FunctionAnalysisManager &am);

  private:
    friend llvm::AnalysisInfoMixin<AllocationSiteAnalysis>;
    static llvm::AnalysisKey Key;
};
