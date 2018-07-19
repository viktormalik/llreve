/*
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#include "Memory.h"
#include "Opts.h"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Operator.h>
#include <Helper.h>

using namespace llreve::opts;

llvm::AnalysisKey AllocationSiteAnalysis::Key;

AllocationSiteAnalysis::Result AllocationSiteAnalysis::run(
        llvm::Function &Fun,
        llvm::FunctionAnalysisManager &am) {
    std::vector<const llvm::CallInst *> result;
    unsigned index = 0;
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (auto CallInst = llvm::dyn_cast<llvm::CallInst>(&Instr)) {
                if (isHeapAllocation(*getCalledFunction(CallInst))) {
                    auto N = llvm::MDNode::get(
                            Fun.getContext(),
                            llvm::MDString::get(Fun.getContext(),
                                                std::to_string(index++)));
                    CallInst->setMetadata("alloc_site_suffix", N);
                    result.push_back(CallInst);
                }
            }
        }
    }
    return result;
}
