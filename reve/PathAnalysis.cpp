#include "PathAnalysis.h"

#include "Helper.h"
#include "SExpr.h"

#include <iostream>

#include "llvm/IR/Instructions.h"

char PathAnalysis::PassID;

PathMap PathAnalysis::run(llvm::Function &F,
                          llvm::FunctionAnalysisManager *AM) {
    PathMap MyPaths;
    auto BidirMarkBlockMap = AM->getResult<MarkAnalysis>(F);
    for (auto BBTuple : BidirMarkBlockMap.MarkToBlocksMap) {
        // don't start at return instructions
        if (BBTuple.first != EXIT_MARK && BBTuple.first != UNREACHABLE_MARK) {
            for (auto BB : BBTuple.second) {
                std::map<int, Paths> NewPaths =
                    findPaths(BBTuple.first, BB, BidirMarkBlockMap);
                for (auto NewPathTuple : NewPaths) {
                    MyPaths[BBTuple.first][NewPathTuple.first].insert(
                        MyPaths[BBTuple.first][NewPathTuple.first].end(),
                        NewPathTuple.second.begin(), NewPathTuple.second.end());
                }
            }
        }
    }

    return MyPaths;
}

std::map<int, Paths> findPaths(int For, llvm::BasicBlock *BB,
                               BidirBlockMarkMap BidirBlockMarkMap) {
    std::map<int, Paths> FoundPaths;
    auto MyPaths = traverse(BB, BidirBlockMarkMap, true, {});
    for (auto &PathIt : MyPaths) {
        set<int> Indices;
        if (PathIt.empty()) {
            Indices.insert(EXIT_MARK);
        } else {
            Indices = BidirBlockMarkMap.BlockToMarksMap.at(PathIt.back().Block);
        }
        for (auto Index : Indices) {
            // don't allow paths to the same node but with a different mark
            if (PathIt.empty() ||
                !(PathIt.back().Block == BB && Index != For)) {
                FoundPaths[Index].push_back(Path(BB, PathIt));
            }
        }
    }
    return FoundPaths;
}

Paths_ traverse(llvm::BasicBlock *BB, BidirBlockMarkMap MarkedBlocks,
                bool First, std::set<llvm::BasicBlock *> Visited) {
    if ((!First && isMarked(BB, MarkedBlocks)) || isReturn(BB, MarkedBlocks)) {
        Paths_ MyPaths;
        MyPaths.push_back(Path_());
        return MyPaths;
    }
    if (Visited.find(BB) != Visited.end()) {
        logError("Found cycle at block:");
        BB->print(llvm::errs());
        llvm::errs() << "\n";
        exit(1);
    }
    Visited.insert(BB);
    auto TermInst = BB->getTerminator();
    if (auto BranchInst = llvm::dyn_cast<llvm::BranchInst>(TermInst)) {
        if (BranchInst->isUnconditional()) {
            auto TraversedPaths = traverse(BranchInst->getSuccessor(0),
                                           MarkedBlocks, false, Visited);
            for (auto &P : TraversedPaths) {
                P.insert(P.begin(), Edge(nullptr, BranchInst->getSuccessor(0)));
            }
            return TraversedPaths;
        }
        auto TraversedPaths0 =
            traverse(BranchInst->getSuccessor(0), MarkedBlocks, false, Visited);
        auto TraversedPaths1 =
            traverse(BranchInst->getSuccessor(1), MarkedBlocks, false, Visited);
        for (auto &P : TraversedPaths0) {
            P.insert(P.begin(),
                     Edge(name(BranchInst->getCondition()->getName()),
                          BranchInst->getSuccessor(0)));
        }
        for (auto &P : TraversedPaths1) {
            P.insert(
                P.begin(),
                Edge(makeUnaryOp("not",
                                 name(BranchInst->getCondition()->getName())),
                     BranchInst->getSuccessor(1)));
        }
        for (auto &Path : TraversedPaths1) {
            TraversedPaths0.push_back(Path);
        }
        return TraversedPaths0;
    }
    llvm::errs() << "Unknown terminator\n";
    TermInst->print(llvm::errs());
    llvm::errs() << "\n";
    return Paths_();
}

bool isMarked(llvm::BasicBlock *BB, BidirBlockMarkMap MarkedBlocks) {
    auto Marks = MarkedBlocks.BlockToMarksMap.find(BB);
    if (Marks != MarkedBlocks.BlockToMarksMap.end()) {
        return !(Marks->second.empty());
    }
    return false;
}

bool isReturn(llvm::BasicBlock *BB, BidirBlockMarkMap MarkedBlocks) {
    auto Marks = MarkedBlocks.BlockToMarksMap.find(BB);
    if (Marks != MarkedBlocks.BlockToMarksMap.end()) {
        return Marks->second.find(EXIT_MARK) != Marks->second.end() ||
               llvm::isa<llvm::UnreachableInst>(BB->getTerminator());
    }
    return false;
}

llvm::BasicBlock *lastBlock(Path Path) {
    if (Path.Edges.empty()) {
        return Path.Start;
    }
    return Path.Edges.back().Block;
}
