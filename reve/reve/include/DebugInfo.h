/*
 * Processing of debug information
 *
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#pragma once

#include "MonoPair.h"
#include "AnalysisResults.h"
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/TypeFinder.h>
#include <llvm/IR/Instructions.h>
#include <set>

class DebugInfo {
  public:
    DebugInfo(MonoPair<llvm::Module &> modules) : ModFirst(modules.first),
                                                  ModSecond(modules.second) {
        DebugInfoFirst.processModule(modules.first);
        DebugInfoSecond.processModule(modules.second);
        calculateGEPIndexAlignments();
    };

  private:
    llvm::Module &ModFirst;
    llvm::Module &ModSecond;
    llvm::DebugInfoFinder DebugInfoFirst;
    llvm::DebugInfoFinder DebugInfoSecond;

    // For each GEP instruction, check if the accessed struct members of
    // the same name have the same alignment in both modules. If not, add
    // metadata to the instruction of one module containing new value of
    // the alignment
    void calculateGEPIndexAlignments();

    // Get debug info for struct type with given name
    llvm::DICompositeType *getStructTypeInfo(const llvm::StringRef name,
                                             const Program prog) const;

    // Get index of the struct member having given name
    static int getTypeMemberIndex(const llvm::DICompositeType &type,
                                  const llvm::StringRef name);
    // Get name of the struct member on the given index
    static llvm::StringRef getElementNameAtIndex(
            const llvm::DICompositeType &type,
            uint64_t index);
    // Add metadata with new offset to the GEP instruction
    static void setNewAlignmentOfIndex(llvm::GetElementPtrInst &GEP,
                                       unsigned long index,
                                       uint64_t alignment,
                                       unsigned bitWidth,
                                       llvm::LLVMContext &c);
    // Check if the struct element has same index as the previous element (this
    // is caused by the compiler due to struct alignment)
    static bool isSameElemIndex(const llvm::DIDerivedType *TypeElem);
};

class RemoveDebugInfoPass : public llvm::PassInfoMixin<RemoveDebugInfoPass> {
  public:
    llvm::PreservedAnalyses run(llvm::Function &Fun,
                                llvm::FunctionAnalysisManager &fam);
};

bool isDebugInfo(const llvm::Function &Fun);
bool isDebugInfo(const llvm::Instruction &Instr);

std::string getStructTypeName(const llvm::StructType *type);
