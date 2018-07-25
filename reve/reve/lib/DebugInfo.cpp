/*
 * Processing of debug information
 *
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#include "DebugInfo.h"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <Helper.h>

using namespace llvm;

PreservedAnalyses RemoveDebugInfoPass::run(
        Function &Fun, FunctionAnalysisManager &fam) {
    std::vector<Instruction *> toRemove;
    for (auto &BB : Fun) {
        for (auto &Instr : BB) {
            if (isDebugInfo(Instr)) {
                toRemove.push_back(&Instr);
            }
        }
    }
    for (auto Instr : toRemove)
        Instr->eraseFromParent();
    return PreservedAnalyses();
}

bool isDebugInfo(const Instruction &Instr) {
    if (auto CallInstr = dyn_cast<CallInst>(&Instr)) {
        auto fun = CallInstr->getCalledFunction();
        if (fun)
            return isDebugInfo(*fun);
    }
    return false;
}

bool isDebugInfo(const Function &Fun) {
    return Fun.getIntrinsicID() == Intrinsic::dbg_declare ||
           Fun.getIntrinsicID() == Intrinsic::dbg_value;
}

std::string getStructTypeName(const llvm::StructType *type) {
    std::string name = type->getName();
    name.erase(0, std::string("struct.").length());
    if (name.find_last_of(".") != std::string::npos)
        name.erase(name.find_last_of("."));
    return name;
}

DICompositeType *DebugInfo::getStructTypeInfo(const llvm::StringRef name,
                                              const Program prog) const {
    auto types = prog == Program::First ? DebugInfoFirst.types()
                                        : DebugInfoSecond.types();
    for (auto Type : types) {
        if (auto StructType = dyn_cast<DICompositeType>(Type)) {
            if (StructType->getName() == name)
                return StructType;
        }
    }
    return nullptr;
}

void DebugInfo::calculateGEPIndexAlignments() {
    // Check if any debug info was collected
    if (DebugInfoFirst.type_count() == 0 || DebugInfoSecond.type_count() == 0)
        return;

    for (auto &Fun : ModFirst) {
        if (&Fun != mainFuns.first && !callsTransitively(*mainFuns.first, Fun))
            continue;

        for (auto &BB : Fun) {
            for (auto &Instr : BB) {
                if (auto GEP = dyn_cast<GetElementPtrInst>(&Instr)) {
                    GEP->dump();
                    std::vector<llvm::Value *> indices;
                    // Iterate all indices
                    for (auto idx = GEP->idx_begin();
                         idx != GEP->idx_end(); ++idx) {
                        auto indexedType =
                                GEP->getIndexedType(GEP->getSourceElementType(),
                                                    ArrayRef<llvm::Value *>(
                                                            indices));
                        if (!indexedType->isStructTy()) {
                            indices.push_back(*idx);
                            continue;
                        }

                        if (auto IndexConstant = dyn_cast<ConstantInt>(*idx)) {
                            // Numeric value of the current index
                            uint64_t indexFirst = IndexConstant->getZExtValue();

                            // Name of the current type (type being indexed)
                            if (!dyn_cast<StructType>(indexedType)->hasName())
                                continue;
                            std::string typeName = getStructTypeName(
                                    dyn_cast<StructType>(indexedType));

                            // Get name of the element at the current index in
                            // the first module
                            auto TypeDIFirst =
                                    getStructTypeInfo(typeName, Program::First);
                            if (!TypeDIFirst)
                                continue;

                            StringRef elementName =
                                    getElementNameAtIndex(*TypeDIFirst,
                                                          indexFirst);

                            // Find index of the element with the same name in
                            // the second module
                            auto TypeDISecond =
                                    getStructTypeInfo(typeName,
                                                      Program::Second);
                            if (!TypeDISecond)
                                continue;

                            int indexSecond =
                                    getTypeMemberIndex(*TypeDISecond,
                                                       elementName);

                            // If indices do not match, align the first one to
                            // be the same as the second one
                            if (indexSecond > 0 && indexFirst != indexSecond) {
                                setNewAlignmentOfIndex(
                                        *GEP, indices.size(),
                                        (unsigned) indexSecond,
                                        IndexConstant->getBitWidth(),
                                        ModFirst.getContext());
                                errs() << "New index: " << indexSecond << "\n";
                            }
                        }
                        indices.push_back(*idx);
                    }
                }
            }
        }
    }
}

bool DebugInfo::isSameElemIndex(const DIDerivedType *TypeElem) {
    if (TypeElem->getFlag("DIFlagBitField") &&
        TypeElem->getExtraData()) {
        if (auto ExtraDataValue = dyn_cast<ConstantAsMetadata>(
                TypeElem->getExtraData())) {
            if (auto ExtraDataConst = dyn_cast<ConstantInt>(
                    ExtraDataValue->getValue())) {
                if (ExtraDataConst->getZExtValue() !=
                    TypeElem->getOffsetInBits())
                    return true;
            }
        }
    }
    return false;
}

int DebugInfo::getTypeMemberIndex(const DICompositeType &type,
                                  const StringRef name) {
    unsigned index = 0;
    for (auto Elem : type.getElements()) {
        if (auto TypeElem = dyn_cast<DIDerivedType>(Elem)) {
            if (isSameElemIndex(TypeElem))
                index--;

            if (TypeElem->getName() == name)
                return index;
        }
        index++;
    }
    return -1;
}

StringRef DebugInfo::getElementNameAtIndex(const DICompositeType &type,
                                           uint64_t index) {


    unsigned currentIndex = 0;
    for (auto Elem : type.getElements()) {
        if (auto TypeElem = dyn_cast<DIDerivedType>(Elem)) {
            if (currentIndex == index)
                return TypeElem->getName();

            if (!isSameElemIndex(TypeElem))
                currentIndex++;
        }
    }
    return "";
}

void DebugInfo::setNewAlignmentOfIndex(llvm::GetElementPtrInst &GEP,
                                       unsigned long index,
                                       uint64_t alignment,
                                       unsigned bitWidth,
                                       llvm::LLVMContext &c) {
    MDNode *MD = MDNode::get(c, ConstantAsMetadata::get(
            ConstantInt::get(c, APInt(bitWidth, alignment, false))));
    GEP.setMetadata("idx_align_" + std::to_string(index), MD);
}
