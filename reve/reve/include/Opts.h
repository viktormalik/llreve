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

#include "CommandLine.h"
#include "MarkAnalysis.h"
#include "MonoPair.h"
#include "SMT.h"

#include "llvm/IR/Function.h"

#include <map>

namespace llreve {

template <typename T> struct FunctionInvariant {
    T preCondition;
    T postCondition;
};

namespace opts {
extern llreve::cl::OptionCategory ReveCategory;

/// Options used for preprocessing modules
class PreprocessOpts {
  public:
    bool ShowCFG;
    bool ShowMarkedCFG;
    bool InferMarks;
    PreprocessOpts(bool showCFG, bool showMarkedCFG, bool inferMarks)
        : ShowCFG(showCFG), ShowMarkedCFG(showMarkedCFG),
          InferMarks(inferMarks) {}
};

enum class HeapOpt { Enabled, Disabled };

enum class StackOpt { Enabled, Disabled };

enum class GlobalConstantsOpt { Enabled, Disabled };

enum class FunctionEncoding { OnlyRecursive, Iterative };

enum class ByteHeapOpt { Enabled, Disabled };
enum class SMTFormat { Z3, SMTHorn };
enum class PerfectSynchronization { Enabled, Disabled };

/// Singleton for the options used for SMT generation to avoid having to pass
/// around the config object
class SMTGenerationOpts {
  public:
    static SMTGenerationOpts &getInstance() {
        static SMTGenerationOpts instance;
        return instance;
    }
    // Convenience method to make sure you don’t forget to set parameters
    static void initialize(
        MonoPair<llvm::Function *> mainFunctions, HeapOpt heap, StackOpt stack,
        GlobalConstantsOpt globalConstants, FunctionEncoding onlyRecursive,
        ByteHeapOpt byteHeap, bool everythingSigned, SMTFormat muZ,
        PerfectSynchronization perfectSync, bool passInputThrough, bool bitvect,
        bool invert, bool initPredicate, bool disableAutoAbstraction,
        std::map<Mark, smt::SharedSMTRef> iterativeRelationalInvariants,
        std::map<const llvm::Function *,
                 std::map<Mark, FunctionInvariant<smt::SharedSMTRef>>>
            functionalFunctionalInvariants,
        std::map<MonoPair<const llvm::Function *>,
                 std::map<Mark, FunctionInvariant<smt::SharedSMTRef>>>
            functionalRelationalInvariants,
        std::set<MonoPair<const llvm::Function *>> assumeEquivalent,
        std::set<MonoPair<llvm::Function *>> coupleFunctions,
        std::map<const llvm::Function *, int> functionNumerals,
        MonoPair<std::map<int, const llvm::Function *>>
            reversedFunctionNumerals);
    MonoPair<llvm::Function *> MainFunctions = {nullptr, nullptr};
    HeapOpt Heap;
    StackOpt Stack;
    GlobalConstantsOpt GlobalConstants;
    FunctionEncoding OnlyRecursive;
    ByteHeapOpt ByteHeap;
    bool EverythingSigned;
    SMTFormat OutputFormat;
    PerfectSynchronization PerfectSync;
    bool PassInputThrough;
    bool BitVect;
    bool Invert;
    bool InitPredicate;
    bool DisableAutoAbstraction;
    // If an invariant is not in the map a declaration is added and it’s up to
    // the SMT solver to find it
    std::map<Mark, smt::SharedSMTRef> IterativeRelationalInvariants;
    // These are the invariants used for nonmutual function calls
    std::map<const llvm::Function *,
             std::map<Mark, FunctionInvariant<smt::SharedSMTRef>>>
        FunctionalFunctionalInvariants;
    // These are the invarians for coupled function calls
    std::map<MonoPair<const llvm::Function *>,
             std::map<Mark, FunctionInvariant<smt::SharedSMTRef>>>
        FunctionalRelationalInvariants;
    std::set<MonoPair<const llvm::Function *>> AssumeEquivalent;
    // The order in the pairs is normalized so that the first function is always
    // in the first module.
    std::set<MonoPair<llvm::Function *>> CoupledFunctions;
    // This map is used when serializing in inverted mode to identify which
    // functions a clause belongs to
    std::map<const llvm::Function *, int> FunctionNumerals;
    // This is just a reversed version of the above map separated by module
    MonoPair<std::map<int, const llvm::Function *>> ReversedFunctionNumerals = {
        {}, {}};

  private:
    SMTGenerationOpts() = default;
    SMTGenerationOpts(SMTGenerationOpts const &) = delete;
    void operator=(SMTGenerationOpts const &) = delete;
};

/// Options used for reading the C source and compiling it to llvm modules
class InputOpts {
  public:
    std::vector<std::string> Includes;
    std::string ResourceDir;
    MonoPair<std::string> FileNames;
    InputOpts(std::vector<std::string> includes, std::string resourceDir,
              std::string file1, std::string file2)
        : Includes(includes), ResourceDir(resourceDir),
          FileNames(makeMonoPair(file1, file2)) {}
};

/// Options used for serializing the SMT
class SerializeOpts {
  public:
    std::string OutputFileName;
    bool DontInstantiate;
    bool MergeImplications;
    bool Pretty;
    bool InlineLets;
    SerializeOpts(std::string outputFileName, bool DontInstantiate,
                  bool MergeImplications, bool Pretty, bool InlineLets)
        : OutputFileName(outputFileName), DontInstantiate(DontInstantiate),
          MergeImplications(MergeImplications), Pretty(Pretty),
          InlineLets(InlineLets) {}
};

/// Options that are parsed from special comments inside the programs
/// Currently this consists of custom relations and preconditions
class FileOptions {
  public:
    // map from function names to conditions
    std::multimap<std::string, std::string> FunctionConditions;
    smt::SharedSMTRef InRelation;
    smt::SharedSMTRef OutRelation;
    // Indicates if InRelation is meant to replace the existing relation or just
    // enhance it
    bool AdditionalInRelation;
    FileOptions(std::multimap<std::string, std::string> functionConditions,
                smt::SharedSMTRef inRelation, smt::SharedSMTRef outRelation,
                bool additionalIn)
        : FunctionConditions(functionConditions), InRelation(inRelation),
          OutRelation(outRelation), AdditionalInRelation(additionalIn) {}
};

/// If inline-opts is passed this will try to read arguments from the files.
/// In that case the files have to come before any other option that takes an
/// argument.
auto parseCommandLineArguments(int argc, const char **argv) -> void;

// Search for command line options that are specified inside the programs
auto getInlineOpts(const char *file1, const char *file2)
    -> std::vector<std::string>;

// Search for options that can only be specified as special comments inside the
// programs
auto getFileOptions(MonoPair<std::string> fileNames) -> FileOptions;

auto searchCustomRelations(MonoPair<std::string> fileNames, bool &additionalIn)
    -> MonoPair<smt::SharedSMTRef>;
auto searchCustomRelationsInFile(std::string file, smt::SharedSMTRef &in,
                                 smt::SharedSMTRef &out, bool &additionalIn)
    -> void;
auto searchFunctionConditions(MonoPair<std::string> fileNames)
    -> std::multimap<std::string, std::string>;
auto searchFunctionConditionsInFile(std::string file)
    -> std::multimap<std::string, std::string>;
auto parseFunctionPairFlags(llreve::cl::list<std::string> &functionPairFlag)
    -> std::set<MonoPair<std::string>>;
// Depending on the value of disableAutoCoupling this will infer functions to be
// coupled based on their name or use the function names in the
// `coupledFunctionNames`
auto getCoupledFunctions(MonoPair<llvm::Module &> modules,
                         bool disableAutoCoupling,
                         std::set<MonoPair<std::string>> coupledFunctionNames)
    -> std::set<MonoPair<llvm::Function *>>;
auto inferCoupledFunctionsByName(MonoPair<llvm::Module &> modules)
    -> std::set<MonoPair<llvm::Function *>>;
// This function will exit the program if it fails so only use this for input
// validation
auto lookupFunctionNamePairs(
    MonoPair<llvm::Module &> modules,
    std::set<MonoPair<std::string>> coupledFunctionNames)
    -> std::set<MonoPair<llvm::Function *>>;
MonoPair<llvm::Function *> findMainFunction(MonoPair<llvm::Module &> modules,
                                            std::string functionName);

bool isLlreveIntrinsic(const llvm::Function &);
bool isIntrinsicSupported(const llvm::Function &Fun);
bool hasMutualFixedAbstraction(MonoPair<const llvm::Function *> functions);
bool hasFixedAbstraction(const llvm::Function &function);
auto addConstToFunctionPairSet(
    std::set<MonoPair<llvm::Function *>> functionPairs)
    -> std::set<MonoPair<const llvm::Function *>>;

// These maps are needed when serializing in inverted mode to mark the functions
// a clause belongs to
auto generateFunctionMap(MonoPair<const llvm::Module &> modules)
    -> std::pair<std::map<const llvm::Function *, int>,
                 MonoPair<std::map<int, const llvm::Function *>>>;
}
}
