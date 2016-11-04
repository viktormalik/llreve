/*
 * This file is part of
 *    llreve - Automatic regression verification for LLVM programs
 *
 * Copyright (C) 2016 Karlsruhe Institute of Technology
 *
 * The system is published under a BSD license.
 * See LICENSE (distributed with this file) for details.
 */

#include <iostream>
#include <string>
#include <vector>

#include "Compat.h"
#include "Compile.h"
#include "ModuleSMTGeneration.h"
#include "Opts.h"
#include "Preprocess.h"
#include "Serialize.h"
#include "llreve/dynamic/Analysis.h"
#include "llreve/dynamic/Model.h"
#include "llreve/dynamic/SerializeTraces.h"

#include "clang/Driver/Compilation.h"

#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Transforms/IPO.h"

#include <sys/stat.h>

using std::string;
using std::vector;
using std::shared_ptr;
using std::make_shared;
using std::map;

using clang::CodeGenAction;

using namespace llreve::dynamic;
using namespace llreve::opts;

static llreve::cl::opt<string> FileName1Flag(llreve::cl::Positional,
                                             llreve::cl::desc("FILE1"),
                                             llreve::cl::Required);
static llreve::cl::opt<string> FileName2Flag(llreve::cl::Positional,
                                             llreve::cl::desc("FILE2"),
                                             llreve::cl::Required);
static llreve::cl::opt<bool> // The parser
    BoundedFlag("bounded", llreve::cl::desc("Use bounded integers"));
static llreve::cl::opt<bool> HeapFlag("heap",
                                      llreve::cl::desc("Activate heap"));
static llreve::cl::opt<bool>
    InstantiateFlag("instantiate", llreve::cl::desc("Instantiate arrays"));
static llreve::cl::opt<bool>
    InvertFlag("invert",
               llreve::cl::desc("Check for satisfiability of negation"));
static llreve::cl::opt<bool> CegarFlag("cegar", llreve::cl::desc("Cegar"));
static llreve::cl::opt<string>
    PatternFileFlag("patterns",
                    llreve::cl::desc("Path to file containing patterns"),
                    llreve::cl::Required);
static llreve::cl::opt<string> OutputDirectoryFlag(
    "output",
    llreve::cl::desc("Directory containing the output of the interpreter"));
static llreve::cl::list<string> IncludesFlag("I",
                                             llreve::cl::desc("Include path"));
static llreve::cl::opt<string> ResourceDirFlag(
    "resource-dir",
    llreve::cl::desc("Directory containing the clang resource files, "
                     "e.g. /usr/local/lib/clang/3.8.0"));

static llreve::cl::opt<bool> ShowCFGFlag("show-cfg",
                                         llreve::cl::desc("Show cfg"));
static llreve::cl::opt<bool>
    ShowMarkedCFGFlag("show-marked-cfg",
                      llreve::cl::desc("Show cfg before mark removal"));

static llreve::cl::opt<string> MainFunctionFlag(
    "fun", llreve::cl::desc("Name of the function which should be verified"),
    llreve::cl::Required);
// Serialize flags
static llreve::cl::opt<string>
    OutputFileNameFlag("o", llreve::cl::desc("SMT output filename"),
                       llreve::cl::value_desc("filename"),
                       llreve::cl::Required);
static llreve::cl::opt<bool>
    MergeImplications("merge-implications",
                      llreve::cl::desc("Merge implications"));

int main(int argc, const char **argv) {
    llreve::cl::ParseCommandLineOptions(argc, argv);
    InputOpts inputOpts(IncludesFlag, ResourceDirFlag, FileName1Flag,
                        FileName2Flag);
    PreprocessOpts preprocessOpts(ShowCFGFlag, ShowMarkedCFGFlag, false);

    MonoPair<shared_ptr<CodeGenAction>> acts =
        makeMonoPair(make_shared<clang::EmitLLVMOnlyAction>(),
                     make_shared<clang::EmitLLVMOnlyAction>());
    MonoPair<shared_ptr<llvm::Module>> modules =
        compileToModules(argv[0], inputOpts, acts);
    llvm::legacy::PassManager PM;
    PM.add(llvm::createStripSymbolsPass(true));
    PM.run(*modules.first);
    PM.run(*modules.second);
    MonoPair<llvm::Module &> moduleRefs = {*modules.first, *modules.second};

    if (CegarFlag) {
        InvertFlag = true;
    }

    SMTGenerationOpts::initialize(
        findMainFunction(moduleRefs, MainFunctionFlag),
        HeapFlag ? llreve::opts::Heap::Enabled : llreve::opts::Heap::Disabled,
        Stack::Disabled, GlobalConstants::Disabled, FunctionEncoding::Iterative,
        ByteHeap::Enabled, false, SMTFormat::SMTHorn,
        PerfectSynchronization::Disabled, false, BoundedFlag, InvertFlag, false,
        false, {}, {}, {}, {}, inferCoupledFunctionsByName(moduleRefs),
        generateFunctionMap(moduleRefs));

    AnalysisResultsMap analysisResults =
        preprocessModules(moduleRefs, preprocessOpts);
    // fopen doesn’t signal if the path points to a directory, thus we have to
    // check for that separately and to catch the error.
    struct stat s;
    int ret = stat(PatternFileFlag.c_str(), &s);
    if (ret != 0) {
        logError("Couldn’t open pattern file\n");
        exit(1);
    }
    if (s.st_mode & S_IFDIR) {
        logError("Pattern file points to a directory\n");
        exit(1);
    }

    FILE *patternFile = fopen(PatternFileFlag.c_str(), "r");
    if (patternFile == nullptr) {
        logError("Couldn’t open pattern file\n");
        exit(1);
    }
    auto patterns = parsePatterns(patternFile);
    std::cerr << "Found " << patterns.size() << " patterns\n";
    for (auto pat : patterns) {
        pat->dump(std::cerr);
        std::cerr << "\n";
    }
    fclose(patternFile);

    FileOptions fileOpts = getFileOptions(inputOpts.FileNames);
    vector<smt::SharedSMTRef> smtExprs;
    if (CegarFlag) {
        smtExprs = cegarDriver(moduleRefs, analysisResults, patterns, fileOpts);
    } else {
        smtExprs = driver(moduleRefs, analysisResults, patterns, fileOpts);
    }
    if (!smtExprs.empty()) {
        serializeSMT(smtExprs, false,
                     SerializeOpts(OutputFileNameFlag, !InstantiateFlag,
                                   MergeImplications, true, false));
    }

    llvm::llvm_shutdown();
}