/*
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#include "VarArgs.h"

std::string VarArgs::name() const {
    std::string result;
    for (auto arg : argTypes) {
        result += "_" + typeName(arg);
    }
    return result;
}

bool VarArgs::operator==(const VarArgs &rhs) const {
    if (argTypes.size() != rhs.argTypes.size())
        return false;
    for (unsigned i = 0; i < argTypes.size(); ++i) {
        if (typeName(argTypes[i]) != typeName(rhs.argTypes[i]))
            return false;
    }
    return true;
}

bool VarArgs::operator<(const VarArgs &rhs) const {
    if (argTypes.size() < rhs.argTypes.size())
        return true;
    else if (argTypes.size() > rhs.argTypes.size())
        return false;
    else
        return name() < rhs.name();
}

std::vector<smt::SortedVar> VarArgs::toSortedVars() const {
    std::vector<smt::SortedVar> args;
    unsigned i = 0;
    for (auto type : argTypes) {
        std::string name = "$VarArg$" + std::to_string(i++);
        auto sVar = smt::SortedVar(name, smt::llvmType(type));
        args.push_back(sVar);
        if (llreve::opts::SMTGenerationOpts::getInstance().Stack ==
            llreve::opts::StackOpt::Enabled &&
            type->isPointerTy()) {
            args.push_back({name + "_OnStack", smt::boolType()});
        }
    }
    return args;
}
