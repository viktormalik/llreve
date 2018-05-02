/*
 * Class for representing types of variable arguments of function
 *
 * Created by Viktor Malik (vmalik@redhat.com)
 *
 * Published under Apache 2.0 license.
 * See LICENSE for details.
 */

#pragma once

#include <vector>
#include <llvm/IR/Type.h>
#include "Helper.h"

class VarArgs {
  public:
    std::vector<llvm::Type *> argTypes;

    std::string name() const;
    std::vector<smt::SortedVar> toSortedVars() const;

    bool operator==(const VarArgs &rhs) const;
    bool operator<(const VarArgs &rhs) const;
};
