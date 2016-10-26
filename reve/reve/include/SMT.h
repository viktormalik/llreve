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

#include "SExpr.h"
#include "Type.h"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Instruction.h"

#include <map>
#include <set>
#include <sstream>
#include <string>

#include "z3++.h"

namespace smt {

// forward declare
class SortedVar;

struct HeapInfo {
    std::string arrayName;
    std::string index;
    std::string suffix;
    HeapInfo(std::string arrayName, std::string index, std::string suffix)
        : arrayName(arrayName), index(index), suffix(suffix) {}
};

struct Z3DefineFun {
    z3::expr_vector vars;
    z3::expr e;
};

class SMTExpr : public std::enable_shared_from_this<SMTExpr> {
  public:
    SMTExpr(const SMTExpr & /*unused*/) = default;
    SMTExpr &operator=(SMTExpr &) = delete;
    SMTExpr() = default;
    virtual ~SMTExpr() = default;
    virtual SExprRef toSExpr() const = 0;
    virtual std::set<std::string> uses() const;
    virtual std::shared_ptr<const SMTExpr> compressLets(
        std::vector<std::pair<std::string, std::shared_ptr<const SMTExpr>>>
            defs = std::vector<
                std::pair<std::string, std::shared_ptr<const SMTExpr>>>())
        const;
    virtual std::vector<std::shared_ptr<const SMTExpr>>
    splitConjunctions() const;
    // Rename assignments to unique names. This allows moving things around as
    // done by mergeImplications.
    virtual std::shared_ptr<const SMTExpr>
    renameAssignments(std::map<std::string, int> variableMap) const;
    virtual std::shared_ptr<const SMTExpr> mergeImplications(
        std::vector<std::shared_ptr<const SMTExpr>> conditions) const;
    virtual std::shared_ptr<const SMTExpr> instantiateArrays() const;
    virtual std::unique_ptr<const HeapInfo> heapInfo() const;
    // This removes foralls and declares them as global variables. This is
    // needed for the z3 muz format.
    virtual std::shared_ptr<const SMTExpr>
    removeForalls(std::set<SortedVar> &introducedVariables) const;
    virtual std::shared_ptr<const SMTExpr> inlineLets(
        std::map<std::string, std::shared_ptr<const SMTExpr>> assignments)
        const;
    virtual void toZ3(z3::context &cxt, z3::solver &solver,
                      std::map<std::string, z3::expr> &nameMap,
                      std::map<std::string, Z3DefineFun> &defineFunMap) const;
    virtual z3::expr
    toZ3Expr(z3::context &cxt, std::map<std::string, z3::expr> &nameMap,
             const std::map<std::string, Z3DefineFun> &defineFunMap) const;
};

using SharedSMTRef = std::shared_ptr<const SMTExpr>;
using SMTRef = std::unique_ptr<const SMTExpr>;
using Assignment = std::pair<std::string, SharedSMTRef>;
auto makeAssignment(std::string name, SharedSMTRef val)
    -> std::unique_ptr<const Assignment>;

class SetLogic : public SMTExpr {
  public:
    explicit SetLogic(std::string logic) : logic(std::move(logic)) {}
    SExprRef toSExpr() const override;
    std::string logic;
};

class Assert : public SMTExpr {
  public:
    explicit Assert(SharedSMTRef expr) : expr(std::move(expr)) {}
    SharedSMTRef expr;
    SExprRef toSExpr() const override;
    std::set<std::string> uses() const override;
    SharedSMTRef
    removeForalls(std::set<SortedVar> &introducedVariables) const override;
    SharedSMTRef compressLets(std::vector<Assignment> defs) const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    mergeImplications(std::vector<SharedSMTRef> conditions) const override;
    std::vector<SharedSMTRef> splitConjunctions() const override;
    SharedSMTRef instantiateArrays() const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
    void toZ3(z3::context &cxt, z3::solver &solver,
              std::map<std::string, z3::expr> &nameMap,
              std::map<std::string, Z3DefineFun> &defineFunMap) const override;
};

// TypedVariable is simply a reference to a variable with a type attached to it,
// SortedVar is used in binders where the type should also be serialized, e.g.
// forall
class TypedVariable : public SMTExpr {
  public:
    std::string name;
    std::unique_ptr<Type> type;
    TypedVariable(std::string name, std::unique_ptr<Type> type)
        : name(std::move(name)), type(std::move(type)) {}
    std::unique_ptr<const HeapInfo> heapInfo() const override;
    SExprRef toSExpr() const override;
    std::set<std::string> uses() const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
};

class SortedVar {
  public:
    std::string name;
    std::unique_ptr<Type> type;
    SortedVar(std::string name, std::unique_ptr<Type> type)
        : name(std::move(name)), type(std::move(type)) {}
    SExprRef toSExpr() const;
    std::set<std::string> uses() const;
    SortedVar &operator=(const SortedVar &other) {
        name = other.name;
        type = other.type->copy();
        return *this;
    }
    SortedVar(const SortedVar &other)
        : name(other.name), type(other.type->copy()) {}
};

inline bool operator<(const SortedVar &lhs, const SortedVar &rhs) {
    return lhs.name < rhs.name;
}

inline bool operator>(const SortedVar &lhs, const SortedVar &rhs) {
    return rhs < lhs;
}

inline bool operator<=(const SortedVar &lhs, const SortedVar &rhs) {
    return !(lhs > rhs);
}

inline bool operator>=(const SortedVar &lhs, const SortedVar &rhs) {
    return !(lhs < rhs);
}

inline bool operator==(const SortedVar &lhs, const SortedVar &rhs) {
    return lhs.name == rhs.name;
}

inline bool operator!=(const SortedVar &lhs, const SortedVar &rhs) {
    return !(lhs == rhs);
}

class Forall : public SMTExpr {
  public:
    std::vector<SortedVar> vars;
    SharedSMTRef expr;
    Forall(std::vector<SortedVar> vars, SharedSMTRef expr)
        : vars(std::move(vars)), expr(std::move(expr)) {}
    SExprRef toSExpr() const override;
    std::set<std::string> uses() const override;
    SharedSMTRef
    removeForalls(std::set<SortedVar> &introducedVariables) const override;
    SharedSMTRef compressLets(std::vector<Assignment> defs) const override;
    SharedSMTRef instantiateArrays() const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    mergeImplications(std::vector<SharedSMTRef> conditions) const override;
    std::vector<SharedSMTRef> splitConjunctions() const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
};

class CheckSat : public SMTExpr {
  public:
    SExprRef toSExpr() const override;
    SharedSMTRef compressLets(std::vector<Assignment> defs) const override;
    void toZ3(z3::context &cxt, z3::solver &solver,
              std::map<std::string, z3::expr> &nameMap,
              std::map<std::string, Z3DefineFun> &defineFunMap) const override;
};

class GetModel : public SMTExpr {
  public:
    SExprRef toSExpr() const override;
    SharedSMTRef compressLets(std::vector<Assignment> defs) const override;
    void toZ3(z3::context &cxt, z3::solver &solver,
              std::map<std::string, z3::expr> &nameMap,
              std::map<std::string, Z3DefineFun> &defineFunMap) const override;
};

class Let : public SMTExpr {
  public:
    std::vector<Assignment> defs;
    SharedSMTRef expr;
    Let(std::vector<Assignment> defs, SharedSMTRef expr)
        : defs(std::move(defs)), expr(std::move(expr)) {}
    SExprRef toSExpr() const override;
    SharedSMTRef
    removeForalls(std::set<SortedVar> &introducedVariables) const override;
    std::set<std::string> uses() const override;
    SharedSMTRef
    compressLets(std::vector<Assignment> passedDefs) const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    mergeImplications(std::vector<SharedSMTRef> conditions) const override;
    std::vector<SharedSMTRef> splitConjunctions() const override;
    SharedSMTRef instantiateArrays() const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
    z3::expr toZ3Expr(
        z3::context &cxt, std::map<std::string, z3::expr> &nameMap,
        const std::map<std::string, Z3DefineFun> &defineFunMap) const override;
};

// We could unify these in a generic type but it’s probably not worth the
// trouble
class ConstantFP : public SMTExpr {
  public:
    llvm::APFloat value;
    explicit ConstantFP(const llvm::APFloat value) : value(value) {}
    SExprRef toSExpr() const override;
};

class ConstantInt : public SMTExpr {
  public:
    llvm::APInt value;
    explicit ConstantInt(const llvm::APInt value) : value(value) {}
    SExprRef toSExpr() const override;
};

class ConstantBool : public SMTExpr {
  public:
    bool value;
    explicit ConstantBool(bool value) : value(value) {}
    SExprRef toSExpr() const override;
    SharedSMTRef compressLets(std::vector<Assignment> defs) const override;
};

// This is for constants or expressions that have not been parsed and should
// just be included literally
class ConstantString : public SMTExpr {
  public:
    std::string value;
    explicit ConstantString(std::string value) : value(value) {}
    SExprRef toSExpr() const override; //  {
    std::set<std::string> uses() const override;
    SharedSMTRef compressLets(std::vector<Assignment> defs) const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
    z3::expr toZ3Expr(
        z3::context &cxt, std::map<std::string, z3::expr> &nameMap,
        const std::map<std::string, Z3DefineFun> &defineFunMap) const override;
};

class Op : public SMTExpr {
  public:
    Op(std::string opName, std::vector<SharedSMTRef> args)
        : opName(std::move(opName)), args(std::move(args)), instantiate(true) {}
    Op(std::string opName, std::vector<SharedSMTRef> args, bool instantiate)
        : opName(std::move(opName)), args(std::move(args)),
          instantiate(instantiate) {}
    std::string opName;
    std::vector<SharedSMTRef> args;
    bool instantiate;
    SExprRef toSExpr() const override;
    SharedSMTRef
    removeForalls(std::set<SortedVar> &introducedVariables) const override;
    std::set<std::string> uses() const override;
    SharedSMTRef compressLets(std::vector<Assignment> defs) const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    mergeImplications(std::vector<SharedSMTRef> conditions) const override;
    std::vector<SharedSMTRef> splitConjunctions() const override;
    SharedSMTRef instantiateArrays() const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
    z3::expr toZ3Expr(
        z3::context &cxt, std::map<std::string, z3::expr> &nameMap,
        const std::map<std::string, Z3DefineFun> &defineFunMap) const override;
};

class FPCmp : public SMTExpr {
  public:
    enum class Predicate {
        False,
        OEQ,
        OGT,
        OGE,
        OLT,
        OLE,
        ONE,
        ORD,
        UNO,
        UEQ,
        UGT,
        UGE,
        ULT,
        ULE,
        UNE,
        True
    };
    // Operations that take two floats and return a bool
    Predicate op;
    // This is the type of the two arguments that an fcmp instruction retrieves
    // (they have to be identical)
    std::unique_ptr<Type> type;
    SharedSMTRef op0;
    SharedSMTRef op1;
    FPCmp(Predicate op, std::unique_ptr<Type> type, SharedSMTRef op0,
          SharedSMTRef op1)
        : op(op), type(std::move(type)), op0(op0), op1(op1) {}
    SExprRef toSExpr() const override;
    std::set<std::string> uses() const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
};

class BinaryFPOperator : public SMTExpr {
  public:
    enum class Opcode { FAdd, FSub, FMul, FDiv, FRem };
    Opcode op;
    std::unique_ptr<Type> type;
    SharedSMTRef op0;
    SharedSMTRef op1;
    BinaryFPOperator(Opcode op, std::unique_ptr<Type> type, SharedSMTRef op0,
                     SharedSMTRef op1)
        : op(op), type(std::move(type)), op0(op0), op1(op1) {}
    SExprRef toSExpr() const override;
    std::set<std::string> uses() const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
};

class TypeCast : public SMTExpr {
  public:
    llvm::Instruction::CastOps op;
    std::unique_ptr<Type> sourceType;
    std::unique_ptr<Type> destType;
    SharedSMTRef operand;
    TypeCast(llvm::Instruction::CastOps op, std::unique_ptr<Type> sourceType,
             std::unique_ptr<Type> destType, SharedSMTRef operand)
        : op(op), sourceType(std::move(sourceType)),
          destType(std::move(destType)), operand(operand) {}
    SExprRef toSExpr() const override;
    std::set<std::string> uses() const override;
    SharedSMTRef
    renameAssignments(std::map<std::string, int> variableMap) const override;
    SharedSMTRef
    inlineLets(std::map<std::string, SharedSMTRef> assignments) const override;
};

class Query : public SMTExpr {
  public:
    Query(std::string queryName) : queryName(std::move(queryName)) {}
    std::string queryName;
    SExprRef toSExpr() const override;
};

auto stringExpr(std::string name) -> std::unique_ptr<ConstantString>;

auto makeSMTRef(SharedSMTRef arg) -> SharedSMTRef;
auto makeSMTRef(std::string arg) -> SharedSMTRef;

template <typename... Args>
auto makeOp(std::string opName, Args... args) -> std::unique_ptr<Op> {
    std::vector<SharedSMTRef> args_ = {makeSMTRef(std::move(args))...};
    return std::make_unique<Op>(opName, args_);
}

auto makeOp(std::string opName, std::vector<std::string> args)
    -> std::unique_ptr<const Op>;

class FunDecl : public SMTExpr {
  public:
    FunDecl(std::string funName, std::vector<std::unique_ptr<Type>> inTypes,
            std::unique_ptr<Type> outType)
        : funName(std::move(funName)), inTypes(std::move(inTypes)),
          outType(std::move(outType)) {}
    std::string funName;
    std::vector<std::unique_ptr<Type>> inTypes;
    std::unique_ptr<Type> outType;
    SExprRef toSExpr() const override;
    SharedSMTRef instantiateArrays() const override;
};

class FunDef : public SMTExpr {
  public:
    FunDef(std::string funName, std::vector<SortedVar> args,
           std::unique_ptr<Type> outType, SharedSMTRef body)
        : funName(std::move(funName)), args(std::move(args)),
          outType(std::move(outType)), body(std::move(body)) {}
    std::string funName;
    std::vector<SortedVar> args;
    std::unique_ptr<Type> outType;
    SharedSMTRef body;
    SExprRef toSExpr() const override;
    SharedSMTRef instantiateArrays() const override;
    void toZ3(z3::context &cxt, z3::solver &solver,
              std::map<std::string, z3::expr> &nameMap,
              std::map<std::string, Z3DefineFun> &defineFunMap) const override;
};

class Comment : public SMTExpr {
  public:
    Comment(std::string val) : val(std::move(val)) {}
    std::string val;
    SExprRef toSExpr() const override;
};

class VarDecl : public SMTExpr {
  public:
    VarDecl(SortedVar var) : var(std::move(var)) {}
    SortedVar var;
    SExprRef toSExpr() const override;
    void toZ3(z3::context &cxt, z3::solver &solver,
              std::map<std::string, z3::expr> &nameMap,
              std::map<std::string, Z3DefineFun> &defineFunMap) const override;
};

auto nestLets(SharedSMTRef clause, std::vector<Assignment> defs)
    -> SharedSMTRef;

bool isArray(const Type &type);

std::unique_ptr<SMTExpr> memoryVariable(std::string name);
std::unique_ptr<TypedVariable> typedVariableFromSortedVar(const SortedVar &var);
}
