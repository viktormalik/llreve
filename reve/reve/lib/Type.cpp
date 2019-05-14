#include "Type.h"

#include "Helper.h"
#include "Memory.h"
#include "Opts.h"

using std::make_unique;
using std::string;
using std::unique_ptr;
using std::vector;

using namespace sexpr;
using namespace llreve::opts;

namespace smt {
TypeTag BoolType::getTag() const { return TypeTag::Bool; }
TypeTag IntType::getTag() const { return TypeTag::Int; }
TypeTag FloatType::getTag() const { return TypeTag::Float; }
TypeTag ArrayType::getTag() const { return TypeTag::Array; }
TypeTag StructType::getTag() const { return TypeTag::Struct; }

SExprRef BoolType::toSExpr() const { return sexprFromString("Bool"); }
SExprRef IntType::toSExpr() const {
    if (SMTGenerationOpts::getInstance().BitVect) {
        SExprVec args;
        args.push_back(sexprFromString("BitVec"));
        args.push_back(sexprFromString(std::to_string(this->bitWidth)));
        return std::make_unique<Apply>("_", std::move(args));
    } else {
        return sexprFromString("Int");
    }
}
SExprRef FloatType::toSExpr() const {
    if (SMTGenerationOpts::getInstance().BitVect) {
        SExprVec args;
        args.push_back(sexprFromString("FloatingPoint"));
        args.push_back(sexprFromString(std::to_string(this->exponentWidth)));
        args.push_back(sexprFromString(std::to_string(this->significandWidth)));
        return std::make_unique<Apply>("_", std::move(args));
    } else {
        return sexprFromString("Real");
    }
}
SExprRef ArrayType::toSExpr() const {
    SExprVec args;
    args.push_back(domain.toSExpr());
    args.push_back(target.toSExpr());
    return std::make_unique<Apply>("Array", std::move(args));
}

sexpr::SExprRef StructType::toSExpr() const {
    SExprVec args;
    for (const Type &elem : elements)
        args.push_back(elem.toSExpr());
    return std::make_unique<Apply>("Tuple" + std::to_string(elements.size()),
                                   std::move(args));
}

ArrayType memoryType() { return ArrayType(int64Type(), IntType(8)); }

IntType int64Type() { return IntType(64); }

BoolType boolType() { return BoolType(); }

IntType pointerType() { return IntType(64); }

static unsigned semanticsExponent(const llvm::fltSemantics &semantics) {
    return llvm::APFloat::semanticsSizeInBits(semantics) -
           llvm::APFloat::semanticsPrecision(semantics);
}

Type llvmType(const llvm::Type *type) {
    if (type->isPointerTy()) {
        return pointerType();
    } else if (type->isIntegerTy()) {
        if (type->getIntegerBitWidth() == 1) {
            return boolType();
        } else {
            return IntType(type->getIntegerBitWidth());
        }
    } else if (type->isFloatingPointTy()) {
        return FloatType(
            semanticsExponent(type->getFltSemantics()),
            llvm::APFloat::semanticsPrecision(type->getFltSemantics()));
    } else if (type->isVoidTy()) {
        // Void is always a constant zero
        return int64Type();
    } else if (auto structType = llvm::dyn_cast<llvm::StructType>(type)) {
        StructType resultType;
        for (const auto &elem : structType->elements())
            resultType.elements.push_back(llvmType(elem));
        return resultType;
    } else {
        logErrorData("Unsupported type\n", *type);
        exit(1);
    }
}

Type inferTypeByName(string arg) {
    if (arg.find("HEAP$") != std::string::npos
        || arg.find("STACK$") != std::string::npos) {
        return memoryType();
    }
    return int64Type();
}
}
