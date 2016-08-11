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

#include "SMT.h"

#include <regex>

using Memory = uint8_t;
const uint8_t HEAP_MASK = 0x01;
const uint8_t STACK_MASK = 0x02;

const std::regex HEAP_REGEX =
    std::regex("^(HEAP|STACK)\\$(1|2)(_old|_res)?$", std::regex::ECMAScript);
const std::regex INDEX_REGEX =
    std::regex("^i(1|2)(_res|_old|_stack)?$", std::regex::ECMAScript);
