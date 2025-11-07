/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ***************************************
 * compiler flags, function attributes *
 ***************************************
 */


#ifndef TIND_COMPILER_H

#define TIND_COMPILER_H


// #if defined(__GNUC__)
//
// #pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
// #pragma GCC diagnostic ignored "-Wparentheses"
//
// #elif defined(__clang__) /* defined(__GNUC__) */
//
// #pragma clang diagnostic ignored "-Wmaybe-uninitialized"
// #pragma clang diagnostic ignored "-Wparentheses"
//
// #endif /* defined(__GNUC__) */


#define TIND__ATTRIBUTE_INLINE __attribute__((always_inline))

#define TIND__ATTRIBUTE_FUNCTION __attribute__((optimize(3)))


#endif /* TIND_COMPILER_H */

