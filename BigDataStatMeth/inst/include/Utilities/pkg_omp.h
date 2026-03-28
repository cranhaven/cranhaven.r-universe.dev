// Configuration based on R-cran package data.table
//      https://github.com/Rdatatable/data.table
/**
 * @file pkg_omp.h
 * @brief OpenMP configuration and compatibility layer
 *
 * This header provides a compatibility layer for OpenMP functionality,
 * ensuring consistent behavior whether OpenMP is available or not.
 * It is based on the R-cran data.table package's OpenMP implementation
 * approach.
 *
 * Key features:
 * - OpenMP version detection and feature enablement
 * - Fallback definitions for non-OpenMP builds
 * - Consistent interface across OpenMP versions
 * - Support for monotonic scheduling in newer OpenMP versions
 *
 * @note Original configuration based on R-cran package data.table
 * @see https://github.com/Rdatatable/data.table
 */

#ifdef _OPENMP
#include <omp.h>

/**
 * @brief OpenMP scheduling configuration
 *
 * Enables monotonic scheduling for OpenMP 4.5 and later versions.
 * Monotonic scheduling ensures loop iterations are distributed to threads
 * in a strictly increasing order, which can improve cache efficiency.
 *
 * @note Introduced in OpenMP 4.5 (201511)
 * @see Issue #4786 in data.table
 */
#if _OPENMP >= 201511
#define monotonic_dynamic monotonic:dynamic
#else
#define monotonic_dynamic dynamic
#endif

/**
 * @brief OpenMP version identifier
 *
 * Stores the OpenMP version for use in error messages and version checks.
 * Uses MY_ prefix to avoid potential future conflicts with OpenMP definitions.
 */
#define MY_OPENMP              _OPENMP

#else
/**
 * @brief Fallback definitions for non-OpenMP builds
 *
 * These definitions provide stub implementations of OpenMP functions
 * for systems without OpenMP support. They ensure code can compile and
 * run (in single-threaded mode) even without OpenMP.
 *
 * Defined functions:
 * - omp_get_num_threads()  -> Always returns 1
 * - omp_get_thread_num()   -> Always returns 0
 * - omp_get_max_threads()  -> Always returns 1
 * - omp_get_thread_limit() -> Always returns 1
 * - omp_get_num_procs()    -> Always returns 1
 * - omp_set_nested()       -> No-op
 * - omp_get_wtime()        -> Always returns 0
 *
 * @note These stubs maintain API compatibility while defaulting to
 * single-threaded behavior.
 */
#define omp_get_num_threads()  1
#define omp_get_thread_num()   0
#define omp_get_max_threads()  1
#define omp_get_thread_limit() 1
#define omp_get_num_procs()    1
#define omp_set_nested(a)      // empty statement to remove the call
#define omp_get_wtime()        0
#define MY_OPENMP              0
#endif
