#pragma once

#ifdef MKL_FOUND
#include <mkl_cblas.h>
#define ARMA_DONT_USE_FORTRAN_HIDDEN_ARGS
#define ARMA_USE_MKL_TYPES
#else
#ifdef _OPENMP
#include <omp.h>
#endif //_OPENMP
#if defined(__APPLE__)
#include "vecLib/cblas.h"
#elif defined(HAVE_FLEXIBLAS_CBLAS_H)
#include "flexiblas/cblas.h"
#elif defined(HAVE_OPENBLAS_CBLAS_H)
#include "openblas/cblas.h"
#else
#include "cblas.h"
#endif
#endif

#ifndef ARMA_64BIT_WORD
#define ARMA_64BIT_WORD
#endif
#ifndef ARMA_DONT_PRINT_FAST_MATH_WARNING
#define ARMA_DONT_PRINT_FAST_MATH_WARNING
#endif
#define ARMA_DONT_USE_WRAPPER
#define ARMA_USE_BLAS
#define ARMA_USE_LAPACK
// #endif
#ifdef USING_R
#include <RcppArmadillo.h>
#include <progress.hpp>
#endif
#ifndef USING_R
#include <armadillo>
#define ULONG ULONG_FAKE
#include "progressWrapper.h"
#undef ULONG
#undef FILE_CREATE
#endif


#ifndef ERR
#ifdef USING_R
#define ERR Rcpp::Rcerr
#else
#define ERR std::cerr
#endif
#endif

#ifndef WARN
#ifdef USING_R
#define WARN Rcpp::Rcerr
#else
#define WARN std::cerr
#endif
#endif

#ifndef INFO
#ifdef USING_R
#define INFO Rcpp::Rcout
#else
#define INFO std::cout
#endif
#endif

#ifndef OUTPUT
#ifdef USING_R
#define OUTPUT Rcpp::Rcout
#else
#define OUTPUT std::cout
#endif
#endif
