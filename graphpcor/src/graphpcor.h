
/* graphpcor.h
 *
 * Copyright (C) 2023 Elias T Krainski
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * The author's contact information:
 *
 *        Elias T Krainski
 *        CEMSE Division
 *        King Abdullah University of Science and Technology
 *        Thuwal 23955-6900, Saudi Arabia
 */


#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "cgeneric.h"

#if defined(_OPENMP)
#include <omp.h>
#endif


#if !defined(Calloc)
#define Calloc(n_, type_)  (type_ *)calloc((n_), sizeof(type_))
#endif
#define SQR(x) ((x)*(x))
#define pow2(x) ((x)*(x))
#define pow3(x) (pow2(x)*(x))
#define pow4(x) (pow2(x)*pow2(x))

#if !defined(iszero)
#ifdef __SUPPORT_SNAN__
#define iszero(x) (fpclassify(x) == FP_ZERO)
#else
#define iszero(x) (((__typeof(x))(x)) == 0)
#endif
#endif

#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif
#define F_ONE ((fortran_charlen_t)1)

double ddot_(int *n, double *dx, int *incx, double *dy, int *incy, fortran_charlen_t);

void dspr_(char *uplo, int *N, double *alpha, double *X, int *incx, double *AP, fortran_charlen_t);

void dgeqp3_(int *N, int *M, double *A, int *LDA, int *PIVOT, double *tau, double *work, int *lwork, int *info, fortran_charlen_t);

void dsyrk_(char *uplo, char *transa, int *n, int *k, double *alpha, double *a, int *lda, double *beta, double *c, int *ldc, fortran_charlen_t);

void dgemv_(char *trans, int *M, int *N, double *alpha,
	    double *A, int *LDA, double *x, int *incx, double *beta, double *y, int *incy, fortran_charlen_t);

void dtpttr_(char *uplo, int *n, double *ap, double *a, int *lda, int *info, fortran_charlen_t);
void dlauum_(char *uplo, int *n, double *a, int *lda, int *info, fortran_charlen_t);
void dlauu2_(char *uplo, int *n, double *a, int *lda, int *info, fortran_charlen_t);

void dtrmm_(char *side, char *uplo, char *transa, char *diag,
	    int *m, int *n, double *alpha, double *a, int *lda, double *b, int *ldb, fortran_charlen_t);
void dsymm_(char *side, char *uplo,
	    int *m, int *n, double *alpha, double *a, int *lda, double *b, int *ldb, double *beta, double *c, int *ldc, fortran_charlen_t);

void dgemm_(char *transa, char *transb,
	    int *m, int *n, int *k, double *alpha, double *a, int *lda, double *b, int *ldb, double *beta, double *c, int *ldc, fortran_charlen_t);

void dgesv_(int *N, int *NRHS, double *A, int *LDA, int *IPIV, double *B, int *LDB, int *INFO, fortran_charlen_t);

void dpotrf_(char *uplo, int *N, double *A, int *LDA, int *INFO, fortran_charlen_t);

void dposv_(char *uplo, int *N, int *NRHS, double *A, int *LDA, double *B, int *LDB, int *INFO, fortran_charlen_t);

void dpotri_(char *uplo, int *N, double *AP, int *INFO, fortran_charlen_t);

void dpptrf_(char *uplo, int *N, double *AP, int *INFO, fortran_charlen_t);
void dpptri_(char *uplo, int *N, double *AP, int *INFO, fortran_charlen_t);
void dpftrf_(char *transr, char *uplo, int *n, double *A, int *info, fortran_charlen_t);

inla_cgeneric_func_tp inla_cgeneric_generic0;
inla_cgeneric_func_tp inla_cgeneric_treepcor;
inla_cgeneric_func_tp inla_cgeneric_kronecker;
inla_cgeneric_func_tp inla_cgeneric_LKJ;
inla_cgeneric_func_tp inla_cgeneric_Wishart;
inla_cgeneric_func_tp inla_cgeneric_pc_prec_correl;
inla_cgeneric_func_tp inla_cgeneric_graphpcor;
