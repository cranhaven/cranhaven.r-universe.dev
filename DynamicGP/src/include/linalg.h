/********************************************************************************
 *
 * Bayesian Regression and Adaptive Sampling with Gaussian Process Trees
 * Copyright (C) 2005, University of California
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Questions? Contact Robert B. Gramacy (rbgramacy@ams.ucsc.edu)
 *
 ********************************************************************************/


#ifndef __LINALG_H__
#define __LINALG_H__

#include "matrix.h"
#include "R_ext/BLAS.h"
#include "R_ext/Lapack.h"

#ifndef CBLAS_ENUM_DEFINED_H
   #define CBLAS_ENUM_DEFINED_H
   enum CBLAS_ORDER {CblasRowMajor=101, CblasColMajor=102 };
   enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113, AtlasConj=114};
   enum CBLAS_UPLO  {CblasUpper=121, CblasLower=122};
   enum CBLAS_DIAG  {CblasNonUnit=131, CblasUnit=132};
   enum CBLAS_SIDE  {CblasLeft=141, CblasRight=142};
#endif

void linalg_dtrsv(const enum CBLAS_TRANSPOSE TA, int n, double **A, int lda,
		  double *Y, int ldy);
void linalg_daxpy(int n, double alpha, double *X, int ldx, double *Y, int ldy);
double linalg_ddot(int n, double *X, int ldx, double *Y, int ldy);
void linalg_dgemm(const enum CBLAS_TRANSPOSE TA, const enum CBLAS_TRANSPOSE TB,
		int m, int n, int k, double alpha, double **A, int lda, double **B,
		int ldb, double beta, double **C, int ldc);
void linalg_dsymm(const enum CBLAS_SIDE side,
		int m, int n, double alpha, double **A, int lda, double **B,
		int ldb, double beta, double **C, int ldc);
void linalg_dgemv(const enum CBLAS_TRANSPOSE TA,
		int m, int n, double alpha, double **A, int lda,
		double *X, int ldx, double beta, double *Y, int ldy);
void linalg_dsymv(int n, double alpha, double **A, int lda,
		double *X, int ldx, double beta, double *Y, int ldy);

int linalg_dposv(int n, double **Mutil, double **Mi);
int linalg_dgesv(int n, double **Mutil, double **Mi);
int linalg_dpotrf(int n, double **var);
void linalg_dtrmv(const enum CBLAS_UPLO up, const enum CBLAS_TRANSPOSE tr,
		  const enum CBLAS_DIAG diag, int n, double **A, int lda,
		  double *x, int incx);
void linalg_dtrsm(const enum CBLAS_SIDE side, const enum CBLAS_UPLO up,
		  const enum CBLAS_TRANSPOSE tr, enum CBLAS_DIAG diag,
		  int m, int n, double alpha, double **A, int lda,
		  double *b, int ldb);
int linalg_dgesdd(double **, int, int, double *, double *, double **);
int linalgext_dposv(int n, int m, double **Mutil, double **Mi);
#endif

