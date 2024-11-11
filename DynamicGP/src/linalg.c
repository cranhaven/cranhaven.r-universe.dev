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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 *
 * Questions? Contact Robert B. Gramacy (rbgramacy@ams.ucsc.edu)
 *
 ********************************************************************************/
#include "linalg.h"
#include "matrix.h"
#include <assert.h>
#include <stdlib.h>

const char uplo = 'U';
const char jobz = 'S';

/*
 * linalg_dtrsv:
 *
 * analog of dtrsv in cblas nad blas
 * assumed row-major lower-tri and non-unit
 */
void linalg_dtrsv(const enum CBLAS_TRANSPOSE TA, int n, double **A, int lda,
                  double *Y, int ldy) {
  char ta;
  char diag = 'N';
  if (TA == CblasTrans)
    ta = 'T';
  else
    ta = 'N';
  F77_CALL(dtrsv)(&uplo, &ta, &diag, &n, *A, &lda, Y, &ldy FCONE FCONE FCONE);
}

/*
 * linalg_ddot:
 *
 * analog of ddot in cblas nad blas
 */
double linalg_ddot(int n, double *X, int ldx, double *Y, int ldy) {
  double result;
  int n64, ldx64, ldy64;
  n64 = n;
  ldx64 = ldx;
  ldy64 = ldy;
  result = F77_CALL(ddot)(&n64, X, &ldx64, Y, &ldy64);
  return result;
}

/*
 * linalg_daxpy:
 *
 * analog of daxpy in cblas nad blas
 */
void linalg_daxpy(int n, double alpha, double *X, int ldx, double *Y, int ldy) {
  int n64, ldx64, ldy64;
  n64 = n;
  ldx64 = ldx;
  ldy64 = ldy;
  /* daxpy(&n,&alpha,X,&ldx,Y,&ldy); */
  F77_CALL(daxpy)(&n64, &alpha, X, &ldx64, Y, &ldy64);
}

/*
 * linalg_dgemm:
 *
 * analog of dgemm in cblas nad blas
 * assumed column major representation
 */
void linalg_dgemm(const enum CBLAS_TRANSPOSE TA, const enum CBLAS_TRANSPOSE TB,
                  int m, int n, int k, double alpha, double **A, int lda,
                  double **B, int ldb, double beta, double **C, int ldc) {
  int m64, n64, k64, lda64, ldb64, ldc64;
  char ta, tb;
  m64 = m;
  n64 = n;
  k64 = k;
  lda64 = lda;
  ldb64 = ldb;
  ldc64 = ldc;
  if (TA == CblasTrans)
    ta = 'T';
  else
    ta = 'N';
  if (TB == CblasTrans)
    tb = 'T';
  else
    tb = 'N';
  F77_CALL(dgemm)
  (&ta, &tb, &m64, &n64, &k64, &alpha, *A, &lda64, *B, &ldb64, &beta, *C,
   &ldc64 FCONE FCONE);
}

/*
 * linalg_dgemv:
 *
 * analog of dgemv in cblas nad blas
 * assumed column major representation
 */
void linalg_dgemv(const enum CBLAS_TRANSPOSE TA, int m, int n, double alpha,
                  double **A, int lda, double *X, int ldx, double beta,
                  double *Y, int ldy) {
  int m64, n64, lda64, ldx64, ldy64;
  char ta;
  m64 = m;
  n64 = n, lda64 = lda;
  ldx64 = ldx;
  ldy64 = ldy;
  if (TA == CblasTrans)
    ta = 'T';
  else
    ta = 'N';
  /* dgemv(&ta,&m,&n,&alpha,*A,&lda,X,&ldx,&beta,Y,&ldy); */
  F77_CALL(dgemv)
  (&ta, &m64, &n64, &alpha, *A, &lda64, X, &ldx64, &beta, Y, &ldy64 FCONE);
}

/*
 * linalg_dsymm:
 *
 * analog of dsymm in cblas nad blas
 * assumed column major and upper-triangluar representation
 */
void linalg_dsymm(const enum CBLAS_SIDE SIDE, int m, int n, double alpha,
                  double **A, int lda, double **B, int ldb, double beta,
                  double **C, int ldc) {
  int m64, n64, lda64, ldb64, ldc64;
  char side;
  m64 = m;
  n64 = n;
  lda64 = lda;
  ldb64 = ldb;
  ldc64 = ldc;
  if (SIDE == CblasRight)
    side = 'R';
  else
    side = 'L';
  /* dsymm(&side,&uplo,&m,&n,&alpha,*A,&lda,*B,&ldb,&beta,*C,&ldc); */
  F77_CALL(dsymm)
  (&side, &uplo, &m64, &n64, &alpha, *A, &lda64, *B, &ldb64, &beta, *C,
   &ldc64 FCONE FCONE);
}

/*
 * linalg_dsymv:
 *
 * analog of dsymv in cblas and blas
 * assumed column major representation
 */
void linalg_dsymv(int n, double alpha, double **A, int lda, double *X, int ldx,
                  double beta, double *Y, int ldy) {
  int n64, lda64, ldy64, ldx64;
  n64 = n;
  lda64 = lda;
  ldx64 = ldx;
  ldy64 = ldy;
  /* dsymv(&uplo,&n,&alpha,*A,&lda,X,&ldx,&beta,Y,&ldy); */

  F77_CALL(dsymv)
  (&uplo, &n64, &alpha, *A, &lda64, X, &ldx64, &beta, Y, &ldy64 FCONE);
}

/*
 * linalg_dposv:
 *
 * analog of dposv in clapack and lapack where
 * Mutil is with colmajor and uppertri or rowmajor
 * and lowertri
 */
int linalg_dposv(int n, double **Mutil, double **Mi) {
  /* then use LAPACK */
  int n64, info;
  n64 = n;
  F77_CALL(dposv)(&uplo, &n64, &n64, *Mutil, &n64, *Mi, &n64, &info FCONE);
  return (int)info;
}

/*
 * linalg_dgesv:
 *
 * analog of dgesv in clapack and lapack;
 * row or col major doesn't matter because it is
 * assumed that Mutil is symmetric
 *
 * inverse_lu used this with RowMajor, other with ColMajor
 */
int linalg_dgesv(int n, double **Mutil, double **Mi) {
  int info;
  int *p;

  p = new_ivector(n);
  F77_CALL(dgesv)(&n, &n, *Mutil, &n, p, *Mi, &n, &info);
  free(p);
  return info;
}

/*
 *
 * analog of dpotrf in clapack and lapack where
 * var is with colmajor and uppertri or rowmajor
 * and lowertri
 */
int linalg_dpotrf(int n, double **var) {
  int n64, info;
  n64 = n;
  F77_CALL(dpotrf)(&uplo, &n64, *var, &n64, &info FCONE);
  return (int)info;
}

int linalg_dgesdd(double **X, int nrow, int ncol, double *s, double *u,
                  double **vt) {
  int info = 0, lwork = -1;
  int nsv = nrow < ncol ? nrow : ncol;
  int *iwork = (int *)malloc(8 * (int)(nsv) * sizeof(int));
  double tmp, *work;

  F77_CALL(dgesdd)
  (&jobz, &nrow, &ncol, *X, &nrow, s, u, &nrow, *vt, &nsv, &tmp, &lwork, iwork,
   &info FCONE);
  if (info != 0)
    return info;

  lwork = (int)tmp;

  work = (double *)malloc(lwork * sizeof(double));

  F77_CALL(dgesdd)
  (&jobz, &nrow, &ncol, *X, &nrow, s, u, &nrow, *vt, &nsv, work, &lwork, iwork,
   &info FCONE);
  free(work);
  free(iwork);
  return info;
}

void linalg_dtrmv(const enum CBLAS_UPLO up, const enum CBLAS_TRANSPOSE tr,
                  const enum CBLAS_DIAG diag, int n, double **A, int lda,
                  double *x, int incx) {
  char uplo, trans, isdiag;
  uplo = (up == CblasUpper) ? 'U' : 'L';
  trans = (tr == CblasTrans) ? 'T' : 'N';
  isdiag = (diag == CblasUnit) ? 'U' : 'N';
  F77_CALL(dtrmv)
  (&uplo, &trans, &isdiag, &n, *A, &lda, x, &incx FCONE FCONE FCONE);
}

void linalg_dtrsm(const enum CBLAS_SIDE side, const enum CBLAS_UPLO up,
                  const enum CBLAS_TRANSPOSE tr, enum CBLAS_DIAG diag, int m,
                  int n, double alpha, double **A, int lda, double *b,
                  int ldb) {
  char isleft, uplo, trans, isdiag;
  isleft = (side == CblasLeft) ? 'L' : 'R';
  uplo = (up == CblasUpper) ? 'U' : 'L';
  trans = (tr == CblasTrans) ? 'T' : 'N';
  isdiag = (diag == CblasUnit) ? 'U' : 'N';
  F77_CALL(dtrsm)
  (&isleft, &uplo, &trans, &isdiag, &m, &n, &alpha, *A, &lda, b,
   &ldb FCONE FCONE FCONE FCONE);
}

int linalgext_dposv(int n, int m, double **Mutil, double **Mi) {
  int n64, m64, info;
  n64 = n;
  m64 = m;
  F77_CALL(dposv)(&uplo, &n64, &m64, *Mutil, &n64, *Mi, &n64, &info FCONE);
  return (int)info;
}
