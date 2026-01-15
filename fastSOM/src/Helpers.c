#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Applic.h>
#include <Rmath.h>
#include <stdlib.h>


SEXP solve_sym(SEXP S, SEXP B, SEXP N, SEXP M)
{
	/* S: symmetric NxN-matrix */
	/* B: rhs matrix of dimension N by M: will be replaced by S^(-1) B by using Cholesky decomposition */

	const int n = asInteger(N);
	const int m = asInteger(M);
	const double * const s = REAL(S);
	double * const b = REAL(B);

	double * const tmpL = (double *) malloc(n*n*sizeof(double));
	
	int i,j,k;
	
	/* calculate Cholesky decomposition tmpL of S */
	for (i=0;i<n;i++) 
		for (j=0;j<=i;j++)
			tmpL[i+j*n] = s[i+j*n]; /* make a copy of (the lower half) of S */ 

	tmpL[0] = sqrt(tmpL[0]);
	for (j=1;j<n;j++) tmpL[j] /= tmpL[0];

	for (i=1;i<n;i++)
	{
		for (j=i;j<n;j++) for (k=i;k<=j;k++)
			tmpL[j+k*n] -= tmpL[k+(i-1)*n]*tmpL[j+(i-1)*n];
		tmpL[i+i*n] = sqrt(tmpL[i+i*n]);
		for (j=i+1;j<n;j++) tmpL[j+i*n] /= tmpL[i+i*n];
	}
	/* Cholesky done: S = tmpL %*% t(tmpL) */
	
	/* B <- solve(tmpL,B) */
	for (j=0;j<m;j++)
	{
		b[0+j*n] /= tmpL[0+0*n]; 
		for (i=1;i<n;i++) 
		{
			for (k=0;k<i;k++)
				b[i+j*n] -= (tmpL[i+k*n]*b[k+j*n]);
			b[i+j*n] /= tmpL[i+i*n];
		}
	}
	
	/* B <- solve(t(tmpL),B) */
	for (j=0;j<m;j++)
	{
		b[n-1+j*n] /= tmpL[(n-1)+(n-1)*n]; 
		for (i=n-2;i>=0;i--) 
		{
			for (k=i+1;k<n;k++)
				b[i+j*n] -= (tmpL[k+i*n]*b[k+j*n]);
			b[i+j*n] /= tmpL[i+i*n];
		}
	}
	
	
	free(tmpL);
	return(R_NilValue);
}

/* calculates a part of the Cholesky decomposition of a matrix s */
void chol_part_C(const double * const s, int n, int col, const int const * perm, double * res)
{
	/* s is the matrix for which we seek for its Cholesky decomposition */ 
	/* n is the dimension of the matrix */
	/* col is the column number up to which calculations are done */
	/* perm is a permutation of 0...n-1 */ 
	/* res is the result */
	int i,j,k;
	
	for (i=0;i<n;i++) 
		for (j=0;j<=i;j++)
			res[perm[i]+perm[j]*n] = s[perm[i]+perm[j]*n]; /* make a copy of (the lower half) of S */ 

	for (i=0;i<n;i++) 
		for (j=i+1;j<n;j++)
			res[perm[i]+perm[j]*n] = 0.0; /* set values above the diagonal to zero */
	
	res[perm[0]+perm[0]*n] = sqrt(res[perm[0]+perm[0]*n]);
	for (j=1;j<n;j++) res[perm[j]+perm[0]*n] /= res[perm[0]+perm[0]*n];

	for (i=1;i<col;i++)
	{
		for (j=i;j<n;j++) for (k=i;k<=j;k++)
			res[perm[j]+perm[k]*n] -= res[perm[k]+(perm[i-1])*n]*res[perm[j]+(perm[i-1])*n];
		res[perm[i]+perm[i]*n] = sqrt(res[perm[i]+perm[i]*n]);
		for (j=i+1;j<n;j++) res[perm[j]+perm[i]*n] /= res[perm[i]+perm[i]*n];
	}
}

/* produces an N x N matrix with each row containing 0 through N-1 in increasing order, but with row number in last column */ 
SEXP Nminus1(SEXP N)
{
	const int n = asInteger(N);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,n*n));
	double * const res = REAL(Res);
	
	int i,j;
	
	for (i=0;i<n;i++)
	{
		for (j=0;j<i;j++) res[i+j*n]=j;
		for (j=i+1;j<n;j++) res[i+(j-1)*n]=j;
		res[i+(n-1)*n]=i;
	}
	
	UNPROTECT(1);
	return(Res);
}

/* scales a matrix S by D (typically D contains 1/standard_deviations): D S D */ 
SEXP scaleSigma(SEXP S, SEXP D, SEXP N)
{
	const int n = asInteger(N);
	const double * const s = REAL(S);
	const double * const d = REAL(D);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,n*n));
	double * const res = REAL(Res);

	int i,j;
	
	for (i=0;i<n;i++) 
		for (j=0;j<n;j++)
			res[i+j*n] = d[i]*d[j]*s[i+j*n];

	UNPROTECT(1);
	return(Res);
}

/* scales an array A by D (typically D contains 1/standard_deviations): D A D^{-1} */ 
SEXP scaleA(SEXP A, SEXP D, SEXP N, SEXP H)
{
	const int n = asInteger(N);
	const int h = asInteger(H);
	const double * const a = REAL(A);
	const double * const d = REAL(D);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,n*n*h));
	double * const res = REAL(Res);

	int i,j,k;
	
	for (i=0;i<n;i++) 
		for (j=0;j<n;j++)
			for (k=0;k<h;k++)
				res[i+j*n+k*n*n] = d[i]/d[j]*a[i+j*n+k*n*n];

	UNPROTECT(1);
	return(Res);
}

