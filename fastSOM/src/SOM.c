#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Applic.h>
#include <Rmath.h>
#include <stdlib.h>

void chol_part_C(const double * const s, int n, int col, const int const * perm, double * res);

SEXP array_stuff(SEXP B, SEXP A, SEXP S, SEXP N1, SEXP N2, SEXP H)
{
	double * b = REAL(B);
	const double * const a = REAL(A);	
	const double * const s = REAL(S);	
	const int n1 = asInteger(N1);
	const int n2 = asInteger(N2);
	const int h = asInteger(H);
	// B = B + A %*% S
	int i,j,k,hh;
	for (hh=0;hh<h;hh++) for (i=0;i<n1;i++) for (j=0;j<n1;j++) for (k=0;k<n2;k++)
		b[i+j*n1+hh*n1*n1] += (a[i+k*n1+hh*n1*n2]*s[k+j*n2]);
	return(R_NilValue);
}

SEXP matrix_stuff(SEXP A, SEXP B, SEXP C, SEXP N1, SEXP N2, SEXP N3)
{
// A: N1 x N3, B: N1 x N2, C: N2 x N3
	double * a = REAL(A);
	const double * const b = REAL(B);	
	const double * const c = REAL(C);	
	const int n1 = asInteger(N1);
	const int n2 = asInteger(N2);
	const int n3 = asInteger(N3);
	// A = A + B %*% C
	int i,j,k;
	for (i=0;i<n1;i++) for (j=0;j<n3;j++) for (k=0;k<n2;k++)
		a[i+j*n1] += (b[i+k*n1]*c[k+j*n2]);
	return(R_NilValue);
}

SEXP paste_together(SEXP Res1, SEXP Res2, SEXP N, SEXP N1, SEXP N2, SEXP Combs, SEXP Ncombs)
{
	const double * const res1 = REAL(Res1);
	const double * const res2 = REAL(Res2);
	const int * const combs = INTEGER(Combs);
	const int n = asInteger(N);
	const int n1 = asInteger(N1);
	const int n2 = asInteger(N2);
	const int ncombs = asInteger(Ncombs);

	SEXP Res;
	int i, indMax=0, indMin=0, veclen=3+2*n, veclen1=3+2*n1, veclen2=3+2*n2;
	PROTECT(Res = allocVector(REALSXP,veclen));
	double *const res = REAL(Res); 
	res[0] = 0.0; //average
	res[1] = 0.0; //max
	res[2] = n; //min
	
	double tmp;
	
	for (i=0;i<ncombs;i++)
	{
		res[0] += ((res1[0+i*veclen1]+res2[0+i*veclen2])/ncombs);
		tmp = res1[1+i*veclen1] + res2[1+i*veclen2];
		if (tmp>res[1])
		{
			res[1] = tmp;
			indMax = i;
		}
		tmp = res1[2+i*veclen1] + res2[2+i*veclen2];
		if (tmp<res[2])
		{
			res[2] = tmp;
			indMin = i;
		}
	}
	
	for (i=0;i<n1;i++)
	{
		res[3+i]=combs[(int) res1[3+i+indMax*veclen1]-1+indMax*n];
		res[3+n+i]=combs[(int) res1[3+i+n1+indMin*veclen1]-1+indMin*n];
	}
	for (i=0;i<n2;i++)
	{
		res[3+n1+i]=combs[(int) res2[3+i+indMax*veclen2]-1+n1+indMax*n];
		res[3+n+n1+i]=combs[(int) res2[3+i+n2+indMin*veclen2]-1+n1+indMin*n];
	}
	UNPROTECT(1);
	return(Res);
}

SEXP trALsquared_perms(SEXP S, SEXP A, SEXP N, SEXP H, SEXP Perms, SEXP Nperms, SEXP FirstPerm)
{
	const int n = asInteger(N);
	const int h = asInteger(H);
	const int nperms = asInteger(Nperms);
	const int firstPerm = asInteger(FirstPerm);
	const int * const perms = INTEGER(Perms);
	const double * const s = REAL(S);
	const double * const a = REAL(A);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,3+2*n));
	double *const res = REAL(Res); 
	res[0] = 0.0; //average
	res[1] = 0.0; //max
	res[2] = n; //min

	double * const tmpL = (double *) malloc(n*n*sizeof(double));
	double tmp,cur_res;
	
	int i,k,hh,i_perms;
	int * perm;
	for (i_perms=0;i_perms<nperms;i_perms++)
	{
		perm = (int *) perms + ((firstPerm+i_perms)*n);

		chol_part_C(s,n,n,perm,tmpL); 

		cur_res=0.0;
		for (i=0;i<n;i++)
			for (hh=0;hh<h;hh++)
			{
				tmp=0.0;
				for (k=i;k<n;k++) // tmpL is lower-triangular!
					tmp += (a[perm[i]+perm[k]*n+hh*n*n]*tmpL[perm[k]+perm[i]*n]);
				cur_res += tmp*tmp;
			}

		res[0] += cur_res/nperms;
		if (cur_res > res[1])
		{
			res[1] = cur_res;
			for (i=0;i<n;i++) 
				res[3+i] = perm[i]+1;  
		}
		if (cur_res < res[2])
		{
			res[2] = cur_res;
			for (i=0;i<n;i++) 
				res[3+n+i] = perm[i]+1;  
		}
		
	}
	
	free(tmpL);
	UNPROTECT(1);
	return(Res);
}

SEXP trALplusBLinv_squared_perms(SEXP S, SEXP A, SEXP B, SEXP N, SEXP H, SEXP Perms, SEXP Nperms)
{
	const int n = asInteger(N);
	const int h = asInteger(H);
	const int nperms = asInteger(Nperms);
	const int * const perms = INTEGER(Perms);
	const double * const s = REAL(S);
	const double * const a = REAL(A);
	const double * const b = REAL(B);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,3+2*n));
	double * const res = REAL(Res); 
	res[0] = 0.0; //average
	res[1] = 0.0; //max
	res[2] = n; //min
	
	double * const tmpL = (double *) malloc(n*n*sizeof(double));
	double * const copyB = (double *) malloc(n*n*sizeof(double));
	double tmp, cur_res;
	
	int i,j,k,hh,i_perms;
	int * perm;
	for (i_perms=0;i_perms<nperms;i_perms++)
	{
		perm = (int *) perms + (i_perms*n);
		chol_part_C(s,n,n,perm,tmpL); 

		cur_res=0.0;
		for (hh=0;hh<h;hh++)
		{
			for (i=0;i<n;i++) for (k=0;k<=i;k++) // need only lower part of B
				copyB[perm[i]+perm[k]*n] = b[perm[i]+perm[k]*n+hh*n*n];
			for (i=0;i<n;i++)
			{
				tmp=0.0;
				for (k=i;k<n;k++) // (AL)_{ii}, using that tmpL is lower-triangular!
					tmp += (a[perm[i]+perm[k]*n+hh*n*n]*tmpL[perm[k]+perm[i]*n]);
				for (j=i;j<n;j++) copyB[perm[j]+perm[i]*n] /= tmpL[perm[i]+perm[i]*n]; 
				tmp += copyB[perm[i]+perm[i]*n]; // diagonal of B L'^{-1}
				cur_res += (tmp*tmp);
				for (j=i+1;j<n;j++) for (k=j;k<n;k++) // update copyB
					copyB[perm[k]+perm[j]*n] -= (copyB[perm[k]+perm[i]*n]*tmpL[perm[j]+perm[i]*n]);
			}
		}
		
		res[0] += cur_res/nperms;
		if (cur_res > res[1])
		{
			res[1] = cur_res;
			for (i=0;i<n;i++) 
				res[3+i] = perm[i]+1;  
		}
		if (cur_res < res[2])
		{
			res[2] = cur_res;
			for (i=0;i<n;i++) 
				res[3+n+i] = perm[i]+1;  
		}
		
	}

	free(tmpL);
	free(copyB);
	UNPROTECT(1);
	return(Res);
}

SEXP trALplusBLinv_squared(SEXP S, SEXP A, SEXP B, SEXP N, SEXP H, SEXP Perm)
{
	const int n = asInteger(N);
	const int h = asInteger(H);
	const int * const perm = INTEGER(Perm);
	const double * const s = REAL(S);
	const double * const a = REAL(A);
	const double * const b = REAL(B);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,1));
	double * const res = REAL(Res); 
	double * const tmpL = (double *) malloc(n*n*sizeof(double));
	double * const copyB = (double *) malloc(n*n*sizeof(double));
	double tmp;
	
	int i,j,k,hh;

	chol_part_C(s,n,n,perm,tmpL); 

	*res=0.0;
	for (hh=0;hh<h;hh++)
	{
		for (i=0;i<n;i++) for (k=0;k<=i;k++) // need only lower part of B
			copyB[perm[i]+perm[k]*n] = b[perm[i]+perm[k]*n+hh*n*n];
		for (i=0;i<n;i++)
		{
			tmp=0.0;
			for (k=i;k<n;k++) // (AL)_{ii}, using that tmpL is lower-triangular!
				tmp += (a[perm[i]+perm[k]*n+hh*n*n]*tmpL[perm[k]+perm[i]*n]);
			for (j=i;j<n;j++) copyB[perm[j]+perm[i]*n] /= tmpL[perm[i]+perm[i]*n]; 
			tmp += copyB[perm[i]+perm[i]*n]; // diagonal of B L'^{-1}
			*res += (tmp*tmp);
			for (j=i+1;j<n;j++) for (k=j;k<n;k++) // update copyB
				copyB[perm[k]+perm[j]*n] -= (copyB[perm[k]+perm[i]*n]*tmpL[perm[j]+perm[i]*n]);
		}
	}	
	free(tmpL);
	free(copyB);
	UNPROTECT(1);
	return(Res);
}

SEXP ALsquared_perms(SEXP S, SEXP A, SEXP N, SEXP H, SEXP Perms, SEXP Nperms)
{
	const int n = asInteger(N);
	const int h = asInteger(H);
	const int nperms = asInteger(Nperms);
	const int * const perms = INTEGER(Perms);
	const double * const s = REAL(S);
	const double * const a = REAL(A);

	SEXP Average, Minimum, Maximum, Res, Names;
	PROTECT (Res                  = allocVector(VECSXP, 3));
	PROTECT (Names                = allocVector(STRSXP, 3));
	PROTECT (Average              = allocVector(REALSXP, n*n));
	PROTECT (Minimum              = allocVector(REALSXP, n*n));
	PROTECT (Maximum              = allocVector(REALSXP, n*n));
	double * const average        = REAL(Average);
	double * const minimum        = REAL(Minimum);
	double * const maximum        = REAL(Maximum);

	double * const tmpL = (double *) malloc(n*n*sizeof(double));
	double tmp, * const mat = (double *) malloc(n*n*sizeof(double));
	
	int i,j,k,hh;

	int n2 = n*n, i_perms, *perm;
	for (i=0; i<n2;i++) 
	{
		average[i]=0.0;
		minimum[i]=1.0;
		maximum[i]=0.0;
	}

	for (i_perms=0;i_perms<nperms;i_perms++)
	{
		perm = (int*) perms + (i_perms*n);
		chol_part_C(s,n,n,perm,tmpL); 

		for (i=0;i<n;i++)
			for (j=0;j<n;j++)
		{
			mat[i+perm[j]*n] = 0.0;
			for (hh=0;hh<h;hh++)
			{
				tmp=0.0;
				for (k=j;k<n;k++) // tmpL is lower-triangular!
					tmp += (a[i+perm[k]*n+hh*n*n]*tmpL[perm[k]+perm[j]*n]);
				//average[i+perm[j]*n] += (tmp*tmp);
				mat[i+perm[j]*n] += (tmp*tmp);
			}
			average[i+perm[j]*n] += mat[i+perm[j]*n];
			minimum[i+perm[j]*n] = fmin2(minimum[i+perm[j]*n],mat[i+perm[j]*n]);
			maximum[i+perm[j]*n] = fmax2(maximum[i+perm[j]*n],mat[i+perm[j]*n]);
		}
	}

	for (i=0;i<n2;i++) average[i] /= nperms;
	
	free(tmpL);
	free(mat);
	SET_VECTOR_ELT(Res, 0, Average);
	SET_VECTOR_ELT(Res, 1, Minimum);
	SET_VECTOR_ELT(Res, 2, Maximum);
	SET_STRING_ELT(Names, 0, mkChar("Average"));
	SET_STRING_ELT(Names, 1, mkChar("Minimum"));
	SET_STRING_ELT(Names, 2, mkChar("Maximum"));
	setAttrib(Res, R_NamesSymbol, Names);
	UNPROTECT(5);
	return(Res);
}

SEXP ALsquared(SEXP S, SEXP A, SEXP N, SEXP H, SEXP Perm)
{
	const int n = asInteger(N);
	const int h = asInteger(H);
	const int * const perm = INTEGER(Perm);
	const double * const s = REAL(S);
	const double * const a = REAL(A);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,n*n));
	double * const res = REAL(Res);
	double * const tmpL = (double *) malloc(n*n*sizeof(double));
	double tmp;
	
	int i,j,k,hh;

	chol_part_C(s,n,n,perm,tmpL); 

	for (i=0;i<n;i++)
		for (j=0;j<n;j++)
		{
			res[i+perm[j]*n] = 0.0;
			for (hh=0;hh<h;hh++)
			{
				tmp=0.0;
				for (k=j;k<n;k++) // tmpL is lower-triangular!
					tmp += (a[i+perm[k]*n+hh*n*n]*tmpL[perm[k]+perm[j]*n]);
				res[i+perm[j]*n] += tmp*tmp;
			}
		}
	free(tmpL);
	UNPROTECT(1);
	return(Res);
}

SEXP trALsquared(SEXP S, SEXP A, SEXP N, SEXP H, SEXP Perm)
{
	const int n = asInteger(N);
	const int h = asInteger(H);
	const int * const perm = INTEGER(Perm);
	const double * const s = REAL(S);
	const double * const a = REAL(A);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,1));
	double *const res = REAL(Res); 
	double * const tmpL = (double *) malloc(n*n*sizeof(double));
	double tmp;
	
	int i,k,hh;

	chol_part_C(s,n,n,perm,tmpL); 

	*res=0.0;
	for (i=0;i<n;i++)
		for (hh=0;hh<h;hh++)
		{
			tmp=0.0;
			for (k=i;k<n;k++) // tmpL is lower-triangular!
				tmp += (a[perm[i]+perm[k]*n+hh*n*n]*tmpL[perm[k]+perm[i]*n]);
			*res += tmp*tmp;
		}
	free(tmpL);
	UNPROTECT(1);
	return(Res);
}

/* function to calculate average, minimum and maximum spillover table entries over all permutations */
SEXP SOT_avg(SEXP S, SEXP A, SEXP N, SEXP H, SEXP NcK, SEXP Cumpos, SEXP Gensets, SEXP NminusOne)
{
	/* S: positive symmetric positive definite maxtrix (Sigma) */
	/* A: array (A) */
	/* N: number of variables */
	/* H: forecast horizon (third dimension of A) */
	/* NcK: "N-1 choose K" (vector with K ranging from 0 to N-1*/
	/* CumPos: cumulative sum of NcK, determines where elements of Gensets grow by an additional element */
	/* Gensets: matrix with 2^(N-1) rows (corresponding to the subsets of 1,...,N-1) with every row consisting of the elements of the subset followed by the remaining elements of 1,...,N-1 */ 
	/* NminusOne: N by N-matrix */
	
	const int n = asInteger(N);
	const int h = asInteger(H);
	const int * const ncK = INTEGER(NcK);
	const int * const cumpos = INTEGER(Cumpos);
	const int * const nminusOne = INTEGER(NminusOne);
	const int * const gensets = INTEGER(Gensets);

	const double * const a = REAL(A);
	const double * const s = REAL(S);

	SEXP Average, Minimum, Maximum, Average_p, Minimum_p, Maximum_p, Average_n, Minimum_n, Maximum_n,Res, Names;
	PROTECT (Res                  = allocVector(VECSXP, 9));
	PROTECT (Names                = allocVector(STRSXP, 9));
	PROTECT (Average              = allocVector(REALSXP, n*n));
	PROTECT (Minimum              = allocVector(REALSXP, n*n));
	PROTECT (Maximum              = allocVector(REALSXP, n*n));
	PROTECT (Average_p            = allocVector(REALSXP, n*n));
	PROTECT (Minimum_p            = allocVector(REALSXP, n*n));
	PROTECT (Maximum_p            = allocVector(REALSXP, n*n));
	PROTECT (Average_n            = allocVector(REALSXP, n*n));
	PROTECT (Minimum_n            = allocVector(REALSXP, n*n));
	PROTECT (Maximum_n            = allocVector(REALSXP, n*n));
	double * const average        = REAL(Average);
	double * const minimum        = REAL(Minimum);
	double * const maximum        = REAL(Maximum);
	double * const average_p      = REAL(Average_p);
	double * const minimum_p      = REAL(Minimum_p);
	double * const maximum_p      = REAL(Maximum_p);
	double * const average_n      = REAL(Average_n);
	double * const minimum_n      = REAL(Minimum_n);
	double * const maximum_n      = REAL(Maximum_n);
	
	double tmp;
	double * const tmpL = (double *) malloc(n*n*sizeof(double));
	double * const current_column = (double *) malloc(n*sizeof(double));
	double * const current_column_p = (double *) malloc(n*sizeof(double));
	double * const current_column_n = (double *) malloc(n*sizeof(double));
	int * const current_perm = (int *) malloc(n*sizeof(int));
	double * const weights = (double *) malloc(n*sizeof(double));

	int hh,i,j,k,l,m;
	int len = cumpos[n]; // number of rows of gensets
	int cur_subset; // currently considered subset

	for (i=0;i<n;i++) weights[i]=1.0/(n*ncK[i]); /* weights */

	/* Initialization */
	for (j=0;j<(n*n);j++)
	{
		average[j]=0.0;
		minimum[j]=1.0;
		maximum[j]=0.0;
		average_p[j]=0.0;
		minimum_p[j]=1.0;
		maximum_p[j]=0.0;
		average_n[j]=0.0;
		minimum_n[j]=1.0;
		maximum_n[j]=0.0;
	}	
	
	for (i=0;i<n;i++) /* positions */
	{
		for (k=0;k<ncK[i];k++) /* subsets of length i */
		{
			cur_subset = k+cumpos[i]; // currently considered subset
			for (j=0;j<n;j++) /* variables */
			{
				for (l=0;l<i;l++) current_perm[l]=nminusOne[j+gensets[cur_subset+len*l]*n]; // first i elements
				current_perm[i]=j; // j's position is i
				for (l=i+1;l<n;l++) current_perm[l]=nminusOne[j+gensets[cur_subset+len*(l-1)]*n]; // remaining elements
				chol_part_C(s,n,i+1,current_perm,tmpL); // calculate Cholesky decomposition of L[current_perm,current_perm] up to column i+1
				for (l=0;l<n;l++) 
				{
					current_column[l]=0.0; // current_column <- 0
					current_column_p[l]=0.0; // current_column_p <- 0
					current_column_n[l]=0.0; // current_column <- 0
				}
				for (hh=0;hh<h;hh++)
				{
					for (l=0;l<n;l++) //calculate l'th entry of A[,,h] %*% tmpL[,i] (tmpL[firstpart,i] is zero!)
					{
						tmp = a[current_perm[l]+j*n+hh*n*n]*tmpL[j+j*n]; // a[perm[l],j,hh]*tmpL[j,j]
						for (m=i+1;m<n;m++) tmp += (a[current_perm[l]+current_perm[m]*n+hh*n*n]*tmpL[current_perm[m]+j*n]); // a[perm[l],perm[m],hh]*tmpL[perm[m],j]
						current_column[current_perm[l]] += (tmp*tmp); // column of spillover table for variable j, when variable j is at position i, and variables ... precede it
						if (tmp>0) current_column_p[current_perm[l]] += (tmp*tmp); // column of positive contributions to spillover table for variable j, when variable j is at position i, and variables ... precede it
						if (tmp<0) current_column_n[current_perm[l]] += (tmp*tmp); // column of negative contributions to spillover table for variable j, when variable j is at position i, and variables ... precede it
					}
				}
				for (l=0;l<n;l++)
				{
					average[l+j*n] += (weights[i]*current_column[l]);
					minimum[l+j*n] = fmin2(minimum[l+j*n],current_column[l]);
					maximum[l+j*n] = fmax2(maximum[l+j*n],current_column[l]);
					average_p[l+j*n] += (weights[i]*current_column_p[l]);
					minimum_p[l+j*n] = fmin2(minimum_p[l+j*n],current_column_p[l]);
					maximum_p[l+j*n] = fmax2(maximum_p[l+j*n],current_column_p[l]);
					average_n[l+j*n] += (weights[i]*current_column_n[l]);
					minimum_n[l+j*n] = fmin2(minimum_n[l+j*n],current_column_n[l]);
					maximum_n[l+j*n] = fmax2(maximum_n[l+j*n],current_column_n[l]);
				}
			}
		}
	}

	free(tmpL);
	free(current_column);
	free(current_column_p);
	free(current_column_n);
	free(current_perm);
	free(weights);
	
	SET_VECTOR_ELT(Res, 0, Average);
	SET_VECTOR_ELT(Res, 1, Minimum);
	SET_VECTOR_ELT(Res, 2, Maximum);
	SET_VECTOR_ELT(Res, 3, Average_p);
	SET_VECTOR_ELT(Res, 4, Minimum_p);
	SET_VECTOR_ELT(Res, 5, Maximum_p);
	SET_VECTOR_ELT(Res, 6, Average_n);
	SET_VECTOR_ELT(Res, 7, Minimum_n);
	SET_VECTOR_ELT(Res, 8, Maximum_n);
	SET_STRING_ELT(Names, 0, mkChar("Average"));
	SET_STRING_ELT(Names, 1, mkChar("Minimum"));
	SET_STRING_ELT(Names, 2, mkChar("Maximum"));
	SET_STRING_ELT(Names, 3, mkChar("Average_p"));
	SET_STRING_ELT(Names, 4, mkChar("Minimum_p"));
	SET_STRING_ELT(Names, 5, mkChar("Maximum_p"));
	SET_STRING_ELT(Names, 6, mkChar("Average_n"));
	SET_STRING_ELT(Names, 7, mkChar("Minimum_n"));
	SET_STRING_ELT(Names, 8, mkChar("Maximum_n"));
	setAttrib(Res, R_NamesSymbol, Names);
	UNPROTECT(11);
	return(Res);
}

/* function to calculate the forecast error variances */
SEXP fev(SEXP S, SEXP A, SEXP N, SEXP H)
{
	/* S: positive symmetric positive definite maxtrix (Sigma) */
	/* A: array (A) */
	/* N: number of variables */
	/* H: forecast horizon (third dimension of A) */
	const int n = asInteger(N);
	const int h = asInteger(H);
	const double * const a = REAL(A);
	const double * const s = REAL(S);
	SEXP Res;
	PROTECT(Res = allocVector(REALSXP,n));
	double * const res = REAL(Res);

	int i,j,k,hh;
	
	for (i=0;i<n;i++) 
	{
		res[i]=0.0;
		for (hh=0;hh<h;hh++)
		{
			for (j=0;j<n;j++) res[i] += (a[i+j*n+hh*n*n]*a[i+j*n+hh*n*n]*s[j+j*n]);
			for (j=0;j<n;j++) for (k=j+1;k<n;k++)
				res[i] += (2*a[i+j*n+hh*n*n]*a[i+k*n+hh*n*n]*s[k+j*n]);
		}
	}
	UNPROTECT(1);
	return(Res);
}

