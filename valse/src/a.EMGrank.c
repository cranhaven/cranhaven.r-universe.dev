#include <R.h>
#include <Rdefines.h>
#include "EMGrank.h"

// See comments in src/sources/EMGrank.c and R/EMGrank.R (wrapper)
SEXP EMGrank(
	SEXP Pi_,
	SEXP Rho_,
	SEXP mini_,
	SEXP maxi_,
	SEXP X_,
	SEXP Y_,
	SEXP eps_,
	SEXP rank_
) {
	// Get matrices dimensions
	SEXP dimX = getAttrib(X_, R_DimSymbol);
	int n = INTEGER(dimX)[0];
	int p = INTEGER(dimX)[1];
	SEXP dimRho = getAttrib(Rho_, R_DimSymbol);
	int m = INTEGER(dimRho)[0];
	int k = INTEGER(dimRho)[2];

	////////////
	// INPUTS //
	////////////

	// get scalar parameters
	int mini = INTEGER_VALUE(mini_);
	int maxi = INTEGER_VALUE(maxi_);
	double eps = NUMERIC_VALUE(eps_);

	// Get pointers from SEXP arrays ; WARNING: by columns !
	double* Pi = REAL(Pi_);
	double* Rho = REAL(Rho_);
	double* X = REAL(X_);
	double* Y = REAL(Y_);
	int* rank = INTEGER(rank_);

	/////////////
	// OUTPUTS //
	/////////////

	SEXP phi, LLF, dimPhi;
	PROTECT(dimPhi = allocVector(INTSXP, 3));
	int* pDimPhi = INTEGER(dimPhi);
	pDimPhi[0] = p; pDimPhi[1] = m; pDimPhi[2] = k;
	PROTECT(phi = allocArray(REALSXP, dimPhi));
	PROTECT(LLF = allocVector(REALSXP, 1));
	double *pPhi=REAL(phi), *pLLF=REAL(LLF);

	/////////////////////
	// Call to EMGrank //
	/////////////////////

	EMGrank_core(Pi, Rho, mini, maxi, X, Y, eps, rank,
		pPhi,pLLF,
		n,p,m,k);

	// Build list from OUT params and return it
	SEXP listParams, listNames;
	PROTECT(listParams = allocVector(VECSXP, 2));
	char* lnames[2] = {"phi", "LLF"}; //lists labels
	PROTECT(listNames = allocVector(STRSXP,2));
	for (int i=0; i<2; i++)
		SET_STRING_ELT(listNames,i,mkChar(lnames[i]));
	setAttrib(listParams, R_NamesSymbol, listNames);
	SET_VECTOR_ELT(listParams, 0, phi);
	SET_VECTOR_ELT(listParams, 1, LLF);

	UNPROTECT(5);
	return listParams;
}
