#include <R.h>
#include <Rdefines.h>
#include "EMGLLF.h"

// See comments in src/sources/EMGLLF.c and R/EMGLLF.R (wrapper)
SEXP EMGLLF(
	SEXP phiInit_,
	SEXP rhoInit_,
	SEXP piInit_,
	SEXP gamInit_,
	SEXP mini_,
	SEXP maxi_,
	SEXP gamma_,
	SEXP lambda_,
	SEXP X_,
	SEXP Y_,
	SEXP eps_
) {
	// Get matrices dimensions
	int n = INTEGER(getAttrib(X_, R_DimSymbol))[0];
	SEXP dim = getAttrib(phiInit_, R_DimSymbol);
	int p = INTEGER(dim)[0];
	int m = INTEGER(dim)[1];
	int k = INTEGER(dim)[2];

	////////////
	// INPUTS //
	////////////

	// get scalar parameters
	int mini = INTEGER_VALUE(mini_);
	int maxi = INTEGER_VALUE(maxi_);
	double gamma = NUMERIC_VALUE(gamma_);
	double lambda = NUMERIC_VALUE(lambda_);
	double eps = NUMERIC_VALUE(eps_);

	// Get pointers from SEXP arrays ; WARNING: by columns !
	double* phiInit = REAL(phiInit_);
	double* rhoInit = REAL(rhoInit_);
	double* piInit = REAL(piInit_);
	double* gamInit = REAL(gamInit_);
	double* X = REAL(X_);
	double* Y = REAL(Y_);

	/////////////
	// OUTPUTS //
	/////////////

	SEXP phi, rho, pi, llh, S, affec, dimPhiS, dimRho;
	PROTECT(dimPhiS = allocVector(INTSXP, 3));
	int* pDimPhiS = INTEGER(dimPhiS);
	pDimPhiS[0] = p; pDimPhiS[1] = m; pDimPhiS[2] = k;
	PROTECT(dimRho = allocVector(INTSXP, 3));
	int* pDimRho = INTEGER(dimRho);
	pDimRho[0] = m; pDimRho[1] = m; pDimRho[2] = k;
	PROTECT(phi = allocArray(REALSXP, dimPhiS));
	PROTECT(rho = allocArray(REALSXP, dimRho));
	PROTECT(pi = allocVector(REALSXP, k));
	PROTECT(llh = allocVector(REALSXP, 1));
	PROTECT(S = allocArray(REALSXP, dimPhiS));
	PROTECT(affec = allocVector(INTSXP, n));
	double *pPhi=REAL(phi), *pRho=REAL(rho), *pPi=REAL(pi), *pLlh=REAL(llh), *pS=REAL(S);
	int *pAffec=INTEGER(affec);

	////////////////////
	// Call to EMGLLF //
	////////////////////

	EMGLLF_core(phiInit,rhoInit,piInit,gamInit,mini,maxi,gamma,lambda,X,Y,eps,
		pPhi,pRho,pPi,pLlh,pS,pAffec,
		n,p,m,k);

	// Build list from OUT params and return it
	SEXP listParams, listNames;
	int nouts = 6;
	PROTECT(listParams = allocVector(VECSXP, nouts));
	char* lnames[6] = {"phi", "rho", "pi", "llh", "S", "affec"}; //lists labels
	PROTECT(listNames = allocVector(STRSXP,nouts));
	for (int i=0; i<nouts; i++)
		SET_STRING_ELT(listNames,i,mkChar(lnames[i]));
	setAttrib(listParams, R_NamesSymbol, listNames);
	SET_VECTOR_ELT(listParams, 0, phi);
	SET_VECTOR_ELT(listParams, 1, rho);
	SET_VECTOR_ELT(listParams, 2, pi);
	SET_VECTOR_ELT(listParams, 3, llh);
	SET_VECTOR_ELT(listParams, 4, S);
	SET_VECTOR_ELT(listParams, 5, affec);

	UNPROTECT(10);
	return listParams;
}
