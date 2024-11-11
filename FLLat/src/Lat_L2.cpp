/*************************************************************************/
//This file contains LatL2C, the C function for running the Latent Feature
//Model under the L2 constraint, and TLatL2CR, the C function for
//calculating the Theta matrix for a new sample(s) when given the Beta
//matrix (i.e., a wrapper for TLatL2C that can be called easily from R).
/*************************************************************************/

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>
#include "gen_lat_func.h"


/*************************************************************************/
// FUNCTIONS TO BE CALLED FROM R.
/*************************************************************************/

extern "C" SEXP LatL2C(SEXP Y, SEXP nF, SEXP inB, SEXP inT, SEXP lam1,
		       SEXP lam2, SEXP thresh, SEXP maxiter, SEXP maxiterB,
		       SEXP maxiterT,SEXP sT) {

  int nProt = 0;
  double *pY = REAL(Y), *pinB = REAL(inB), *pinT = REAL(inT),
    rlam1 = REAL(lam1)[0], rlam2 = REAL(lam2)[0],
    rthresh = REAL(thresh)[0], rsT = REAL(sT)[0];
  int imaxiter = INTEGER(maxiter)[0], imaxiterB = INTEGER(maxiterB)[0],
    imaxiterT = INTEGER(maxiterT)[0];
  R_len_t S = ncols(Y), L = nrows(Y), J = INTEGER(nF)[0];

  //Initializing Beta.
  SEXP newB = R_NilValue;
  PROTECT(newB = allocMatrix(REALSXP,L,J));
  nProt++;
  double *pnewB = REAL(newB), *poldB = new double[L*J];
  CopyAtoB(pinB,poldB,L*J);
  CopyAtoB(pinB,pnewB,L*J);
  //Initializing Theta.
  SEXP newT = R_NilValue;
  PROTECT(newT = allocMatrix(REALSXP,J,S));
  nProt++;
  double *pnewT = REAL(newT), *poldT = new double[J*S];
  CopyAtoB(pinT,poldT,J*S);
  CopyAtoB(pinT,pnewT,J*S);

  double *perrBs = new double[imaxiter+1],
    *perrTs = new double[imaxiter+1], BSqS = SqTotSum(poldB,L*J);
  perrBs[0] = rthresh +1.0;
  perrTs[0] = rthresh + 1.0;
  int niter = 0;

  while (((perrBs[niter]>rthresh)||(perrTs[niter]>rthresh))&&
	 (niter<imaxiter)&&(BSqS!=0.0)) {
    TLatL2C(pnewT,pY,pnewB,rthresh,imaxiterT,rsT,S,L,J);
    perrTs[niter+1] = MatErr(pnewT,poldT,J*S,rthresh);
    CopyAtoB(pnewT,poldT,J*S);
    BC(pnewB,pY,pnewT,rlam1,rlam2,rthresh,imaxiterB,S,L,J);
    perrBs[niter+1] = MatErr(pnewB,poldB,L*J,rthresh);
    CopyAtoB(pnewB,poldB,L*J);
    BSqS = SqTotSum(poldB,L*J);
    niter++;
  }

  SEXP Rniter = R_NilValue, rss = R_NilValue, bic = R_NilValue,
    retlam1 = R_NilValue, retlam2 = R_NilValue;
  // The RSS.
  PROTECT(rss = allocVector(REALSXP,1));
  nProt++;
  REAL(rss)[0] = LatRSS(pY,pnewB,pnewT,S,L,J);
  // The BIC.
  PROTECT(bic = allocVector(REALSXP,1));
  nProt++;
  REAL(bic)[0] = LatBIC(REAL(rss)[0],pnewB,S,L,J);
  // The number of iterations.
  PROTECT(Rniter = allocVector(INTSXP,1));
  nProt++;
  INTEGER(Rniter)[0] = niter;
  // Lambda 1.
  PROTECT(retlam1 = allocVector(REALSXP,1));
  nProt++;
  REAL(retlam1)[0] = rlam1;
  // Lambda 2.
  PROTECT(retlam2 = allocVector(REALSXP,1));
  nProt++;
  REAL(retlam2)[0] = rlam2;

  //The results.
  SEXP res = R_NilValue, resNames = R_NilValue, resClass = R_NilValue;
  //The list of output variables.
  PROTECT(res = allocVector(VECSXP,7));
  nProt++;
  SET_VECTOR_ELT(res,0,newB);
  SET_VECTOR_ELT(res,1,newT);
  SET_VECTOR_ELT(res,2,Rniter);
  SET_VECTOR_ELT(res,3,rss);
  SET_VECTOR_ELT(res,4,bic);
  SET_VECTOR_ELT(res,5,retlam1);
  SET_VECTOR_ELT(res,6,retlam2);
  //The list of output variable names.
  PROTECT(resNames =allocVector(STRSXP,7));
  nProt++;
  SET_STRING_ELT(resNames,0,mkChar("Beta"));
  SET_STRING_ELT(resNames,1,mkChar("Theta"));
  SET_STRING_ELT(resNames,2,mkChar("niter"));
  SET_STRING_ELT(resNames,3,mkChar("rss"));
  SET_STRING_ELT(resNames,4,mkChar("bic"));
  SET_STRING_ELT(resNames,5,mkChar("lam1"));
  SET_STRING_ELT(resNames,6,mkChar("lam2"));
  //Setting the names to the output variables.
  setAttrib(res,R_NamesSymbol,resNames);
  //Setting the class to the output list.
  PROTECT(resClass = allocVector(STRSXP,1));
  nProt++;
  SET_STRING_ELT(resClass,0,mkChar("FLLat"));
  classgets(res,resClass);

  delete [] perrBs;
  delete [] perrTs;
  delete [] poldB;
  delete [] poldT;
  UNPROTECT(nProt);
  return(res);

}

extern "C" SEXP TLatL2CR(SEXP Y, SEXP B, SEXP inT, SEXP thresh,
			 SEXP maxiterT, SEXP sT) {

  int nProt = 0;
  double *pY = REAL(Y), *pB = REAL(B), *pinT = REAL(inT),
    rthresh = REAL(thresh)[0], rsT = REAL(sT)[0];
  int imaxiterT = INTEGER(maxiterT)[0];
  R_len_t S = ncols(Y), L = nrows(Y), J = ncols(B);

  // Initializing Theta.
  SEXP newT = R_NilValue;
  PROTECT(newT = allocMatrix(REALSXP,J,S));
  nProt++;
  double *pnewT = REAL(newT);
  CopyAtoB(pinT,pnewT,J*S);

  // Calculating Theta.
  int niter = TLatL2C(pnewT,pY,pB,rthresh,imaxiterT,rsT,S,L,J);

  SEXP Rniter = R_NilValue, rss = R_NilValue;
  // The RSS.
  PROTECT(rss = allocVector(REALSXP,1));
  nProt++;
  REAL(rss)[0] = LatRSS(pY,pB,pnewT,S,L,J);
  // The number of iterations.
  PROTECT(Rniter = allocVector(INTSXP,1));
  nProt++;
  INTEGER(Rniter)[0] = niter;

  // The results.
  SEXP res = R_NilValue, resNames = R_NilValue;
  // The list of output variables.
  PROTECT(res = allocVector(VECSXP,3));
  nProt++;
  SET_VECTOR_ELT(res,0,newT);
  SET_VECTOR_ELT(res,1,Rniter);
  SET_VECTOR_ELT(res,2,rss);
  // The list of output variable names.
  PROTECT(resNames = allocVector(STRSXP,3));
  nProt++;
  SET_STRING_ELT(resNames,0,mkChar("Theta"));
  SET_STRING_ELT(resNames,1,mkChar("niter"));
  SET_STRING_ELT(resNames,2,mkChar("rss"));
  // Setting the names to the output variables.
  setAttrib(res,R_NamesSymbol,resNames);

  UNPROTECT(nProt);
  return(res);

}


/*************************************************************************/
// REGISTERING NATIVE ROUTINES.
/*************************************************************************/

static const R_CallMethodDef CallEntries[] = {
  {"LatL2C", (DL_FUNC) &LatL2C, 11},
  {"TLatL2CR", (DL_FUNC) &TLatL2CR, 6},
  {NULL, NULL, 0}
};

extern "C" void R_init_FLLat(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
