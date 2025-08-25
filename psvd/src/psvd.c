
/***************************************************************************/
#include<R.h>
#include<Rmath.h>
#include <Rinternals.h>

#include "psvd.h"

/***************************************************************************/
/* This function in used in calcSVD() */
SEXP mGS(SEXP amatC, SEXP mC, SEXP nC) { 
  int i, j, k, m, n, mn, nrow, ncol;
  double ri, rj, tmp, *amat, *vmat, *ivect, *jvect;
  SEXP vmatC, results;
  static const char *resultNames[]={"wp", ""};

  amat = REAL(amatC);
  m = asInteger(mC);
  n = asInteger(nC);
  nrow = m;
  if (m >= n) { ncol = n;  } else {ncol = m;}
  mn = nrow * ncol;
  ivect = (double *)R_alloc(nrow, sizeof(double));
  jvect = (double *)R_alloc(nrow, sizeof(double));

  PROTECT(results = mkNamed(VECSXP, resultNames));
  vmatC = SET_VECTOR_ELT(results, 0, allocVector(REALSXP, mn));
  vmat = REAL(vmatC);
  
  for (i=0; i<mn; i++) vmat[i] = amat[i];
  for (i=0; i<ncol; i++) {
      for (k=0; k<nrow; k++) ivect[k] = vmat[i*nrow+k];
      tmp = 0.0;
      for (k=0; k<nrow; k++) tmp += ivect[k]*ivect[k];
      ri = sqrt(tmp);
      for (k=0; k<nrow; k++) vmat[i*nrow+k] /= ri;
      if (i < (ncol-1)) {
         for (k=0; k<nrow; k++) ivect[k] = vmat[i*nrow+k];
         for (j=i+1; j<ncol; j++) {
             for (k=0; k<nrow; k++) jvect[k] = vmat[j*nrow+k];
             rj = 0.0;
             for (k=0; k<nrow; k++) rj += ivect[k]*jvect[k];
             for (k=0; k<nrow; k++) vmat[j*nrow+k] -= rj*ivect[k];
         }
      }
  }
  
  UNPROTECT(1);
  return results;
} /* end of function mGS()  */
/***************************************************************************/

/***************************************************************************/
/* This function in used in eigenCalc() */
void mGS2(double *amat, int m, int n, double *vmat) {
  int i, j, k, mn, nrow, ncol;
  double ri, rj, tmp, *ivect, *jvect;

  nrow = m;
  if (m >= n) {ncol = n;} else {ncol = m;}
  mn = nrow*ncol;
  ivect = (double *)R_alloc(nrow, sizeof(double));
  jvect = (double *)R_alloc(nrow, sizeof(double));

  for (i=0; i<mn; i++) vmat[i] = amat[i];
  for (i=0; i<ncol; i++) {
      for (k=0; k<nrow; k++) ivect[k] = vmat[i*nrow+k];
      tmp = 0.0;
      for (k=0; k<nrow; k++) tmp += ivect[k]*ivect[k];
      ri = sqrt(tmp);
      for (k=0; k<nrow; k++) vmat[i*nrow+k] /= ri;
      if (i < (ncol-1)) {
         for (k=0; k<nrow; k++) ivect[k] = vmat[i*nrow+k];
         for (j=i+1; j<ncol; j++) {
             for (k=0; k<nrow; k++) jvect[k] = vmat[j*nrow+k];
             rj = 0.0;
             for (k=0; k<nrow; k++) rj += ivect[k]*jvect[k];
             for (k=0; k<nrow; k++) vmat[j*nrow+k] -= rj*ivect[k];
         }
      }
  }
} /* end of function mGS2()  */
/***************************************************************************/

/***************************************************************************/
/* This function is used in eigenCalc() */
void prodMat(double *amat, int m, int n, double *bmat, int p, int q, 
                    double *cmat) {
  int i, j, k;
  double *ivect, *jvect;

  ivect = (double *)R_alloc(n, sizeof(double));
  jvect = (double *)R_alloc(n, sizeof(double));
  /* check size compatibility of the matrices before their multiplication*/
  
  /* for compatible matrices sizes: n=p*/
  for (i=0; i<m; i++) {
      for (k=0; k<n; k++) ivect[k] = amat[i+k*m];
      for (j=0; j<q; j++) {
          for (k=0; k<n; k++) jvect[k] = bmat[k+j*n];
          cmat[i+j*m] = 0.0;
          for (k=0; k<n; k++) cmat[i+j*m] += ivect[k]*jvect[k];
      }
  }
} /* end of function prodMat()  */
/***************************************************************************/

/***************************************************************************/
/* Eigendecomposition of a square symmetric matrix using power method*/
/* This function is used in calcSVD() */
SEXP eigenV(SEXP xmatC, SEXP wpC, SEXP dC, SEXP rC, SEXP itmaxC, SEXP errC) {
  int k, d, r, dr, it, *iter, itmax;
     double *wt, *we, *wc, err, *xmat, *wp, terr;
     SEXP wcC, iterC, results;
     static const char *resultNames[]={"wc", "iter", ""};

     xmat = REAL(xmatC);
     wp = REAL(wpC);
     d = asInteger(dC);
     r = asInteger(rC);
     itmax = asInteger(itmaxC);
     err = asReal(errC);
     dr = d*r;
     wt = (double *)R_alloc(dr, sizeof(double));
     we = (double *)R_alloc(dr, sizeof(double));

     PROTECT(results = mkNamed(VECSXP, resultNames));
     wcC = SET_VECTOR_ELT(results, 0, allocVector(REALSXP, dr));
     wc = REAL(wcC);
     iterC = SET_VECTOR_ELT(results, 1, allocVector(INTSXP, 1));
     iter = INTEGER(iterC);

     iter[0] = 1;
     for (it=0; it<itmax; it++) {
         prodMat(xmat, d, d, wp, d, r, wt);
         mGS2(wt, d, r, wc);
         for (k=0; k<dr; k++) we[k] = wp[k] - wc[k];
         for (k=0; k<dr; k++) wp[k] = wc[k];
	 terr = 0.0;
	 for (k=0; k<dr; k++) terr += we[k] * we[k];
         if (sqrt(terr) <= err) { break; } else { iter[0] += 1; }
     }
     UNPROTECT(1);
     return results;
}/* end of function eigenVectC()  */
/***************************************************************************/

