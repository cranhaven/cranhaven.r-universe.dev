/*************************************************************************/
// This file contains the C++ code for calculating the unnormalized A
// matrix.
/*************************************************************************/

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>


/*************************************************************************/
// PRIVATE FUNCTION PROTOTYPES.
/*************************************************************************/

void RowDes(int* pM, int row, int n, int* pndes, int* pdes);
void EntryDes(int* pM, int entry, int n, int* pndes, int* pdes);


/*************************************************************************/
// FUNCTIONS TO BE CALLED FROM R.
/*************************************************************************/

/*************************************************************************/
// UnNormA() returns the unnormalized A matrix.  The R wrapper function needs
// to divide the A matrix by the sum of the delta h's.
// Arguments:
// M - The merge matrix.
// hts - The vector of delta h's.
/*************************************************************************/
extern "C" SEXP UnNormA(SEXP M, SEXP hts) {

  // Protect counter, number of samples and pointers to matrices.
  int nProt = 0, *pM = INTEGER(M), n = nrows(M)+1;
  double *phts = REAL(hts);

  // Initializing the array pS that will keep track of the updates to A.
  double *pS = new double[n*n];
  for (int i = 0; i < (n*n); i++) {
    pS[i] = 0.0;
  }
  for (int i = 0; i < n; i++) {
    pS[(n+1)*i] = 1.0;
  }

  // Initializing the unnormalized A matrix.
  SEXP uA = R_NilValue;
  PROTECT(uA = allocMatrix(REALSXP,n,n));
  nProt++;
  double *puA = REAL(uA);
  for (int i = 0; i < (n*n); i++) {
    puA[i] = phts[0]*pS[i];
  }

  // Calculating the unnormalized A matrix.
  int ndes;
  int *pndes = &ndes;
  for (int i = 1; i < (n-1); i++) {
    int *pdes = new int[n];
    ndes = 0;
    RowDes(pM, i, n, pndes, pdes);
    for (int j = 0; j < ndes; j++) {
      for (int k = 0; k < ndes; k++) {
	pS[pdes[j]-1+n*(pdes[k]-1)] = (1.0)/ndes;
      }
    }
    for (int l = 0; l < n*n; l++) {
      puA[l] += phts[i]*pS[l];
    }
    delete [] pdes;
  }

  delete [] pS;
  UNPROTECT(nProt);
  return(uA);
}


/*************************************************************************/
// FUNCTIONS NOT CALLED FROM R.
/*************************************************************************/

/*************************************************************************/
// RowDes() is a recursive function that determines the descendents of a
// row of the merge matrix.
// Arguments:
// pM - Pointer to the merge matrix.
// row - A particular row of the merge matrix.
// n - The number of columns in the data.
// pndes - A pointer to the number of descendents.
// pdes - An array of the descendents.
/*************************************************************************/
void RowDes(int* pM, int row, int n, int* pndes, int* pdes) {
  EntryDes(pM, row, n, pndes, pdes);
  EntryDes(pM, row+n-1, n, pndes, pdes);
}

/*************************************************************************/
// EntryDes() is a recursive function that determines the descendents of
// an entry of the merge matrix.
// Arguments:
// pM - Pointer to the merge matrix.
// entry - A particular entry of the merge matrix.
// n - The number of columns in the data.
// pndes - A pointer to the number of descendents.
// pdes - An array of the descendents.
void EntryDes(int* pM, int entry, int n, int* pndes, int* pdes) {
  if (pM[entry-1]<0) {
    pdes[*pndes] = -pM[entry-1];
    (*pndes) += 1;
  } else {
    RowDes(pM, pM[entry-1], n, pndes, pdes);
  }
}


/*************************************************************************/
// REGISTERING NATIVE ROUTINES.
/*************************************************************************/

static const R_CallMethodDef CallEntries[] = {
  {"UnNormA", (DL_FUNC) &UnNormA, 2},
  {NULL, NULL, 0}
};

extern "C" void R_init_compHclust(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
