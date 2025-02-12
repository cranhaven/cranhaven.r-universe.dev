#include "SFSI.h"
//#include "utils.c"

// ----------------------------------------------------------
// Transform a covariance matrix to a correlation matrix
// The correlation between variables i and j is
// r_ij = v_ij/(sqrt(v_ii)*sqrt(v_jj))
// where sqrt(v_ii) is the SD of the variable i.
//
//    A:       Covariance matrix of dimension n
// ----------------------------------------------------------
SEXP R_cov2cor(SEXP n_, SEXP a_, SEXP A_)
{
    long long i, j;
    int nOK = 0;

    int n = INTEGER_VALUE(n_);
    double a = NUMERIC_VALUE(a_);

    double *sd = (double *) R_alloc(n, sizeof(double)); // Standard deviation (diagonal values)

    PROTECT(A_ = AS_NUMERIC(A_));
    double *A = NUMERIC_POINTER(A_);

    for(j=0; j<n; j++){
      sd[j] = sqrt(A[n*j + j]);
      A[n*j + j] = a*1;
      nOK += isfinite(sd[j]);
    }
    for(j=0; j<n-1; j++){
      for(i=j+1; i<n; i++){
        A[n*j + i] = a*A[n*j + i]/(sd[j]*sd[i]);
        A[n*i + j] = a*A[n*i + j]/(sd[j]*sd[i]);
      }
    }

    UNPROTECT(1);

    return(Rf_ScalarInteger(nOK));
}
