#include "SFSI.h"
//#include "utils.c"

// ----------------------------------------------------------
// Transform a covariance matrix to a distance matrix
// The distance between variables i and j is
// d_ij = a*sqrt(v_ii + v_jj -2*v_ij)
// where v_ii is the variance (diagonal value) of the variable i.
//
//    A:       Covariance matrix of dimension n
// ----------------------------------------------------------
SEXP R_cov2dist(SEXP n_, SEXP a_, SEXP A_)
{
    long long i, j;

    int n = INTEGER_VALUE(n_);
    double a = NUMERIC_VALUE(a_);

    double *v = (double *) R_alloc(n, sizeof(double)); // Variances: Diagonal values

    PROTECT(A_ = AS_NUMERIC(A_));
    double *A = NUMERIC_POINTER(A_);

    for(j=0; j<n; j++){   // Diagonal values
      v[j] = A[n*j + j];
      A[n*j + j] = 0;
    }

    for(j=0; j<n-1; j++){
      for(i=j+1; i<n; i++){
        A[n*j + i] = a*sqrt(v[i]+v[j] -2*A[n*j + i]);
        A[n*i + j] = a*sqrt(v[i]+v[j] -2*A[n*i + j]);
      }
    }

    UNPROTECT(1);

    return(R_NilValue);
}
