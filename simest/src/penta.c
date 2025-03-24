#include <R_ext/Arith.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <float.h>
#include "simest.h"

/****************************************************************/

void penta(int dim[], double E[], double A[], double D[], 
            double C[], double F[], double B[], double X[]) { 
  long double xmult;
  int matrix_size = dim[0];
  for (int i = 1; i < matrix_size-1; i++){
    xmult = A[i-1]/D[i-1];
    D[i] -= xmult * C[i-1];
    C[i] -= xmult * F[i-1];
    B[i] -= xmult * B[i-1];
    xmult = E[i-1] / D[i-1];
    A[i] -= xmult * C[i-1];
    D[i+1] -= xmult * F[i-1];
    B[i+1] -= xmult * B[i-1];
  }
  xmult = A[matrix_size-2]/D[matrix_size-2];
  D[matrix_size-1] -= xmult * C[matrix_size -2];
  X[matrix_size-1] = (B[matrix_size-1] - xmult * B[matrix_size-2]) / D[matrix_size-1];
  X[matrix_size-2] = (B[matrix_size-2] - C[matrix_size-2] * X[matrix_size-1])/D[matrix_size-2];

  for (int i = matrix_size -3; i>=0; i--)
   X[i] = (B[i]- F[i]* X[i+2] - C[i] * X[i+1]) / D[i];

} /* penta */
