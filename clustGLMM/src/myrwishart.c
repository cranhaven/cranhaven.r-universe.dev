/*
 * Generating from Wishart distribution
 */


#include <R.h>
#include <Rmath.h>
#include <math.h>

void my_rwishart(double* gen,    // OUT [p*(p+1)/2] Wishart distributed matrix
              double* cholV,  // IN  [p*(p+1)/2] Choleski decomposition (upper triangle) of scale matrix
              double* df,     // degrees of freedom
              int* p          // dimension of the matrix
              )    
{ 
  /* Declarations */
  int i, j, k;                // looping indeces
  int coord;                  // coordinates in upper triangular matrix
  int icol, jcol;             // column in upper triangular matrix
  //int min;                    // minimum of i and j
  double* A;
  A = (double*)malloc(*p*(*p+1)/2 * sizeof(double));
  //double A[*p*(*p+1)/2];      // upper triangular matrix filled with Chi^2 and N(0,1) variables
  double* AcholV;
  AcholV = (double*)malloc(*p*(*p+1)/2 * sizeof(double));
  //double AcholV[*p*(*p+1)/2]; // upper triangular matrix A * cholV
  
  /* Generating Wishart distributed matrix */
  
  // generating A 
  for(i = 0; i < *p; i++){
    // diagonal elements ~ Chi^2
    A[i*(i+3)/2] = sqrt(rchisq(*df - i));
    
    // off-diagonal elements ~ N(0,1)
    for(j = i+1; j < *p; j++){
      A[i + j*(j+1)/2] = rnorm(0.0, 1.0);
    }
  }
  
  // multiplying A * cholV
  for(i = 0; i < *p; i++){
    for(j = i; j < *p; j++){
      jcol = j*(j+1)/2;
      coord = i + jcol;
      AcholV[coord] = 0;
      for(k = i; k <= j; k++){
        AcholV[coord] += A[i + k*(k+1)/2] * cholV[k + jcol];
      }
    }
  }
  
  // the result is t(AcholV) * AcholV - symmetric matrix (upper triangle saved)
  for(i = 0; i < *p; i++){
    for(j = i; j < *p; j++){
      icol = i*(i+1)/2;
      jcol = j*(j+1)/2;
      coord = i + jcol;
      gen[coord] = 0;
      //min = (i < j) ? i : j;
      for(k = 0; k <= i; k++){
        gen[coord] += AcholV[k + icol] * AcholV[k + jcol];
      }
    }
  }
  
  free(A);
  free(AcholV);
}

