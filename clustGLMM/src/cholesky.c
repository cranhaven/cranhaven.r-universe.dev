/* file: cholesky.c */

/* Take the cholesky decomposition in the manner described in FA Graybill
(1976).
*/

#include <R.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/** Original version from the internet **/

int cholesky_orig(double **orig, int n, double **aug, int mcol,double **chol, double **cholaug, int ofs)
  /* 
  Do the augmented cholesky decomposition as described in FA Graybill
  (1976) Theory and Application of the Linear Model. The original matrix
  must be symmetric positive definite. The augmentation matrix, or
  series of column vectors, are multiplied by C^-t, where C is the
  upper triangular cholesky matrix, ie C^t * C = M and M is the original
  matrix. Returns with a value of 0 if M is a non-positive definite 
  matrix. Returns with a value of 1 with succesful completion.
  
  Arguments:
  
  orig (input) double n x n array. The matrix to take the Cholesky
  decomposition of.
  n    (input) integer. Number of rows and columns in orig.
  aug  (input) double n x mcol array. The matrix for the augmented
  part of the decomposition.
  mcol (input) integer. Number of columns in aug.
  chol (output) double n x n array. Holds the upper triangular matrix
  C on output. The lower triangular portion remains unchanged.
  This maybe the same as orig, in which case the upper triangular
  portion of orig is overwritten.
   
  cholaug (output) double n x mcol array. Holds the product C^-t * aug.
  May be the same as aug, in which case aug is over written.
  
  This is very useful for generating from N(mu,Sigma) distribution when we have InvSigma
  Do  1) InvSigma = C^T * C, where C is Cholesky decomposition (upper triangular)
      -) If Z ~ N(0,I) then var(C^{-1} * Z) = C^{-1} * I * C^{-T} = (C^T * C)^{-1} = InvSigma^{-1} = Sigma
      2) So to get X ~ N(0,Sigma) do X = C^{-1} * Z 
  So the function needs to be modified to do C^{-1} * aug and not C^{-T} * aug 
   
  ofs (input) integer. The index of the first element in the matrices.
  Normally this is 0, but commonly is 1 (but may be any integer).
  */
{
  int i, j, k, l;
  int retval = 1;
  
  for (i=ofs; i<n+ofs; i++) {
    chol[i][i] = orig[i][i];
    for (k=ofs; k<i; k++)
      chol[i][i] -= chol[k][i]*chol[k][i];
    if (chol[i][i] <= 0) {
      //fprintf(stderr,"\nERROR: non-positive definite matrix!\n");
      Rprintf("\nERROR: non-positive definite matrix!\n");
      Rprintf("\nproblem from %d %f\n",i,chol[i][i]);
      retval = 0;
      return retval;
    }
    chol[i][i] = sqrt(chol[i][i]);
    
    /*This portion multiplies the extra matrix by C^-t */
    /*for (l=ofs; l<mcol+ofs; l++) {
     *  cholaug[i][l] = aug[i][l];
     *  for (k=ofs; k<i; k++) {
     *    cholaug[i][l] -= cholaug[k][l]*chol[k][i];
     *  }
     *  cholaug[i][l] /= chol[i][i];
     *}
     */
    
    for (j=i+1; j<n+ofs; j++) {
      chol[i][j] = orig[i][j];
      for (k=ofs; k<i; k++)
        chol[i][j] -= chol[k][i]*chol[k][j];
      chol[i][j] /= chol[i][i];
    }
  }
  
  /*This portion multiplies the extra matrix by C^-1 */
  for (i=n+ofs-1; i>=ofs; i--) {
    for (l=ofs; l<mcol+ofs; l++) {
      cholaug[i][l] = aug[i][l];
      for (k=i+1; k<n+ofs; k++) {
        cholaug[i][l] -= cholaug[k][l]*chol[i][k];
      }
      cholaug[i][l] /= chol[i][i];
    }
  }
  
  return retval;
}

int cholesky_solve(double* orig,     // [n * (n+1)/2]  IN  matrix to be CH decomposed
                   int* n,           // [1]            IN  dimension of the matrix 
                   double* aug,      // [n]            IN  right hand side of equation chol * x = aug
                   double* chol,     // [n * (n+1)/2]  OUT upper triangular matrix C, satisfying C^T * C = orig 
                   double* cholaug)  // [n]            OUT solution of equation chol * x = aug
                   
  /*
   * This is very useful for solution of equations Sigma * x = b, where Sigma > 0
   * Sigma is stored as symmetric matrix
   * aug contains the righthand side of the equation
   * cholaug will contain C^{-T} righthand side --> it remains to solve chol * x = cholaug --> can be done by backsolving
   */
{
  int i, j, k, l;
  int diag, coord;
  int retval = 1;
  
  for(i = 0; i < *n; i++) {
    diag = i*(i+3)/2;
    chol[diag] = orig[diag];
    for(k = 0; k < i; k++)
      chol[diag] -= chol[k + i*(i+1)/2]*chol[k + i*(i+1)/2];
    if (chol[diag] <= 0) {
      //fprintf(stderr,"\nERROR: non-positive definite matrix!\n");
      Rprintf("\nERROR: non-positive definite matrix!\n");
      Rprintf("\nproblem from %d %f\n",i,chol[diag]);
      //fflush(stdout);
      retval = 0;
      return retval;
    }
    chol[diag] = sqrt(chol[diag]);
    
    /*This portion multiplies the extra matrix by C^-t */
    
    cholaug[i] = aug[i];
    for (l = 0; l < i; l++) {
      cholaug[i] -= cholaug[l]*chol[l + i*(i+1)/2];
    }
    cholaug[i] /= chol[diag];
    
    
    for (j = i + 1; j < *n; j++) {
      coord = i + j*(j+1)/2;
      chol[coord] = orig[coord];
      for (k = 0; k < i; k++)
        chol[coord] -= chol[k + i*(i+1)/2]*chol[k + j*(j+1)/2];
      chol[coord] /= chol[diag];
    }
    
  }

  return retval;
}

/** My version **/
/* Matrices are stored in one long array */
/* Computes also C^{-1} * aug where aug is just one vector */

int cholesky(double* orig,     // [n * n]  IN  matrix to be CH decomposed
             int* n,           // [1]      IN  dimension of the matrix 
             double* aug,      // [n]      IN  right hand side of equation chol * x = aug
             double* chol,     // [n * n]  OUT upper triangular matrix C, satisfying C^T * C = orig 
             double* cholaug)  // [n]      OUT solution of equation chol * x = aug
  /* 
   * This is very useful for generating from N(mu,Sigma) distribution when we have InvSigma
   * Do  1) InvSigma = C^T * C, where C is Cholesky decomposition (upper triangular)
   *     -) If Z ~ N(0,I) then var(C^{-1} * Z) = C^{-1} * I * C^{-T} = (C^T * C)^{-1} = InvSigma^{-1} = Sigma
   *     2) So to get X ~ N(0,Sigma) do X = C^{-1} * Z 
   * So the function needs to be modified to do C^{-1} * aug and not C^{-T} * aug 
   */
  
{
  int i, j, k; // looping indeces
  int retval = 1;
  
  for (i = 0; i < *n; i++) {
    chol[i + *n * i] = orig[i + *n * i];
    for (k = 0; k < i; k++)
      chol[i + *n * i] -= chol[k + *n * i]*chol[k + *n * i];
    if (chol[i + *n * i] <= 0) {
      //fprintf(stderr,"\nERROR: non-positive definite matrix!\n");
      Rprintf("\nERROR: non-positive definite matrix!\n");
      Rprintf("\nproblem from %d %f\n",i,chol[i + *n * i]);
      retval = 0;
      return retval;
    }
    chol[i + *n * i] = sqrt(chol[i + *n * i]);
    
    /* 
     * Calculation of cholaug cannot be done in this step, since
     * we do not have all necesarry chol domponents computed yet
     * Needs to be done in separate for cycles
     */
    
    for (j = i+1; j < *n; j++) {
      chol[i + *n * j] = orig[i + *n * j];
      for (k = 0; k < i; k++)
        chol[i + *n * j] -= chol[k + *n * i]*chol[k + *n * j];
      chol[i + *n * j] /= chol[i + *n * i];
    }
  }
  
  /* 
   * This portion multiplies the extra matrix by C^-1 
   * C is upper triangular matrix, so we need to solve it from the last one
   * That is why the first cycle goes from (n-1) to 0 
   */
  
  for (i = *n-1; i >= 0; i--) {
    cholaug[i] = aug[i];
    for (k = i+1; k < *n; k++) {
      cholaug[i] -= cholaug[k]*chol[i + *n * k];
    }
    cholaug[i] /= chol[i + *n * i];
  }
  
  return retval;
  }



/** My version that spares the space **/
/* 
 * Matrices are stored in one long array,
 * however, only the triangles, that are useful
 * orig is symmetric --> only upper triangle is delivered
 * chol is upper triangluar --> no need to return zeros
 */
/* Computes also C^{-1} * aug where aug is just one vector */

int cholesky2(double* orig,     // [n * (n+1)/2]  IN  matrix to be CH decomposed
              int* n,           // [1]            IN  dimension of the matrix 
              double* aug,      // [n]            IN  right hand side of equation chol * x = aug
              double* chol,     // [n * (n+1)/2]  OUT upper triangular matrix C, satisfying C^T * C = orig 
              double* cholaug,  // [n]            OUT solution of equation chol * x = aug
              double* det)      // [1]            OUT determinant of matrix orig
  /* 
   * This is very useful for generating from N(mu,Sigma) distribution when we have InvSigma
   * Do  1) InvSigma = C^T * C, where C is Cholesky decomposition (upper triangular)
   *     -) If Z ~ N(0,I) then var(C^{-1} * Z) = C^{-1} * I * C^{-T} = (C^T * C)^{-1} = InvSigma^{-1} = Sigma
   *     2) So to get X ~ N(0,Sigma) do X = C^{-1} * Z 
   * So the function needs to be modified to do C^{-1} * aug and not C^{-T} * aug 
   */
{
  int i, j, k; // looping indeces
  int retval = 1;
  
  for (i = 0; i < *n; i++) {
    chol[i*(i+3)/2] = orig[i*(i+3)/2];
    for (k = 0; k < i; k++)
      chol[i*(i+3)/2] -= chol[k + i*(i+1)/2]*chol[k + i*(i+1)/2];
    if (chol[i*(i+3)/2] <= 0) {
      //fprintf(stderr,"\nERROR: non-positive definite matrix!\n");
      Rprintf("\nERROR: non-positive definite matrix!\n");
      Rprintf("\nproblem from %d %f\n",i,chol[i*(i+3)/2]);
      retval = 0;
      return retval;
    }
    chol[i*(i+3)/2] = sqrt(chol[i*(i+3)/2]);
    
    /* 
    * Calculation of cholaug cannot be done in this step, since
    * we do not have all necesarry chol domponents computed yet
    * Needs to be done in separate for cycles
    */
    
    for (j = i+1; j < *n; j++) {
      chol[i + j*(j+1)/2] = orig[i + j*(j+1)/2];
      for (k = 0; k < i; k++)
        chol[i + j*(j+1)/2] -= chol[k + i*(i+1)/2]*chol[k + j*(j+1)/2];
      chol[i + j*(j+1)/2] /= chol[i*(i+3)/2];
    }
  }
  
  /* 
  * This portion multiplies the extra matrix by C^-1 
  * C is upper triangular matrix, so we need to solve it from the last one
  * That is why the first cycle goes from (n-1) to 0 
  */
  
  for (i = *n-1; i >= 0; i--) {
    cholaug[i] = aug[i];
    for (k = i+1; k < *n; k++) {
      cholaug[i] -= cholaug[k]*chol[i + k*(k+1)/2];
    }
    cholaug[i] /= chol[i*(i+3)/2];
  }
  
  /* Determinant calculation */
  *det = 1;
  
  for(i = 0; i < *n; i++){
    *det *= chol[i*(i+3)/2]; // product of elements on the diagonal of chol matrix
  }
  
  *det *= *det; // squared
  
  
  return retval;
}


int justcholesky(double* orig,     // [n * (n+1)/2]  IN  matrix to be CH decomposed
                 double* chol,     // [n * (n+1)/2]  OUT upper triangular matrix C, satisfying C^T * C = orig 
                 int* n)           // [1]            IN  dimension of the matrix 
{
  int i, j, k; // looping indeces
  int retval = 1;
  
  for (i = 0; i < *n; i++) {
    chol[i*(i+3)/2] = orig[i*(i+3)/2];
    for (k = 0; k < i; k++)
      chol[i*(i+3)/2] -= chol[k + i*(i+1)/2]*chol[k + i*(i+1)/2];
    if (chol[i*(i+3)/2] <= 0) {
      //fprintf(stderr,"\nERROR: non-positive definite matrix!\n");
      Rprintf("\nERROR: non-positive definite matrix!\n");
      Rprintf("\nproblem from %d %f\n",i,chol[i*(i+3)/2]);
      retval = 0;
      return retval;
    }
    chol[i*(i+3)/2] = sqrt(chol[i*(i+3)/2]);
    
    /* 
    * Calculation of cholaug cannot be done in this step, since
    * we do not have all necessary chol components computed yet
    * Needs to be done in separate for cycles
    */
    
    for (j = i+1; j < *n; j++) {
      chol[i + j*(j+1)/2] = orig[i + j*(j+1)/2];
      for (k = 0; k < i; k++)
        chol[i + j*(j+1)/2] -= chol[k + i*(i+1)/2]*chol[k + j*(j+1)/2];
      chol[i + j*(j+1)/2] /= chol[i*(i+3)/2];
    }
  }
  
  return retval;
}




/* From choleski decomposition to inverse matrix of the original one */

void choltoinv(double* chol, // [n*(n+1)/2] cholesky decomposition of some original matrix
               double* inv,  // [n*(n+1)/2] inverse of some original matrix
               int* n)       // [1] dimension of the square matrix
{
  int i, j, k; // looping indeces
  int coord;   // coordinates in array
  double* cholinv;
  cholinv = (double*)malloc(*n * (*n+1)/2 * sizeof(double));
  //double cholinv [*n * (*n+1)/2]; // inverse of cholesky decomposition
  
  
  // first calculation of inverse of chol
  
  for(j = 0; j < *n; j++){
    for(i = j; i >= 0; i--){
      coord = i + j*(j+1)/2;
      cholinv[coord] = (double)(i == j); 
      
      for(k = i+1; k <= j; k++){
        cholinv[coord] -= chol[i + k*(k+1)/2] * cholinv[k + j*(j+1)/2];
      }
      cholinv[coord] /= chol[i*(i+3)/2];
    }
  }
  
  // second calculation of inverse of orig
  
  for(i = 0; i < *n; i++){
    for(j = i; j < *n; j++){
      coord = i + j*(j+1)/2;
      inv[coord] = 0;
      
      for(k = j; k < *n; k++){
        inv[coord] += cholinv[i + k*(k+1)/2] * cholinv[j + k*(k+1)/2];
      }
    }
  }
  
  free(cholinv);
  
  
}

void invofchol(double* chol,    // [n*(n+1)/2] cholesky decomposition of some original matrix
               double* cholinv, // [n*(n+1)/2] inverse of some original matrix
               int* n)          // [1] dimension of the square matrix
{
  int i, j, k; // looping indeces
  int coord;   // coordinates in array
  
  // first calculation of inverse of chol
  
  for(j = 0; j < *n; j++){
    for(i = 0; i <= j; i++){
      coord = i + j*(j+1)/2;
      cholinv[coord] = (double)(i == j); 
      
      for(k = i; k < j; k++){
        cholinv[coord] -= cholinv[i + k*(k+1)/2] * chol[k + j*(j+1)/2];
      }
      cholinv[coord] /= chol[j*(j+3)/2];
    }
  }
  
}
