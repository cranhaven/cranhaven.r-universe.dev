#ifndef CHOL_H
#define CHOL_H

int cholesky_orig(double **orig, 
                  int n, 
                  double **aug, 
                  int mcol,
                  double **chol, 
                  double **cholaug, 
                  int ofs);

int cholesky_solve(double* orig,     // [n * (n+1)/2]  IN  matrix to be CH decomposed
                   int* n,           // [1]            IN  dimension of the matrix 
                   double* aug,      // [n]            IN  right hand side of equation chol * x = aug
                   double* chol,     // [n * (n+1)/2]  OUT upper triangular matrix C, satisfying C^T * C = orig 
                   double* cholaug); // [n]            OUT solution of equation chol * x = aug
  

int cholesky(double* orig,     // [n * n]  IN  matrix to be CH decomposed
             int* n,           // [1]      IN  dimension of the matrix 
             double* aug,      // [n]      IN  right hand side of equation chol * x = aug
             double* chol,     // [n * n]  OUT upper triangular matrix C, satisfying C^T * C = orig 
             double* cholaug); // [n]      OUT solution of equation chol * x = aug
    
int cholesky2(double* orig,     // [n * (n+1)/2]  IN  matrix to be CH decomposed
              int* n,           // [1]            IN  dimension of the matrix 
              double* aug,      // [n]            IN  right hand side of equation chol * x = aug
              double* chol,     // [n * (n+1)/2]  OUT upper triangular matrix C, satisfying C^T * C = orig 
              double* cholaug,  // [n]            OUT solution of equation chol * x = aug
              double* det);     // [1]            OUT determinant of matrix orig
      
int justcholesky(double* orig,     // [n * (n+1)/2]  IN  matrix to be CH decomposed
                 double* chol,     // [n * (n+1)/2]  OUT upper triangular matrix C, satisfying C^T * C = orig 
                 int* n);          // [1]            IN  dimension of the matrix 
  
void choltoinv(double* chol, // [n*(n+1)/2] cholesky decomposition of some original matrix
               double* inv,  // [n(n+1)/2] inverse of some original matrix
               int* n);      // [1] dimension of the square matrix

void invofchol(double* chol,    // [n*(n+1)/2] cholesky decomposition of some original matrix
               double* cholinv, // [n*(n+1)/2] inverse of some original matrix
               int* n);         // [1] dimension of the square matrix
  
#endif
