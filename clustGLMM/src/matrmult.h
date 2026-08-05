#ifndef MATRMULT_H
#define MATRMULT_H

void matprod(double* m1,   // IN  [p * q]
             double* m2,   // IN  [q * r]
             double* m1m2, // OUT [p * r]
             int* p, // IN
             int* q, // IN
             int* r); // IN
  
void XtX(double* m,    // IN  [p * q]
         double* mtm,  // OUT [q * q]
         int* p, // IN  [1] row dimension of m
         int* q); // IN  [1] col dimension of m

void aBa(double* a,    // IN  [p]
         double* B,    // IN  [p * (p+1)/2]
         double* res,  // OUT [1] 
         int* p); // IN  [1] row and col dimension of a and B

void aBa2(double* a,    // IN  [p]
          double* B,    // IN  [p * p]
          double* res,  // OUT [1] 
          int* p); // IN  [1] row and col dimension of a and B

void Ba(double* a,    // IN [p]
        double* B,    // IN [p*(p+1)/2]
        double* res,  // OUT [p]
        int* p);       // IN  [1] row and col dimension of a and B

void minusBa(double* a,    // IN [p]
        double* B,    // IN [p*(p+1)/2]
        double* res,  // OUT [p]
        int* p);       // IN  [1] row and col dimension of a and B

void backsolve2(double* M, // IN  [p*(p+1)/2] upper trianglar matrix (by columns) 
                double* b, // IN  [p] right-hand side of the equation
                int*    p, // IN  [1] dimension of the matrix
                double* x); // OUT [p] the solution

void forwardsolve2(double* M, // IN  [p*(p+1)/2] upper trianglar matrix (by columns)
                   double* b, // IN  [p] right-hand side of the equation
                   int*    p, // IN  [1] dimension of the matrix
                   double* x); // OUT [p] the solution

void detchol(double* chol, // IN  [p * (p+1)/2] uppertriangluar matrix from cholesky decomposition
             double* det,  // OUT [1] determinant of original matrix
             int* p);      // IN  [1] dimension of matrix

#endif
