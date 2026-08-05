#include <R.h>
#include <Rmath.h>


/** General Matrix multiplication **/

void matprod(double* m1,   // IN  [p * q]
             double* m2,   // IN  [q * r]
             double* m1m2, // OUT [p * r]
             int* p, // IN
             int* q, // IN
             int* r) // IN
{ 
  /** Declarations **/
  int i, j, k; // looping indeces
  
  /** Cycles in which the matrix multiplication is conducted **/
  for(i = 0; i < *p; i++){
    for(j = 0; j < *r; j++){
      /* Inner cycle = scalar product of i-th column of m1, j-th row of m2 */
      m1m2[i + *p * j] = 0;
      for(k = 0; k < *q; k++)
        m1m2[i + *p * j] += m1[i + *p * k] * m2[k + *q * j];
    }
  }
  return;
}

/** Transposed matrix times the same matrix **/

void XtX(double* m,    // IN  [p * q]
         double* mtm,  // OUT [q * q]
         int* p, // IN  [1] row dimension of m
         int* q) // IN  [1] col dimension of m
{
  /** Declarations **/
  int i, j, k; // looping indeces
    
  /** Cycles in which the matrix multiplication is conducted **/
  // Diagonal 
  for(i = 0; i < *q; i++){
    mtm[i + *q * i] = 0;
    for(k = 0; k < *p; k++)
      mtm[i + *q * i] += m[k + *p * i] * m[k + *p * i];
  }
  
  // Off diagonal  
  for(i = 0; i < *q; i++){
    for(j = i+1; j < *q; j++){
      mtm[i + *q * j] = 0;
      for(k = 0; k < *p; k++)
        mtm[i + *q * j] += m[k + *p * i] * m[k + *p * j];
      // transposition to the other triangle
      mtm[j + *q * i] = mtm[i + *q * j];
    }
  }
  return;
}

/** Quadratic form - transposed vector times symmetric matrix times vector **/
/**  Symmetric matrix stored by  **/

void aBa(double* a,    // IN  [p]
         double* B,    // IN  [p * (p+1)/2]
         double* res,  // OUT [1] 
         int* p) // IN  [1] row and col dimension of a and B
{
  /** Declarations **/
  int i, j, col; // looping indeces
  
  /** Initialization **/
  *res = 0.0;
  
  /** Computation **/
  // Diagonal 
  //for(i = 0; i < *p; i++)
  //  *res += a[i] * a[i] * B[i*(i+3)/2];
  
  // Off diagonal
  //for(j = 0; j < *p; j++){
  //  col = j*(j+1)/2;
  //  for(i = 0; i < j; i++){
  //    *res += 2 * a[i] * a[j] * B[i + col];
  //  }
  //}
  
  for(i = 0; i < *p; i++){
    // Diagonal 
    *res += a[i] * a[i] * B[i*(i+3)/2];
  
    // Off diagonal
    for(j = i+1; j < *p; j++){
      col = j*(j+1)/2;
      *res += 2.0 * a[i] * a[j] * B[i + col];
    }
  }
  // returns 0.0 if *p=0
  return;
}

/** Symmetric matrix is stored as general matrix **/

void aBa2(double* a,    // IN  [p]
          double* B,    // IN  [p * p]
          double* res,  // OUT [1] 
          int* p) // IN  [1] row and col dimension of a and B
{
  /** Declarations **/
  int i, j; // looping indeces
  
  /** Initialization **/
  *res = 0;
  
  /** Computation **/
  // Diagonal 
  for(i = 0; i < *p; i++)
    *res += a[i] * a[i] * B[i + *p * i];
  
  // Off diagonal
  for(j = 0; j < *p; j++){
    for(i = 0; i < j; i++){
      *res += 2 * a[i] * a[j] * B[i + *p * j];
    }
  }
  return;
}

void Ba(double* a,    // IN [p]
        double* B,    // IN [p*(p+1)/2]
        double* res,  // OUT [p]
        int* p)       // IN  [1] row and col dimension of a and B
{
  /** Declarations **/
  int i, j; // looping indeces
  
  for(i = 0; i < *p; i++){
    res[i] = 0;
    // below diag
    for(j = 0; j < i-1; j++){
      res[i] += B[j + i*(i+1)/2] * a[j];
    }
    // diag
    res[i] += B[i*(i+3)/2] * a[i];
    // up diag
    for(j = i+1; j < *p; j++){
      res[i] += B[i + j*(j+1)/2] * a[j];
    }
  }
}

void minusBa(double* a,    // IN [p]
        double* B,    // IN [p*(p+1)/2]
        double* res,  // OUT [p]
        int* p)       // IN  [1] row and col dimension of a and B
{
  /** Declarations **/
  int i, j; // looping indeces
  
  for(i = 0; i < *p; i++){
    res[i] = 0.0;
    // below diag
    for(j = 0; j < i-1; j++){
      res[i] -= B[j + i*(i+1)/2] * a[j];
    }
    // diag
    res[i] -= B[i*(i+3)/2] * a[i];
    // up diag
    for(j = i+1; j < *p; j++){
      res[i] -= B[i + j*(j+1)/2] * a[j];
    }
  }
}


/** Backsolve - from upper triangular matrix **/

void backsolve2(double* M, // IN  [p*(p+1)/2] upper trianglar matrix (by columns) 
                double* b, // IN  [p] right-hand side of the equation
                int*    p, // IN  [1] dimension of the matrix
                double* x) // OUT [p] the solution
{
  int i, k; // looping indeces
  
  for (i = *p-1; i >= 0; i--) {
    x[i] = b[i];
    for (k = i+1; k < *p; k++) {
      x[i] -= x[k]*M[i + k*(k+1)/2];
    }
    x[i] /= M[i*(i+3)/2];
  }

}

/** Forwardsolve - from lower triangular matrix (which is stored as upper) **/

void forwardsolve2(double* M, // IN  [p*(p+1)/2] upper trianglar matrix (by columns)
                   double* b, // IN  [p] right-hand side of the equation
                   int*    p, // IN  [1] dimension of the matrix
                   double* x) // OUT [p] the solution
{
  int i, k; // looping indeces
  
  for (i = 0; i < *p; i++) {
    x[i] = b[i];
    for (k = 0; k < i; k++) {
      x[i] -= x[k]*M[k + i*(i+1)/2];
    }
    x[i] /= M[i*(i+3)/2];
  }
  
}

void detchol(double* chol, // IN  [p * (p+1)/2] uppertriangluar matrix from cholesky decomposition
             double* det,  // OUT [1] determinant of original matrix
             int* p        // IN  [1] dimension of matrix
             )
{
  int i; // looping index
  *det = 1;
  for(i = 0; i < *p; i++){
    *det *= chol[i*(i+3)/2];
  }
  *det *= *det; // squared
}

