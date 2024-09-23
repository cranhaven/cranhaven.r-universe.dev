/*******************************************************************************
Vector operations
*******************************************************************************/

#include "trenchR.h"

// Computation of dot product.
// Arguments: u,v vectors of size n x 1.
// Returns t.
// equals to sum(u(i) * v(i)) from i = 0 to i = n - 1.
double dot(int n,double* u,double* v)
{
   double t = 0.0;
   int i;

   for (i = 0; i < n; i++)
      t += u[i] * v[i];

   return t;
}

// Computation of convolution product.
// Arguments: u,v vectors of size n x 1.
// Returns t.
// equals to sum(u(n-1-i) * v(i)) from i = 0 to i = n - 1.
double flipupdot(int n,double* u,double* v)
{
   double t = 0.0;
   int i;

   for (i = 0; i < n; i++)
      t += u[n - 1 - i] * v[i];

   return t;
}

// Computation of sum of the elements of a vector.
// Arguments: u vector of size n x 1.
// Returns t.
// equals to sum(u(i)) from i = 0 to i = n - 1.
double sum(int n,double* u)
{
   double t = 0.0;
   int i;

   for (i = 0; i < n; i++)
      t += u[i];

   return t;
}

// Computation of sum of the elements of an integer vector.
// Arguments: u vector of size n x 1.
// Returns t.
// equals to sum(u(i)) from i = 0 to i = n - 1.
int sumint(int n,int* u)
{
   int t = 0;
   int i;

   for (i = 0; i < n; i++)
      t += u[i];

   return t;
}

// The signum function of a real number u.
// Arguments: u real number.
int sign(double u)
{
    if (u > 0) return 1;
    if (u < 0) return -1;
    return 0;
}
