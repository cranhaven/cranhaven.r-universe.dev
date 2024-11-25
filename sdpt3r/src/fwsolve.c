#include "mextriang.h"
void fwsolve(double *y,const double *u, const double *x, const int n)
{
  int k;

  /*-----------------------------------------------
    u(1:k-1,k)'*y(1:k-1) + u(k,k)*y(k) = x(k).
   -----------------------------------------------*/
   for (k = 0; k < n; k++, u += n)
      y[k] = (x[k] - realdot(y,u,k)) / u[k];
}
