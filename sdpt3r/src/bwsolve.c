#include "mextriang.h"
void bwsolve(double *x,const double *u,const int n)
{
   int j;

  /*---------------------------------------------
    xnew[j] = x[j] / u[j,j]
    Then, we update the right-hand side:
    xnew(0:j-1) = x(0:j-1) - xnew[j] * u(0:j-1)
   ---------------------------------------------*/
   j = n;
   u += SQR(n);
   while(j > 0){
     --j;
     u -= n;
     x[j] /= u[j];
     subscalarmul(x,x[j],u,j);
   }
}
