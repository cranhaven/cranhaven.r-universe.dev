#include "mextriang.h"
double realdot(const double *x, const double *y, const int n)
 {
   int i;
   double r;

   r=0.0;
   for(r=0.0, i=0; i< n-7; i++){          /* LEVEL 8 */
      r+= x[i] * y[i]; i++;
      r+= x[i] * y[i]; i++;
      r+= x[i] * y[i]; i++;
      r+= x[i] * y[i]; i++;
      r+= x[i] * y[i]; i++;
      r+= x[i] * y[i]; i++;
      r+= x[i] * y[i]; i++;
      r+= x[i] * y[i];
   }
   if(i < n-3){                           /* LEVEL 4 */
     r+= x[i] * y[i]; i++;
     r+= x[i] * y[i]; i++;
     r+= x[i] * y[i]; i++;
     r+= x[i] * y[i]; i++;
   }
   if(i < n-1){                           /* LEVEL 2 */
     r+= x[i] * y[i]; i++;
     r+= x[i] * y[i]; i++;
   }
   if(i < n)                              /* LEVEL 1 */
     r+= x[i] * y[i];
   return r;
}
