#include "mextriang.h"
void subscalarmul(double *x, const double alpha, const double *y, const int n)
{ int i;

  for(i=0; i< n-7; i++){          /* LEVEL 8 */
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i];
  }
  if(i < n-3){                    /* LEVEL 4 */
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
  }
  if(i < n-1){                    /* LEVEL 2 */
    x[i] -= alpha * y[i]; i++;
    x[i] -= alpha * y[i]; i++;
  }
  if(i < n)                       /* LEVEL 1 */
    x[i] -= alpha * y[i];
}
