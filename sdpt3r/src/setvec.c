#include "schurmat.h"
void setvec(int n, double *x, double alpha)

{ int k;
  for (k=0; k<=n; ++k) { x[k] = alpha; }
  return; 
}
