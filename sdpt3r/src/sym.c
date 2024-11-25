#include "smat.h"
void sym(double *Q, int n)
{
  int j, k, jn, kn; 
  
  for (j=0; j<n; ++j){
    jn = j*n; 
    for (k=0; k<j ; ++k){ 
      kn = k*n;
      Q[j+kn] = Q[k+jn]; }
  }
  return;
}
