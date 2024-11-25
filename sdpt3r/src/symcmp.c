#include "smat.h"
void symcmp(double *Q, double *QI, int n)
{
  int j, k, jn, kn; 
  
  for (j=0; j<n; ++j){
    jn = j*n; 
    for (k=0; k<j ; ++k){ 
      kn = k*n;
      Q[j+kn]  =  Q[k+jn]; 
      QI[j+kn] = -QI[k+jn]; 
    }
  }
  return;
}
