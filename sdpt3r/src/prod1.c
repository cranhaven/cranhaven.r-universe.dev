#include "mexprod.h"
void  prod1(int m, int n, int p, 
            double *A,  int *irA, int *jcA, int isspA, 
            double *B,  double *P,  int *irP, int *jcP, 
            int *list1, int *list2, int len)

{ int  j, k, r, t, rn, jn, jold, kstart, kend, idx, count;
  double  tmp;  
  
  jold = -1; count = 0; jn = 0;
  for (t=0; t<len; ++t) {
    r = list1[t];
    j = list2[t];  
    if (j != jold) {
      jn = j*n;
      jold = j; }       
    if (!isspA) {
      rn = r*n;
      tmp = realdot2(A,rn,B,jn,n);  }
    else {
      tmp = 0;  
      kstart = jcA[r];   
      kend   = jcA[r+1]; 
      for (k=kstart; k<kend; ++k) {
        idx = irA[k]; 
        tmp += A[k]*B[idx+jn]; } 
    }
    P[count] = tmp; 
    irP[count] = r;  jcP[j+1]++;  count++; 
  }
  for (k=0; k<p; k++) { jcP[k+1] += jcP[k]; } 
  return;
}
