#include "mexprod.h"
void  prod2(int m, int n, int p,  
            double *A, int *irA, int *jcA, int isspA,
            double *B, int *irB, int *jcB, int isspB,
            double *P, int *irP, int *jcP, double *Btmp, 
            int *list1, int *list2, int len)  

{ int  j, k, r, t, rn, jold, kstart, kend, idx, count;
  double  tmp;  
  
  jold = -1; count = 0;  
  for (t=0; t<len; ++t) {
    r = list1[t];  
    j = list2[t];  
    if (j != jold) {
      /*jn = j*n;*/  
      /***** copy j-th column of sparse B to Btmp *****/ 
        for (k=0; k<n; ++k) { Btmp[k] = 0; }
      kstart = jcB[j];  kend = jcB[j+1];
      for (k=kstart; k<kend; ++k) { idx = irB[k]; Btmp[idx] = B[k];  }
      jold = j; 
    }
    if (!isspA) {
      rn = r*n; 
      tmp = realdot2(A,rn,Btmp,0,n); }
    else {
      tmp = 0;  
      kstart = jcA[r];   kend = jcA[r+1]; 
      for (k=kstart; k<kend; ++k) {
        idx = irA[k]; 
        tmp += A[k]*Btmp[idx]; } 
    }
    P[count] = tmp; 
    irP[count] = r;  jcP[j+1]++;   count++; 
  }
  for (k=0; k<p; k++) { jcP[k+1] += jcP[k]; } 
  return;
}
