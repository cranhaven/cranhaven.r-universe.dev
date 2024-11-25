#include "svec.h"
void svec1(int n, double r2, 
           double *A, int *irA, int *jcA, int isspA, 
           double *B, int *irB, int *jcB, int isspB) 

{  int idx, i, j, jn, k, kstart, kend, idxj; 
  
  if (!isspB & !isspA) { 
    idx = 0; 
    for (j=0; j<n; j++) { 
      jn = j*n; 
      for (i=0; i<j; i++) { 
        B[idx] = A[i+jn]*r2; 
        idx++; } 
      B[idx] = A[j+jn]; 
      idx++; 
    }
  } else if (isspB & !isspA) { 
    idx = 0; 
    idxj = 0; 
    for (j=0; j<n; j++) { 
      jn = j*n; 
      idxj += j; 
      for (i=0; i<j; i++) { 
        irB[idx] = i+idxj;               
        B[idx]   = A[i+jn]*r2; 
        idx++; } 
      irB[idx] = j+idxj; 
      B[idx]   = A[j+jn]; 
      idx++; 
    }  
    jcB[1] = idx;  
  } else if ((!isspB) & isspA) {
    idx = 0; 
    for (j=0; j<n; j++) { 
      idx += j; 
      kstart = jcA[j]; kend = jcA[j+1]; 
      if (kstart < kend) { 
        for (k=kstart; k<kend; k++) { 
          i = irA[k]; 
          if (i >= j) { break; } 
          B[idx+i] = A[k]*r2; 
        }
        if (i == j) {  B[idx+i] = A[k]; }        
      }
    }
  } else if (isspB & isspA) {
    idx = 0; 
    idxj = 0; 
    for (j=0; j<n; j++) { 
      idxj += j; 
      kstart = jcA[j];  kend = jcA[j+1];
      if (kstart < kend) { 
        for (k=kstart; k<kend; k++) { 
          i = irA[k]; 
          if (i >= j) {  break;  } 
          irB[idx] = i+idxj;              
          B[idx]   = A[k]*r2; 
          idx++; } 
        if ( i == j) { 
          irB[idx] = j+idxj; 
          B[idx]   = A[k]; 
          idx++;  }
      }  
    }
    jcB[1] = idx;  
  }
  return;
}
