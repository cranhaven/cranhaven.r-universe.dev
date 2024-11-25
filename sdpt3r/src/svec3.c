#include "svec.h"
void svec3(int n, double r2, 
           double *A, int *irA, int *jcA, int isspA, 
           double *B, int *irB, int *jcB, int isspB) 

{  int idx, rowidx, i, j, k, kstart, kend; 
  
  if (!isspB & !isspA) { 
    idx = 0; 
    for (i=0; i<n; i++) { 
      for (j=0; j<i; j++) { 
        B[idx] = A[i+j*n]*r2; 
        idx++; } 
      B[idx] = A[j+j*n]; 
      idx++; 
    }
  } else if (isspB & !isspA) { 
    idx = 0; 
    rowidx = 0; 
    for (i=0; i<n; i++) { 
      rowidx += i; 
      for (j=0; j<i; j++) { 
        irB[idx] = j+rowidx;               
        B[idx]   = A[i+j*n]*r2; 
        idx++; } 
      irB[idx] = j+rowidx; 
      B[idx]   = A[j+j*n]; 
      idx++; 
    }  
    jcB[1] = idx;  
  } else if ((!isspB) & isspA) {
    for (j=0; j<n; j++) { 
      kstart = jcA[j]; kend = jcA[j+1]; 
      for (k=kstart; k<kend; k++) { 
        i = irA[k]; 
        if (j < i) { 
          idx = i*(i+1)/2; 
          B[j+idx] = A[k]*r2; }
        else if (j==i) {
          idx = i*(i+1)/2; 
          B[j+idx] = A[k]; }        
      }
    }
  } else if (isspB & isspA) {
    idx = 0; 
    for (j=0; j<n; j++) { 
      kstart = jcA[j];  kend = jcA[j+1];
      for (k=kstart; k<kend; k++) { 
        i = irA[k]; 
        if (j < i) {                  
          irB[idx] = j + i*(i+1)/2;              
          B[idx] = A[k]*r2;   
          idx++; }
        else if (j==i) {
          irB[idx] = j + i*(i+1)/2; 
          B[idx] = A[k]; 
          idx++;  
        }
      } 
    }
    jcB[1] = idx;  
  }
  return;
}
