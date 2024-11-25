#include "svec.h"
void svec4cmp(int n, int numblk, int *cumblksize, int *blknnz, 
              double r2, 
              double *A, int *irA, int *jcA, int isspA, 
              double *B, int *irB, int *jcB, int isspB,
              double *AI, double *BI) 

{  int idx, i, i1, j, l, jstart, jend, istart;
  int rowidx, idx2, idx3, k, kstart, kend; 
  
  i1 = 0;
  if (!isspB) { 
    for (l=0; l<numblk; l++) { 
      jstart = cumblksize[l]; jend = cumblksize[l+1];  
      istart = jstart;
      idx = blknnz[l]; 
      for (j=jstart; j<jend; j++) { 
        kstart = jcA[j]; kend = jcA[j+1]; 
        idx2 = idx + j-jstart; 
        for (k=kstart; k<kend; k++) { 
          i = irA[k]; 
          idx3 = idx2+i1*(i1+1)/2; 
          if (j < i) {
            i1 = i-istart; 
            B[idx3]  = A[k]*r2; 
            BI[idx3] = AI[k]*r2; 
          } else if (j==i) {
            i1 = i-istart; 
            B[idx3]  = A[k]; 
            BI[idx3] = AI[k]; 
          }   
        }
      }
    }  
  } else {
    idx = 0; 
    for (l=0; l<numblk; l++) { 
      jstart = cumblksize[l]; jend = cumblksize[l+1];  
      istart = jstart;
      for (j=jstart; j<jend; j++) {               
        rowidx = blknnz[l] + (j-jstart); 
        kstart = jcA[j];  kend = jcA[j+1];
        for (k=kstart; k<kend; k++) { 
          i = irA[k]; 
          if (j < i) {
            i1 = i-istart; 
            irB[idx] = rowidx + i1*(i1+1)/2;              
            B[idx]   = A[k]*r2; 
            BI[idx]  = AI[k]*r2; 
            idx++; } 
          else if (j==i) { 
            i1 = i-istart; 
            irB[idx] = rowidx + i1*(i1+1)/2; 
            B[idx]   = A[k]; 
            BI[idx]  = AI[k]; 
            idx++; }
        }
      }
    }
    jcB[1] = idx;  
  }
  return;
}
