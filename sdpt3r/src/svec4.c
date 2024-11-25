#include "svec.h"
void svec4(int n, int numblk, int *cumblksize, int *blknnz, 
           double r2, 
           double *A, int *irA, int *jcA, int isspA, 
           double *B, int *irB, int *jcB, int isspB) 

{  int idx, i, i1, j, l, jstart, jend, istart;
  int rowidx, idx2, k, kstart, kend; 
  
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
          if (j < i) {
            i1 = i-istart; 
            B[idx2+i1*(i1+1)/2] = A[k]*r2; }
          else if (j==i) {
            i1 = i-istart; 
            B[idx2+i1*(i1+1)/2] = A[k]; }        
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
            idx++; } 
          else if (j==i) { 
            i1 = i-istart; 
            irB[idx] = rowidx + i1*(i1+1)/2; 
            B[idx]   = A[k]; 
            idx++; }
        }
      }
    }
    jcB[1] = idx;  
  }
  return;
}
