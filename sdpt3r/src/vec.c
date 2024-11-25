#include "schurmat.h"
void vec(int numblk, int *cumblksize, int *blknnz, 
         double *A, int *irA, int *jcA, double *B) 

{  int idx0, idx, i, j, l, jstart, jend, istart, blksize;
  int k, kstart, kend; 
  
  for (l=0; l<numblk; l++) { 
    jstart = cumblksize[l]; 
    jend   = cumblksize[l+1];  
    blksize = jend-jstart; 
    istart = jstart;
    idx0 = blknnz[l]; 
    for (j=jstart; j<jend; j++) { 
      idx = idx0 + (j-jstart)*blksize; 
      kstart = jcA[j]; kend = jcA[j+1]; 
      for (k=kstart; k<kend; k++) { 
        i = irA[k];
        B[idx+i-istart] = A[k]; }
    }
  }  
  return;
}
