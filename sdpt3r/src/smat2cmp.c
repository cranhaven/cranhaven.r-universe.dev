#include "smat.h"
void smat2cmp(int n, const int numblk, const int *cumblksize, const int *blknnz, 
              const double ir2, 
              const double *A, const int *irA, const int *jcA, int isspA, 
              int mA, int colidx, 
              double *B, int *irB, int *jcB, int isspB,
              double *AI, double *BI)

{  int  idx, i, j, r, k, kstart, kend, idxj, j2, count;
  int  t, t2, istart, jstart, jend, rowidx; 
  double hf=0.5; 
  
  i=0;
  if (!isspA) { 
    idx = 0; 
    jstart = 0; jend = 0; 
    for (t=0; t<numblk; t++) {   	
      jend = cumblksize[t+1]; 
      istart = jstart;
      idxj = colidx*mA; 
      for (j=jstart; j<jend; j++){
        idxj += j-jstart; 
        rowidx = blknnz[t]-istart+idxj; 
        for (i=istart; i<j; i++) {
          irB[idx] = i;  
          B[idx] = ir2*A[rowidx+i];  BI[idx] = ir2*AI[rowidx+i]; 
          idx++; 
        }                    
        irB[idx] = j;  
        B[idx] = hf*A[rowidx+j];  BI[idx] = hf*AI[rowidx+j]; 
        idx++; 
        jcB[j+1] = idx; 
      } 
      jstart = jend; 
    }
  } else {
    jstart = 0; jend = cumblksize[1]; t2 = 0; 
    kstart = jcA[colidx]; kend = jcA[colidx+1]; 
    count  = 0; j2 = 0; idxj = 0; 
    for (k=kstart; k<kend; k++) { 
      r = irA[k];
      for (t=t2; t<numblk; t++) { if (r-blknnz[t+1]<0) {break;} } 
      if (t > t2) { 
        t2 = t; 
        jstart = cumblksize[t2]; jend = cumblksize[t2+1]; 
        idxj = blknnz[t2];
        j2 = jstart; 
      }
      for (j=j2; j<jend; j++) {i=jstart+r-idxj; if (i>j) {idxj+=j-jstart+1;} else {break;}}
      j2=j; 
      irB[count] = i;
      if (i<j) {
        B[count] = ir2*A[k]; BI[count] = ir2*AI[k];
      }  else {
        B[count] = hf*A[k];  BI[count] = hf*AI[k];
      }
      ++jcB[j+1]; 
      count++; 
    }  
    for (j=0; j<n; j++) { jcB[j+1] += jcB[j]; }
  }
  return; 
}
