#include "svec.h"
void svec2cmp(int n, int numblk, int *cumblksize, int *blknnz, 
              double r2, 
              double *A, int *irA, int *jcA, int isspA, 
              double *B, int *irB, int *jcB, int isspB,
              double *AI, double *BI) 

{  int idx, i, j, jn, l, jstart, jend, istart;
  int rowidx, idxj, k, kstart, kend; 
  
  if (!isspB & !isspA) { 
    idx = 0; 
    jstart = 0; jend = 0; 
    for (l=0; l<numblk; l++) { 
      jend = cumblksize[l+1];   
      istart = jstart; 
      for (j=jstart; j<jend; j++) { 
        jn = j*n; 
        for (i=istart; i<j; i++) { 
          B[idx]  = A[i+jn]*r2; 
          BI[idx] = AI[i+jn]*r2; 
          idx++; } 
        B[idx]  = A[j+jn]; 
        BI[idx] = AI[j+jn]; 
        idx++;    }
      jstart = jend; 
    }
  } else if (isspB & !isspA) { 
    idx = 0; 
    jstart = 0; jend = 0; 
    for (l=0; l<numblk; l++) { 
      jend = cumblksize[l+1];   
      istart = jstart;  
      idxj = 0; 
      for (j=jstart; j<jend; j++) { 
        jn = j*n; 
        idxj += j-jstart;
        rowidx = blknnz[l]-istart+idxj; 
        for (i=istart; i<j; i++) { 
          irB[idx] = rowidx+i;               
          B[idx]   = A[i+jn]*r2; 
          BI[idx]  = AI[i+jn]*r2; 
          idx++; } 
        irB[idx] = rowidx+j; 
        B[idx]   = A[j+jn]; 
        BI[idx]  = AI[j+jn]; 
        idx++; 
      }
      jstart = jend; 
    }  
    jcB[1] = idx;  
  } else if ((!isspB) & isspA) { 
    jstart = 0; jend = 0; 
    for (l=0; l<numblk; l++) { 
      jend = cumblksize[l+1];  
      istart = jstart;
      idx = blknnz[l]; 
      for (j=jstart; j<jend; j++) { 
        idx += j-jstart; 
        kstart = jcA[j]; kend = jcA[j+1]; 
        if (kstart < kend) { 
          for (k=kstart; k<kend; k++) { 
            i = irA[k]; 
            if (i >= j) { break; } 
            B[idx+i-istart]  = A[k]*r2; 
            BI[idx+i-istart] = AI[k]*r2; 
          }
          if (i == j) {  B[idx+i-istart] = A[k]; BI[idx+i-istart] = AI[k]; } 
        }
      }
      jstart = jend; 
    }  
  } else if (isspB & isspA) {
    idx = 0; 
    jstart = 0; jend = 0; 
    for (l=0; l<numblk; l++) { 
      jend = cumblksize[l+1];  
      istart = jstart;
      idxj = 0; 
      for (j=jstart; j<jend; j++) { 
        idxj += j-jstart;
        rowidx = blknnz[l]-istart+idxj;
        kstart = jcA[j];  kend = jcA[j+1];
        if (kstart < kend) { 
          for (k=kstart; k<kend; k++) { 
            i = irA[k]; 
            if (i >= j) {  break;  } 
            irB[idx] = rowidx+i;              
            B[idx]   = A[k]*r2; 
            BI[idx]  = AI[k]*r2; 
            idx++; } 
          if (i == j) { 
            irB[idx] = rowidx+j; 
            B[idx]   = A[k]; 
            BI[idx]  = AI[k]; 
            idx++;  }
        }  
      }
      jstart = jend; 
    }
    jcB[1] = idx;  
  } 
  return;
}
