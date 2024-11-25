#include "smat.h"
void smat1cmp(int n, const double ir2, 
              const double *A, const int *irA, const int *jcA, int isspA, 
              int mA, int colidx, 
              double *B, int *irB, int *jcB, int isspB,
              double *AI, double *BI)

{  int idx, i, j, r, jn, k, kstart, kend, idxj, j2, count, ind;
  double tmp, tmp2;  
  double hf=0.5; 
  
  i=0;
  if (!isspA & !isspB) { 
    idx = colidx*mA; 
    for (j=0; j<n; j++) { 
      jn = j*n; 
      for (i=0; i<j; i++) { 
        B[i+jn]  = ir2*A[idx]; 
        BI[i+jn] = ir2*AI[idx]; 
        idx++; } 
      B[j+jn]  = A[idx];
      BI[j+jn] = AI[idx];
      idx++; 
    }
  } else if (isspA & !isspB) {      
    j2 = 0; idxj = 0; 
    kstart = jcA[colidx];  kend = jcA[colidx+1]; 
    for (k=kstart; k<kend; k++) { 
      r = irA[k];
      for (j=j2; j<n; j++) {i=r-idxj; if (i>j) {idxj+=j+1;} else {break;}} j2=j; 
      if (i < j) { ind = i+j*n; B[ind] = ir2*A[k]; BI[ind] = ir2*AI[k]; }
      else       { ind = i+j*n; B[ind] = A[k];     BI[ind] = AI[k];}
    }
  } else if ((!isspA) & isspB) { 
    idx = colidx*mA; 
    count = 0; 
    for (j=0; j<n; j++) { 
      for (i=0; i<j; i++) {
        tmp = A[idx]; tmp2 = AI[idx]; 
        if ((tmp != 0) || (tmp2 != 0)) { 
          irB[count] = i; 
          B[count] = ir2*tmp; BI[count] = ir2*tmp2; 
          count++; 
        }
        idx++; 
      }     
      tmp = A[idx]; tmp2 = AI[idx]; 
      if ((tmp != 0) || (tmp2 != 0)) { 
        irB[count] = j; B[count] = hf*tmp; BI[count] = hf*tmp2; 
        count++; 
      }
      idx++; 
      jcB[j+1] = count; 
    }   
  } else if (isspA & isspB) { 
    count = 0; 
    j2 = 0; idxj = 0; 
    kstart = jcA[colidx];  kend = jcA[colidx+1]; 
    for (k=kstart; k<kend; k++) { 
      r = irA[k];
      for (j=j2; j<n; j++) {i=r-idxj; if (i>j) {idxj+=j+1;} else {break;}} j2=j; 
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
  if (!isspB) { symcmp(B,BI,n); }
  return; 
}
