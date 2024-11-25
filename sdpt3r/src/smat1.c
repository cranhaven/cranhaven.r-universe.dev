#include "smat.h"
void smat1(int n, const double ir2, 
           const double *A, const int *irA, const int *jcA, int isspA, 
           int mA, int colidx, 
           double *B, int *irB, int *jcB, int isspB)

{  int idx, i, j, r, jn, k, kstart, kend, idxj, j2, count;
  double tmp;  
  double hf=0.5; 
  
  i=0;
  if (!isspA & !isspB) { 
    idx = colidx*mA; 
    for (j=0; j<n; j++) { 
      jn = j*n; 
      for (i=0; i<j; i++) { 
        B[i+jn] = ir2*A[idx]; 
        idx++; } 
      B[j+jn] = A[idx];
      idx++; 
    }
  } else if (isspA & !isspB) {      
    j2 = 0; idxj = 0; 
    kstart = jcA[colidx];  kend = jcA[colidx+1]; 
    for (k=kstart; k<kend; k++) { 
      r = irA[k];
      for (j=j2; j<n; j++) {i=r-idxj; if (i>j) {idxj+=j+1;} else {break;}} j2=j; 
      if (i < j) { B[i+j*n] = ir2*A[k]; }
      else       { B[i+j*n] = A[k]; }
    }
  } else if ((!isspA) & isspB) { 
    idx = colidx*mA; 
    count = 0; 
    for (j=0; j<n; j++) { 
      for (i=0; i<j; i++) {
        tmp = A[idx];
        if (tmp != 0) { irB[count] = i; B[count] = ir2*tmp; count++; }
        idx++; 
      }     
      tmp = A[idx];
      if (tmp != 0) { irB[count] = j; B[count] = hf*tmp; count++; }
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
      if (i<j) {B[count] = ir2*A[k];}  else {B[count] = hf*A[k];}
      ++jcB[j+1]; 
      count++; 
    }   
    for (j=0; j<n; j++) { jcB[j+1] += jcB[j]; }
  }
  if (!isspB) { sym(B,n); }
  return; 
}
