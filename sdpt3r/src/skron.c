#include "skron.h"
void skron(int n, int maxblksize, double *P, double *Q,
           double *x1, double *y1, double *x2, double *y2,
           int r, int c, double *vvtmp)

{  int idx, i, j, k, rn, cn;
  double tmp, tmp1, tmp2, tmp3, tmp4; 
  double hf=0.5;
  
  rn = r*maxblksize; cn = c*maxblksize;
  for (k=0; k<n; k++) {
    x1[k] = P[k+rn]; y1[k] = Q[k+cn];  
    x2[k] = P[k+cn]; y2[k] = Q[k+rn];
  } 
  if (r < c) {
    idx = 0;
    for (j=0; j<n; j++) {
      tmp1 = hf*y1[j]; tmp2 = hf*y2[j]; 
      tmp3 = hf*x1[j]; tmp4 = hf*x2[j];   
      for (i=0; i<j; i++) {
        vvtmp[idx] = tmp1*x1[i]+tmp2*x2[i]+tmp3*y1[i]+tmp4*y2[i];
        idx++; }
      tmp = tmp1*x1[j]+tmp2*x2[j]+tmp3*y1[j]+tmp4*y2[j];
      vvtmp[idx] = tmp*ir2; 
      idx++;
    }
  } else {   
    /*** r = c ***/
      idx = 0;
      for (j=0; j<n; j++) {
        tmp1 = ir2*y1[j]; tmp2 = ir2*x1[j]; 
        for (i=0; i<j; i++) {
          vvtmp[idx] = tmp1*x1[i]+tmp2*y1[i];
          idx++; }
        vvtmp[idx] = y1[j]*x1[j];
        idx++;
      }
  }
  return;
}
