#include "schurmat.h"
void schurij1(int n, double *Avec, 
              int *idxstart, int *nzlistAi, int *nzlistAj,
              double *U, int col, double *schurcol)

{ int    i, ra, ca, rb, cb, rbn, cbn, l, k, kstart, kend, lstart, lend; 
  double tmp1, tmp2, tmp3, tmp4; 
  
  lstart = idxstart[col]; lend = idxstart[col+1]; 
  
  for (i=0; i<=col; i++) {
    if (schurcol[i] != 0) {
      kstart = idxstart[i]; kend = idxstart[i+1]; 
      tmp1 = 0; tmp2 = 0;  
      for (l=lstart; l<lend; ++l) { 
        rb = nzlistAi[l];    
        cb = nzlistAj[l];
        rbn = rb*n; cbn = cb*n;   
        tmp3 = 0; tmp4 = 0;
        for (k=kstart; k<kend; ++k) { 
          ra = nzlistAi[k];
          ca = nzlistAj[k];             
          if (ra<ca) {  
            tmp3 += Avec[k] * (U[ra+rbn]*U[ca+cbn]+U[ra+cbn]*U[ca+rbn]); }
          else { 
            tmp4 += Avec[k] * (U[ra+rbn]*U[ca+cbn]); }
        }
        if (rb<cb) { tmp1 += Avec[l]*(ir2*tmp3 + tmp4); }
        else       { tmp2 += Avec[l]*(ir2*tmp3 + tmp4); } 
      }
      schurcol[i] = r2*tmp1+tmp2; 
    }
  }
  return;
}
