#include "schurmat.h"
void schurij3(int n,  double *Avec, 
              int *idxstart, int *nzlistAi, int *nzlistAj,
              double *U, double *V, int col, double *schurcol)

{ int    ra, ca, rb, cb, rbn, cbn, l, k, idx1, idx2, idx3, idx4;
  int    i, kstart, kend, lstart, lend; 
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
          idx1 = ra+rbn; idx2 = ca+cbn;
          if (ra<ca) { 
            idx3 = ra+cbn; idx4 = ca+rbn; 
            tmp3 += Avec[k] *(U[idx1]*V[idx2]+U[idx2]*V[idx1] \
                              +U[idx3]*V[idx4]+U[idx4]*V[idx3]);  }
          else {
            tmp4 += Avec[k] * (U[idx1]*V[idx2]+U[idx2]*V[idx1]);  }
        }
        if (rb<cb) { tmp1 += Avec[l]*(ir2*tmp3+tmp4); }
        else       { tmp2 += Avec[l]*(ir2*tmp3+tmp4); } 
      }
      schurcol[i] = ir2*tmp1+tmp2/2; 
    }
  }
  return; 
}
