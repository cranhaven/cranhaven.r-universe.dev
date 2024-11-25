#include "schurmat.h"
void schurij4( double *Avec, 
               int *idxstart, int *nzlistAi, int *nzlistAj,
               double *Utmp, double *Vtmp, 
               int *nzlistAr, int *nzlistAc, int *cumblksize, 
               int *blkidx, int col, double *schurcol)

{ int    ra, ca, rb, cb, l, k, kstart, kend, kstartnew, lstart, lend;
  int    idxrb, idxcb, idx1, idx2, idx3, idx4; 
  int    i, cblk, calk, firstime;
  double tmp0, tmp1, tmp2, tmp3, tmp4;
  double hlf=0.5;  
  
  lstart = idxstart[col]; lend = idxstart[col+1];
  
  for (i=0; i<=col; i++) {
    if (schurcol[i] != 0) {
      kstart = idxstart[i]; kend = idxstart[i+1]; 
      kstartnew = kstart;
      tmp1 = 0; tmp2 = 0;  
      for (l=lstart; l<lend; ++l) { 
        rb = nzlistAi[l];    
        cb = nzlistAj[l];
        cblk = blkidx[cb];  
        idxcb = nzlistAc[l]; 
        idxrb = nzlistAr[l];
        tmp3 = 0; tmp4 = 0; firstime = 1; 
        for (k=kstart; k<kend; ++k) { 
          ca = nzlistAj[k];
          calk = blkidx[ca]; 
          if (calk == cblk) { 
            ra = nzlistAi[k];
            idx1 = ra+idxrb; idx2 = ca+idxcb; 
            if (ra<ca) {
              idx3 = ra+idxcb; idx4 = ca+idxrb; 
              tmp3 += Avec[k] * (Utmp[idx1]*Vtmp[idx2] +Utmp[idx2]*Vtmp[idx1] \
                                 +Utmp[idx3]*Vtmp[idx4] +Utmp[idx4]*Vtmp[idx3]); 
            } else {
              tmp4 += Avec[k] * (Utmp[idx1]*Vtmp[idx2] +Utmp[idx2]*Vtmp[idx1]); 
            }
            if (firstime) { kstartnew = k; firstime = 0; }  
          }
          else if (calk > cblk) {
            break;
          }
        }
        kstart = kstartnew; 
        if (rb<cb) { tmp1 += Avec[l]*(ir2*tmp3 + tmp4); }
        else       { tmp2 += Avec[l]*(ir2*tmp3 + tmp4); } 
      }
      tmp0 = ir2*tmp1+hlf*tmp2;
      schurcol[i] = tmp0;
    }
  }
  return; 
}
