#include "schurmat.h"
void schurij4Wrapper(double *Avec, 
               int *idxstart, int *nzlistAi, int *nzlistAj,
               double *Utmp, double *Vtmp, 
               int *nzlistAr, int *nzlistAc, int *cumblksize, 
               int *blkidx, int *col, double *schurcol){
  schurij4(Avec, idxstart, nzlistAi, nzlistAj, Utmp, Vtmp, nzlistAr, nzlistAc, cumblksize, blkidx, col[0], schurcol);
}
