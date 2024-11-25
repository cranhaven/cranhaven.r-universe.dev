#include "schurmat.h"
void schurij2Wrapper(double *Avec, 
              int *idxstart, int *nzlistAi, int *nzlistAj, 
              double *Utmp,
              int *nzlistAr, int *nzlistAc, int *cumblksize, 
              int *blkidx, int *col, double *schurcol){
  schurij2(Avec, idxstart, nzlistAi, nzlistAj, Utmp, nzlistAr, nzlistAc, cumblksize, blkidx, col[0], schurcol);
}
