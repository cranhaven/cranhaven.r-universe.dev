#include "schurmat.h"
void schurij1Wrapper(int *n, double *Avec, int *idxstart, int *nzlistAi, int *nzlistAj, double *U, int *col, double *schurcol){
  schurij1(n[0], Avec, idxstart, nzlistAi, nzlistAj, U, col[0], schurcol);
}
