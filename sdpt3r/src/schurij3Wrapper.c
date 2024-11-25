#include "schurmat.h"
void schurij3Wrapper(int *n,  double *Avec, 
              int *idxstart, int *nzlistAi, int *nzlistAj,
              double *U, double *V, int *col, double *schurcol){
  schurij3(n[0], Avec, idxstart, nzlistAi, nzlistAj, U, V, col[0], schurcol);
}
