#include "colamd.h"
void colamdWrapper(int* n_row, int* n_col, int* ALEN, int* A, int* p, int* p_out, int* dummy) {
  int stats [COLAMD_STATS];
  dummy[0] = 1;
  dummy[0] = colamd(n_row[0], n_col[0], ALEN[0],A,p,(double *) NULL, stats);
}
