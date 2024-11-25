#include "colamd.h"
void symamdWrapper(int* n, int* A, int* p, int* perm, int* dummy) {
  int stats [COLAMD_STATS];
  dummy[0] = 1;
  dummy[0] = symamd(n[0], A, p, perm, (double *) NULL, stats, &calloc, &free);
}
