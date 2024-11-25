#include "smat.h"
void smat2Wrapper(int *n, const int *numblk, const int *cumblksize, const int *blknnz, 
           const double *ir2, 
           const double *A, const int *irA, const int *jcA, int *isspA, 
           int *mA, int *colidx, 
           double *B, int *irB, int *jcB, int *isspB)

{
  smat2(n[0], numblk[0], cumblksize, blknnz, ir2[0], A, irA, jcA, isspA[0],  mA[0], colidx[0], B, irB, jcB, isspB[0]); 
}
