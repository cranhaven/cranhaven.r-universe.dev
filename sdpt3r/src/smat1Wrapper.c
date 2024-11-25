#include "smat.h"
void smat1Wrapper(int *n, const double *ir2, 
           const double *A, const int *irA, const int *jcA, int *isspA, 
           int *mA, int *colidx, 
           double *B, int *irB, int *jcB, int *isspB)

{
  smat1(n[0], ir2[0], A, irA, jcA, isspA[0], mA[0], colidx[0], B, irB, jcB, isspB[0]);
}
