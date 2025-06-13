#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>
/************************
 * ArrayIndex
 ***********************/

SEXP ArrayIndex_C(SEXP Rdims, SEXP Rxrange, SEXP Ryrange, SEXP Rzrange)
{
  /* Digest the datastructures (SEXPs) from R */
  int *dims, *xrange, *yrange, *zrange, xlen, ylen, zlen, len;
  dims = INTEGER(coerceVector(Rdims, INTSXP));
  xrange = INTEGER(coerceVector(Rxrange, INTSXP));
  yrange = INTEGER(coerceVector(Ryrange, INTSXP));
  zrange = INTEGER(coerceVector(Rzrange, INTSXP));
  xlen = length(Rxrange);
  ylen = length(Ryrange);
  zlen = length(Rzrange);
  len = xlen*ylen*zlen;

  /* main */
  int i, j, k, count, *indexlist;
  SEXP Rindexlist;
  PROTECT(Rindexlist = allocVector(INTSXP, len));
  indexlist = INTEGER(Rindexlist);

  count = 0;
  for (k = 0; k < zlen; ++k)
  {
    for (j = 0; j < ylen; ++j)
    {
      for (i = 0; i < xlen; ++i)
      {
        indexlist[count] = (zrange[k]-1)*dims[0]*dims[1] + (yrange[j]-1)*dims[0] + xrange[i];
        count += 1;
      }
    }
  }

  UNPROTECT(1);
  return Rindexlist;
}


void R_init_dmri(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
