/*
*
*  wrapper functions for Fortran.
*
*/

#include <R.h>
#include "pnorm.h"

double F77_SUB(mvphi)(double const *z){
  return pnorm_std(*z, 1L, 0L);
}
