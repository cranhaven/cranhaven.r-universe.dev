#define HAVE_INLINE
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <math.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/RS.h>	
#include <R_ext/BLAS.h>

double softthresh(double x,double t);

#undef DEBUG

SEXP R_softthresh(SEXP args) {
  args = CDR(args); 
  double x,t;
  int i;
  for(i = 0; args != R_NilValue; i++, args = CDR(args)) {
    if (CAR(args) == R_NilValue) {
      continue;
    } else if (i == 0) {
      x = REAL(CAR(args))[0];
    } else if (i == 1) {
      t = REAL(CAR(args))[0];
    }
  }
#ifdef DEBUG
  printf("softthresh: x: %f, t: %f\n", x,t);
#endif
  if (i != 2) {
    error("softthresh takes exactly two parameters!");
  }
  SEXP resR = PROTECT(NEW_NUMERIC(1));
  double* resp = NUMERIC_POINTER(resR);
  *resp = softthresh(x,t);
#ifdef DEBUG
  printf("softthresh: res: %f\n", *resp);
#endif
  UNPROTECT(1);
  return resR;
}
