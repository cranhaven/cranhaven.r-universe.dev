#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

void F77_NAME(gbsonedhom_f)(double *p, int *Ytmat, int *Zmat, int *N, double *SeSp, int *Ycol, int *Zrow, int *Zcol, double *U, int *GI, int *a, int *ret);

extern SEXP gbsonedhom_c(SEXP p, SEXP Ytmat, SEXP Zmat, SEXP N, SEXP SeSp, SEXP Ycol, SEXP Zrow, SEXP Zcol, SEXP U, SEXP GI, SEXP a){
  SEXP ret;
  PROTECT(ret = allocVector(INTSXP, asInteger(N)));
  F77_CALL(gbsonedhom_f)(REAL(p),INTEGER(Ytmat),INTEGER(Zmat),INTEGER(N),REAL(SeSp),INTEGER(Ycol),INTEGER(Zrow),INTEGER(Zcol),REAL(U),INTEGER(GI),INTEGER(a),INTEGER(ret));
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(cvondknachom_f)(double *p, int *Ytmat, int *Zmat, int *N, double *SeSp, int *Ycol, int *Zrow, int *Zcol, double *U, int *GI, int *a, double *ret);

extern SEXP cvondknachom_c(SEXP p, SEXP Ytmat, SEXP Zmat, SEXP N, SEXP SeSp, SEXP Ycol, SEXP Zrow, SEXP Zcol, SEXP U, SEXP GI, SEXP a){
  SEXP ret;
  PROTECT(ret = allocVector(REALSXP, 1));
  F77_CALL(cvondknachom_f)(REAL(p),INTEGER(Ytmat),INTEGER(Zmat),INTEGER(N),REAL(SeSp),INTEGER(Ycol),INTEGER(Zrow),INTEGER(Zcol),REAL(U),INTEGER(GI),INTEGER(a),REAL(ret));
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(gbsonedsreg_f)(double *p, int *Ytmat, int *Zmat, int *N, double *SeSp, int *Ycol, int *Zrow, int *Zcol, double *U, int *GI, int *a, int *ret);

extern SEXP gbsonedsreg_c(SEXP p, SEXP Ytmat, SEXP Zmat, SEXP N, SEXP SeSp, SEXP Ycol, SEXP Zrow, SEXP Zcol, SEXP U, SEXP GI, SEXP a){
  SEXP ret;
  PROTECT(ret = allocVector(INTSXP, asInteger(N)));
  F77_CALL(gbsonedsreg_f)(REAL(p),INTEGER(Ytmat),INTEGER(Zmat),INTEGER(N),REAL(SeSp),INTEGER(Ycol),INTEGER(Zrow),INTEGER(Zcol),REAL(U),INTEGER(GI),INTEGER(a),INTEGER(ret));
  UNPROTECT(1);
  return(ret);
}

void F77_NAME(cvondknacreg_f)(double *dg, double *d2g, int *blen, double *p, double *SeSp, int *Ytmat, int *Zmat, double *X, int *N, int *Ycol, int *Zrow, int *Zcol, double *U, int *GI, int *a, double *ret);

extern SEXP cvondknacreg_c(SEXP dg, SEXP d2g, SEXP blen, SEXP p, SEXP SeSp, SEXP Ytmat, SEXP Zmat, SEXP X, SEXP N, SEXP Ycol, SEXP Zrow, SEXP Zcol, SEXP U, SEXP GI, SEXP a){
  SEXP ret;
  PROTECT(ret = allocMatrix(REALSXP, asInteger(blen), asInteger(blen)));  
  F77_CALL(cvondknacreg_f)(REAL(dg),REAL(d2g),INTEGER(blen),REAL(p),REAL(SeSp),INTEGER(Ytmat),INTEGER(Zmat),REAL(X),INTEGER(N),INTEGER(Ycol),INTEGER(Zrow),INTEGER(Zcol),REAL(U),INTEGER(GI),INTEGER(a),REAL(ret));
  UNPROTECT(1);
  return(ret);
}

static const R_CallMethodDef CallEntries[] = {
  {"gbsonedhom_c",   (DL_FUNC) &gbsonedhom_c,   11},
  {"cvondknachom_c",   (DL_FUNC) &cvondknachom_c,   11},
  {"gbsonedsreg_c",   (DL_FUNC) &gbsonedsreg_c,   11},
  {"cvondknacreg_c",   (DL_FUNC) &cvondknacreg_c,   15},
  {NULL,         NULL,                0}
};

void R_init_groupTesting(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
