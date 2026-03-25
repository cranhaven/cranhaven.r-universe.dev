#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <Rcpp.h>
#include "expokit.h"


/*

NOTE (2017-08-10, NJM)

This init.c file (and first 3 lines above) was built by 
package_native_routine_registration_skeleton().

It was a change requested by Kurt Hornik in rexpokit updates:

Registering the C++ and FORTRAN calls:
https://stat.ethz.ch/pipermail/r-devel/2017-February/073755.html

library(tools)
package_native_routine_registration_skeleton(dir="/GitHub/rexpokit")

*/

/*
  The following symbols/expressions for .NAME have been omitted

    R_dgpadm
    R_dgexpv
    R_dmexpv
    R_mydmexpv
    R_mydgexpv
    R_rexpokit_as_coo

  Most likely possible values need to be added below.
*/

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* END END END init.c added by package_native_routine_registration_skeleton() */ 


/* .Fortran calls */
//extern void F77_NAME(itscale5)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"wrapalldgexpv_", (DL_FUNC) &wrapalldgexpv_, 18},
    {"itscale5_", (DL_FUNC) &itscale5_, 10},
    {NULL, NULL, 0}
};

/*
static const R_FortranMethodDef FortranEntries[] = {
    {"itscale5", (DL_FUNC) &F77_NAME(itscale5), 10},
    {NULL, NULL, 0}
};
*/

void R_init_rexpokit(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    //R_registerRoutines(dll, CEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}




extern "C"
{

// Computes matrix exponential for dense input H
SEXP R_dgpadm(SEXP ideg, SEXP m_, SEXP t, SEXP H, SEXP ldh)
{
  int m = INTEGER(m_)[0];
  int ns = 0, iflag = 0;
  int lwsp = 4*m*m + INTEGER(ideg)[0] + 1;
  
  Rcpp::NumericVector wsp(lwsp);
  Rcpp::IntegerVector ipiv(m);
  
  Rcpp::IntegerVector iexph(1);
  
  Rcpp::List ret; 
  
  wrapdgpadm_(INTEGER(ideg), &m, REAL(t), REAL(H), INTEGER(ldh), REAL(wsp), &lwsp, INTEGER(ipiv), INTEGER(iexph), &ns, &iflag);
  
  //FIXME you should really be checking the iflag return...
  
  ret["wsp"] = wsp; 
  ret["ind"] = iexph;
  
  return ret;
}



SEXP R_dmexpv(SEXP n_, SEXP m, SEXP t, SEXP v, SEXP tol, 
  SEXP anorm, SEXP wsp, SEXP lwsp, SEXP iwsp, SEXP liwsp,
  SEXP ia, SEXP ja, SEXP a, SEXP nz)
{
  int n = INTEGER(n_)[0];
  int iflag = 0, itrace = 0;
  Rcpp::NumericVector res(n*n);
  Rcpp::NumericVector w(n);
  
  Rcpp::List ret; 
  
  PROTECT(lwsp);
  PROTECT(liwsp);
  PROTECT(iwsp);
  PROTECT(tol);
  PROTECT(anorm);
  PROTECT(v);
  PROTECT(wsp);
  
  wrapalldmexpv_(&n, INTEGER(m), REAL(t), REAL(v), REAL(w), 
    REAL(tol), REAL(anorm), REAL(wsp), INTEGER(lwsp), INTEGER(iwsp), 
    INTEGER(liwsp), &itrace, &iflag, INTEGER(ia), 
    INTEGER(ja), REAL(a), INTEGER(nz), REAL(res));
  
  ret["res"] = res; 
  ret["w"] = w;
  
  
  UNPROTECT(7);
  
  return ret;
}



SEXP R_mydmexpv(SEXP n_, SEXP m, SEXP t, SEXP v, SEXP tol, 
  SEXP anorm, SEXP wsp, SEXP lwsp, SEXP iwsp, SEXP liwsp,
  SEXP ia, SEXP ja, SEXP a, SEXP nz)
{
  int n = INTEGER(n_)[0];
  int iflag = 0, itrace = 0;
  Rcpp::NumericVector w(n);
  
  PROTECT(lwsp);
  PROTECT(liwsp);
  PROTECT(iwsp);
  PROTECT(tol);
  PROTECT(anorm);
  PROTECT(v);
  PROTECT(wsp);
  
  mydmexpv_(&n, INTEGER(m), REAL(t), REAL(v), REAL(w), 
    REAL(tol), REAL(anorm), REAL(wsp), INTEGER(lwsp), INTEGER(iwsp), 
    INTEGER(liwsp), &itrace, &iflag, INTEGER(ia), 
    INTEGER(ja), REAL(a), INTEGER(nz));
  
  UNPROTECT(7);
  
  return w;
}



SEXP R_dgexpv(SEXP n_, SEXP m, SEXP t, SEXP v, SEXP tol, 
  SEXP anorm, SEXP wsp, SEXP lwsp, SEXP iwsp, SEXP liwsp,
  SEXP ia, SEXP ja, SEXP a, SEXP nz)
{
  int n = INTEGER(n_)[0];
  int iflag = 0, itrace = 0;
  Rcpp::NumericVector res(n*n);
  Rcpp::NumericVector w(n);
  
  Rcpp::List ret; 
  
  PROTECT(lwsp);
  PROTECT(liwsp);
  PROTECT(iwsp);
  PROTECT(tol);
  PROTECT(anorm);
  PROTECT(v);
  PROTECT(wsp);
  
  
  wrapalldgexpv_(&n, INTEGER(m), REAL(t), REAL(v), REAL(w), 
    REAL(tol), REAL(anorm), REAL(wsp), INTEGER(lwsp), INTEGER(iwsp), 
    INTEGER(liwsp), &itrace, &iflag, INTEGER(ia), 
    INTEGER(ja), REAL(a), INTEGER(nz), REAL(res));
  
  ret["res"] = res; 
  ret["w"] = w;
  
  
  UNPROTECT(7);
  
  return ret;
}



SEXP R_mydgexpv(SEXP n_, SEXP m, SEXP t, SEXP v, SEXP tol, 
  SEXP anorm, SEXP wsp, SEXP lwsp, SEXP iwsp, SEXP liwsp,
  SEXP ia, SEXP ja, SEXP a, SEXP nz)
{
  int n = INTEGER(n_)[0];
  int iflag = 0, itrace = 0;
  Rcpp::NumericVector w(n);
  
  PROTECT(lwsp);
  PROTECT(liwsp);
  PROTECT(iwsp);
  PROTECT(tol);
  PROTECT(anorm);
  PROTECT(v);
  PROTECT(wsp);
  
  
  mydgexpv_(&n, INTEGER(m), REAL(t), REAL(v), REAL(w), 
    REAL(tol), REAL(anorm), REAL(wsp), INTEGER(lwsp), INTEGER(iwsp), 
    INTEGER(liwsp), &itrace, &iflag, INTEGER(ia), 
    INTEGER(ja), REAL(a), INTEGER(nz));
  
  
  UNPROTECT(7);
  
  return w;
}



}
