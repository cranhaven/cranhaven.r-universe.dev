#include <Rcpp.h>
#include "expokit.h"

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
