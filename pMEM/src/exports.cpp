/***************************************************************************\
 *
 * (c) 2023-2024 Guillaume Guénard
 *     Department de sciences biologiques,
 *     Université de Montréal
 *     Montreal, QC, Canada
 *
 * Calculations of predictive Moran's Eigenvector Maps (pMEM), which extends
 * classical MEM for making predictions between the sampling points. It also
 * implements 
 *
 \***************************************************************************/

#include <Rcpp.h>
using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// EuclidReal
NumericMatrix EuclidReal(const NumericMatrix& x, const NumericMatrix& y);
RcppExport SEXP pMEM_EuclidReal(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(EuclidReal(x, y));
    return rcpp_result_gen;
END_RCPP
}

// EuclidCplx2D
ComplexMatrix EuclidCplx2D(const NumericMatrix& x, const NumericMatrix& y, double delta, double theta);
RcppExport SEXP pMEM_EuclidCplx2D(SEXP xSEXP, SEXP ySEXP, SEXP deltaSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< const double >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(EuclidCplx2D(x, y, delta, theta));
    return rcpp_result_gen;
END_RCPP
}

// dwfReal
NumericVector dwfReal(const NumericVector& x, const int method, const NumericVector& par);
RcppExport SEXP pMEM_dwfReal(SEXP xSEXP, SEXP methodSEXP, SEXP parSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
  Rcpp::traits::input_parameter< const int >::type method(methodSEXP);
  Rcpp::traits::input_parameter< const NumericVector& >::type par(parSEXP);
  rcpp_result_gen = Rcpp::wrap(dwfReal(x, method, par));
  return rcpp_result_gen;
  END_RCPP
}

// dwfCplx
ComplexVector dwfCplx(const ComplexVector& x, const int method, const NumericVector& par);
RcppExport SEXP pMEM_dwfCplx(SEXP xSEXP, SEXP methodSEXP, SEXP parSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const ComplexVector& >::type x(xSEXP);
  Rcpp::traits::input_parameter< const int >::type method(methodSEXP);
  Rcpp::traits::input_parameter< const NumericVector& >::type par(parSEXP);
  rcpp_result_gen = Rcpp::wrap(dwfCplx(x, method, par));
  return rcpp_result_gen;
  END_RCPP
}

// centerReal
List centerReal(const NumericMatrix& x, const bool dcnt);
RcppExport SEXP pMEM_centerReal(SEXP xSEXP, SEXP dcntSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const NumericMatrix& >::type x(xSEXP);
  Rcpp::traits::input_parameter< const bool >::type dcnt(dcntSEXP);
  rcpp_result_gen = Rcpp::wrap(centerReal(x, dcnt));
  return rcpp_result_gen;
  END_RCPP
}

// centerCplx
List centerCplx(const ComplexMatrix& x, const bool dcnt);
RcppExport SEXP pMEM_centerCplx(SEXP xSEXP, SEXP dcntSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const ComplexMatrix& >::type x(xSEXP);
  Rcpp::traits::input_parameter< const bool >::type dcnt(dcntSEXP);
  rcpp_result_gen = Rcpp::wrap(centerCplx(x, dcnt));
  return rcpp_result_gen;
  END_RCPP
}

// recenterReal
NumericMatrix recenterReal(const NumericMatrix& x, const NumericVector& c, const bool dcnt);
RcppExport SEXP pMEM_recenterReal(SEXP xSEXP, SEXP cSEXP, SEXP dcntSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const NumericMatrix& >::type x(xSEXP);
  Rcpp::traits::input_parameter< const NumericVector& >::type c(cSEXP);
  Rcpp::traits::input_parameter< const bool >::type dcnt(dcntSEXP);
  rcpp_result_gen = Rcpp::wrap(recenterReal(x, c, dcnt));
  return rcpp_result_gen;
  END_RCPP
}

// recenterCplx
ComplexMatrix recenterCplx(const ComplexMatrix& x, const ComplexVector& c, const bool dcnt);
RcppExport SEXP pMEM_recenterCplx(SEXP xSEXP, SEXP cSEXP, SEXP dcntSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const ComplexMatrix& >::type x(xSEXP);
  Rcpp::traits::input_parameter< const ComplexVector& >::type c(cSEXP);
  Rcpp::traits::input_parameter< const bool >::type dcnt(dcntSEXP);
  rcpp_result_gen = Rcpp::wrap(recenterCplx(x, c, dcnt));
  return rcpp_result_gen;
  END_RCPP
}

// getMinMSE_Cpp
List getMinMSEReal(const NumericMatrix& U, const NumericVector& y, const NumericMatrix& Up, const NumericVector& yy, const bool complete);
RcppExport SEXP pMEM_getMinMSEReal(SEXP USEXP, SEXP ySEXP, SEXP UpSEXP, SEXP yySEXP, SEXP completeSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const NumericMatrix& >::type U(USEXP);
  Rcpp::traits::input_parameter< const NumericVector& >::type y(ySEXP);
  Rcpp::traits::input_parameter< const NumericMatrix& >::type Up(UpSEXP);
  Rcpp::traits::input_parameter< const NumericVector& >::type yy(yySEXP);
  Rcpp::traits::input_parameter< const bool >::type complete(completeSEXP);
  rcpp_result_gen = Rcpp::wrap(getMinMSEReal(U, y, Up, yy, complete));
  return rcpp_result_gen;
  END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"pMEM_EuclidReal", (DL_FUNC) &pMEM_EuclidReal, 2},
    {"pMEM_EuclidCplx2D", (DL_FUNC) &pMEM_EuclidCplx2D, 4},
    {"pMEM_dwfReal", (DL_FUNC) &pMEM_dwfReal, 3},
    {"pMEM_dwfCplx", (DL_FUNC) &pMEM_dwfCplx, 3},
    {"pMEM_centerReal", (DL_FUNC) &pMEM_centerReal, 2},
    {"pMEM_centerCplx", (DL_FUNC) &pMEM_centerCplx, 2},
    {"pMEM_recenterReal", (DL_FUNC) &pMEM_recenterReal, 3},
    {"pMEM_recenterCplx", (DL_FUNC) &pMEM_recenterCplx, 3},
    {"pMEM_getMinMSEReal", (DL_FUNC) &pMEM_getMinMSEReal, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_pMEM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
