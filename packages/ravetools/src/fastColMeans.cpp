#include "fastColMeans.h"

using namespace Rcpp;
// [[Rcpp::interfaces(r, cpp)]]

template <typename T>
SEXP fastColMeans_template(const SEXP& x, const SEXP& col, const SEXP& xDim){
  SEXP re = R_NilValue;
  R_xlen_t nCols, nRows;

  if( xDim == R_NilValue ){
    SEXP xDim_ = PROTECT(Rf_getAttrib(x, R_DimSymbol));
    if(Rf_length(xDim_) != 2){
      re = PROTECT(make_error("C++ `fastColMeans`: x is not a matrix"));
      UNPROTECT(2);
      return re;
    }
    nRows = INTEGER(xDim_)[0];
    nCols = INTEGER(xDim_)[1];
    UNPROTECT(1);
  } else {
    if(Rf_length(xDim) != 2){
      re = PROTECT(make_error("C++ `fastColMeans`: `xDim` is not length of 2"));
      UNPROTECT(1);
      return re;
    }
    nRows = INTEGER(xDim)[0];
    nCols = INTEGER(xDim)[1];
    if(nCols * nRows != Rf_xlength(x)){
      re = PROTECT(make_error("C++ `fastColMeans`: `xDim` is not consistent with `x`"));
      UNPROTECT(1);
      return re;
    }
  }

  T* x_ptr = get_sexp_pointer<T>(x);

  R_xlen_t ii, jj, kk, count;
  double tmp = 0.0;
  double* tmp_ptr = &tmp;

  if(col == R_NilValue){
    re = PROTECT(Rf_allocVector(REALSXP, nCols));
    double* re_ptr = REAL(re);
    for(ii = 0; ii < nCols; ii++, re_ptr++){
      *re_ptr = 0.0;
      count = 0;
      for(jj = 0; jj < nRows; jj++){
        *tmp_ptr = (*(x_ptr + (jj + ii * nRows)));
        if(*tmp_ptr != NA_REAL){
          *re_ptr += *tmp_ptr;
          count++;
        }
      }
      *re_ptr /= (double)(count);
    }
    UNPROTECT(1);
    return re;
  } else {
    re = PROTECT(Rf_allocVector(REALSXP, nCols));
    SEXP re2 = PROTECT(Rf_allocVector(REALSXP, Rf_xlength(col)));
    const double* re_ptr0 = REAL(re);
    double* re_ptr;
    double* re2_ptr = REAL(re2);
    int* col_ptr = INTEGER(col);

    re_ptr = (double*)(re_ptr0);
    for(ii = 0; ii < nCols; ii++, re_ptr++){
      *re_ptr = NA_REAL;
    }

    for(ii = 0; ii < Rf_xlength(col); ii++, col_ptr++, re2_ptr++){
      if(R_finite(*col_ptr) && *col_ptr >= 1 && *col_ptr <= nCols) {

        kk = (*col_ptr) - 1;
        re_ptr = (double*)(re_ptr0 + kk);
        if(!R_finite(*re_ptr)){

          *re_ptr = 0.0;
          count = 0;
          for(jj = 0; jj < nRows; jj++){
            *tmp_ptr = (*(x_ptr + (jj + kk * nRows)));
            if(*tmp_ptr != NA_REAL){
              *re_ptr += *tmp_ptr;
              count++;
            }
          }
          *re_ptr /= (double)(count);
        }

        *re2_ptr = *re_ptr;

      } else {
        *re2_ptr = NA_REAL;
      }
    }
    UNPROTECT(2);
    return re2;

  }
  return re;
}

// [[Rcpp::export]]
SEXP fastColMeans(const SEXP& x, const SEXP& col, const SEXP& xDim){
  SEXP re;

  switch(TYPEOF(x)){
  case INTSXP:
  case LGLSXP:
    re = PROTECT(fastColMeans_template<int>(x, col, xDim));
    break;
  case REALSXP:
    re = PROTECT(fastColMeans_template<double>(x, col, xDim));
    break;
  default:
    re = PROTECT(make_error("C++ `fastColMeans`: Unsupported SEXP type. Only numerical matrices are supported"));
  }
  UNPROTECT(1);
  return re;
}

/*** R
a <- matrix(rnorm(1000), nrow = 5)
range(fastColMeans(a) - colMeans(a))
microbenchmark::microbenchmark(
  {fastColMeans(a)},
  {colMeans(a)}, times = 1000
)

idx <- as.integer(c(NA, sample(200, 200)))
microbenchmark::microbenchmark(
  {fastColMeans(a, idx)},
  {colMeans(a[, idx])}, times = 1000
)
identical(fastColMeans(a, idx), colMeans(a[, idx]))
*/
