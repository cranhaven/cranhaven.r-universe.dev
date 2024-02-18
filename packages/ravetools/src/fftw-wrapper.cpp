#include "fftw-wrapper.h"
#include "ffts.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

/**
 * Migrated: fftw_r2c, mvfftw_r2c, fftw_c2r
 * TODO: fftw_c2c,
 */

// [[Rcpp::export]]
SEXP fftw_r2c(SEXP data, int HermConj = 1,
              int fftwplanopt = 0,
              SEXP ret = R_NilValue) {
  int nprot = 0;

  // check HermConj and ret
  int xlen = Rf_length(data);
  int retlen = 0;
  if( HermConj == 1 ){
    retlen = xlen;
  } else {
    if( HermConj != 0){ HermConj = 0; }
    retlen = ( xlen / 2 ) + 1;
  }
  if( ret == R_NilValue || ret == R_MissingArg ){
    PROTECT(ret = Rf_allocVector(CPLXSXP, retlen));
    nprot++;
  } else {
    if( TYPEOF(ret) != CPLXSXP ){
      stop("ravetools `fftw_r2c`: `ret` should be complex");
    }
    if( Rf_xlength(ret) < retlen ){
      stop("ravetools `fftw_r2c`: `ret` length should be at least " + std::to_string(retlen));
    }
  }

  if( TYPEOF(data) != REALSXP ){
    // in this case, data is copied anyway, and hence not destroyed
    PROTECT(data = Rf_coerceVector(data, REALSXP));
    nprot++;
  // } else if (MAYBE_REFERENCED(data)) {
  // } else if(!inplace && fftwplanopt <= 0) {
    // avoid inplace calculation, which might destroy the input data
    // data = PROTECT(Rf_duplicate(data));
    // nprot++;
  }

  cfft_r2c(&xlen, REAL(data), reinterpret_cast<fftw_complex*>(&COMPLEX(ret)[0]), &HermConj,
           &fftwplanopt);

  if(nprot > 0){
    UNPROTECT(nprot);
  }
  return ret;
}


// [[Rcpp::export]]
SEXP fftw_c2c(SEXP data, int inverse = 0,
              int fftwplanopt = 0,
              SEXP ret = R_NilValue)
{
  int nprot = 0;
  int xlen = Rf_length(data);

  if(ret == R_NilValue){
    PROTECT(ret = Rf_allocVector(CPLXSXP, xlen));
    nprot++;
  } else {
    if(TYPEOF(ret) != CPLXSXP){
      stop("ravetools `fftw_c2c`: `ret` must be complex");
    }
    if(Rf_length(ret) != xlen) {
      stop("ravetools `fftw_c2c`: `ret` must have length of " + std::to_string(xlen));
    }
  }

  if(TYPEOF(data) != CPLXSXP){
    PROTECT(data = Rf_coerceVector(data, CPLXSXP));
    nprot++;
  }

  if(inverse){
    inverse = 1;
  }

  cfft_c2c(
    &xlen,
    reinterpret_cast<fftw_complex*>(&COMPLEX(data)[0]),
    reinterpret_cast<fftw_complex*>(&COMPLEX(ret)[0]),
    &inverse, &fftwplanopt
  );

  if(nprot > 0){
    UNPROTECT(nprot);
  }

  return ret;
}

// [[Rcpp::export]]
SEXP fftw_c2r(SEXP data, int HermConj = 1,
              int fftwplanopt = 0,
              SEXP ret = R_NilValue){
  int nprot = 0;

  // check HermConj and ret
  int xlen = Rf_length(data);
  int retlen = 0;
  if( HermConj == 1 ){
    retlen = xlen;
  } else {
    if( HermConj != 0){ HermConj = 0; }
    retlen = (xlen - 1) * 2;
  }
  if( ret == R_NilValue || ret == R_MissingArg ){
    PROTECT(ret = Rf_allocVector(REALSXP, retlen));
    nprot++;
  } else {
    if( TYPEOF(ret) != REALSXP ){
      stop("ravetools `fftw_c2r`: `ret` should be double");
    }
    if( Rf_xlength(ret) < retlen ){
      stop("ravetools `fftw_c2r`: `ret` length should be at least " + std::to_string(retlen));
    }
    retlen = Rf_xlength(ret);
  }

  if( TYPEOF(data) != CPLXSXP ){
    PROTECT(data = Rf_coerceVector(data, CPLXSXP));
    nprot++;
    // } else if (MAYBE_REFERENCED(data)) {
    // } else if(!inplace) {
    // data = PROTECT(Rf_duplicate(data));
    // nprot++;
  }

  cfft_c2r(&retlen, &xlen, reinterpret_cast<fftw_complex*>(&COMPLEX(data)[0]),
           REAL(ret), &fftwplanopt);

  if(nprot > 0){
    UNPROTECT(nprot);
  }
  return ret;
}


// [[Rcpp::export]]
SEXP mvfftw_r2c(SEXP data,
                int fftwplanopt = 0,
                SEXP ret = R_NilValue)
{
  int nprot = 0;

  // check HermConj and ret
  int nrows = Rf_nrows(data);
  int ncols = Rf_ncols(data);
  int retrows = ( nrows / 2 ) + 1;
  if( ret == R_NilValue || ret == R_MissingArg ){
    PROTECT(ret = Rf_allocMatrix(CPLXSXP, retrows, ncols));
    nprot++;
  } else {
    if( TYPEOF(ret) != CPLXSXP ){
      stop("ravetools `fftw_r2c`: `ret` should be complex");
    }
    if( Rf_xlength(ret) != retrows * ncols ){
      stop("ravetools `fftw_r2c`: `ret` length should be " + std::to_string(retrows * ncols));
    }
  }



  if( TYPEOF(data) != REALSXP ){
    PROTECT(data = Rf_coerceVector(data, REALSXP));
    nprot++;
    // } else if (MAYBE_REFERENCED(data)) {
    // } else if(!inplace && fftwplanopt <= 0) {
    // data need to be copied
    // however fftwplanopt > 0 will copy eventually, so only
    // copy when fftwplanopt <= 0
    // UPDATE: FFTW3 official document mentions that with FFTW_ESTIMATE,
    //   the input/output arrays are not overwritten during planning.

    // data = PROTECT(Rf_duplicate(data));
    // nprot++;
  }

  cmvfft_r2c(&nrows, &ncols, REAL(data),
             reinterpret_cast<fftw_complex*>(&COMPLEX(ret)[0]),
             &fftwplanopt);

  if(nprot > 0){
    UNPROTECT(nprot);
  }

  return(ret);
}


// [[Rcpp::export]]
SEXP fftw_r2c_2d(SEXP data, int HermConj = 1,
                 int fftwplanopt = 0,
                 SEXP ret = R_NilValue) {
  // make sure data has 2 dimensions
  if(!Rf_isMatrix(data)) {
    Rcpp::stop("C++ `fftw_r2c_2d`: `data` must be a matrix.");
  }

  int nprot = 0;

  int nR = Rf_nrows( data );
  int nC = Rf_ncols( data );

  int nRc;

  if( nR % 2 == 0 ) {
    nRc = nR / 2 + 1;
  } else {
    nRc = (nR + 1) / 2;
  }

  R_xlen_t ret_len = (R_xlen_t)nRc * nC;

  if( HermConj == 1 ) {
    ret_len = (R_xlen_t)nR * nC;
  }

  SEXP reDim = PROTECT(Rf_allocVector(INTSXP, 2));
  nprot++;

  if( nR == 0 ) {
    INTEGER(reDim)[0] = 0;
    ret_len = 0;
  } else {
    if( HermConj == 1 ) {
      INTEGER(reDim)[0] = nR;
    } else {
      INTEGER(reDim)[0] = nRc;
    }
  }

  INTEGER(reDim)[1] = nC;

  // Make sure data is double
  if( TYPEOF(data) != REALSXP ){
    // in this case, data is copied anyway, and hence not destroyed
    PROTECT(data = Rf_coerceVector(data, REALSXP));
    nprot++;
  }

  // initialize ret
  if( ret == R_NilValue || ret == R_MissingArg ){
    PROTECT(ret = Rf_allocVector(CPLXSXP, ret_len));
    nprot++;
  } else {
    if( TYPEOF(ret) != CPLXSXP ){
      stop("ravetools `fftw_r2c_2d`: `ret` should be complex");
    }
    if( Rf_xlength(ret) != ret_len ){
      stop("ravetools `fftw_r2c_2d`: `ret` length should be " + std::to_string(ret_len));
    }
  }

  if( nR == 0 || nC == 0 ) {
    Rf_setAttrib(ret, R_DimSymbol, reDim);
    UNPROTECT( nprot );
    return ret;
  }

  cfft_r2c_2d(&nC, &nR, REAL(data), reinterpret_cast<fftw_complex*>(&COMPLEX(ret)[0]),
              &fftwplanopt);


  if( HermConj == 1 ) {

    // rbind(res, Conj(cbind(res[idxRowAppend, 1], res[idxRowAppend, nC:2])))
    Rcomplex *ptr1, *ptr2, *ptr3;

    for(int colIdx = nC - 1; colIdx >= 0; colIdx--) {

      ptr1 = COMPLEX(ret) + (colIdx * nR);
      ptr2 = COMPLEX(ret) + (colIdx * nRc);

      for(int rowIdx = nRc - 1; rowIdx >= 0; rowIdx--) {
        *(ptr1 + rowIdx) = *(ptr2 + rowIdx);
      }

    }

    for(int colIdx = 0; colIdx < nC; colIdx++) {

      ptr1 = COMPLEX(ret) + (colIdx * nR);
      if( colIdx == 0 ) {
        ptr2 = COMPLEX(ret) + (colIdx * nR) + 1;
      } else {
        ptr2 = COMPLEX(ret) + ((nC - colIdx) * nR) + 1;
      }

      for(int rowIdx = nR - 1; rowIdx >= nRc; rowIdx--, ptr2++) {
        ptr3 = ptr1 + rowIdx;
        ptr3->r = ptr2->r;
        ptr3->i = -ptr2->i;
      }

    }
  }

  Rf_setAttrib(ret, R_DimSymbol, reDim);

  UNPROTECT( nprot );
  return ret;

}

// [[Rcpp::export]]
SEXP fftw_c2c_2d(SEXP data, int inverse = 0, int fftwplanopt = 0,
                 SEXP ret = R_NilValue) {
  // make sure data has 2 dimensions
  if(!Rf_isMatrix(data)) {
    Rcpp::stop("C++ `fftw_c2c_2d`: `data` must be a matrix.");
  }

  int nprot = 0;

  int nR = Rf_nrows( data );
  int nC = Rf_ncols( data );

  R_xlen_t ret_len = (R_xlen_t)nR * (R_xlen_t)nC;

  // Make sure data is double
  if( TYPEOF(data) != CPLXSXP ){
    // in this case, data is copied anyway, and hence not destroyed
    PROTECT(data = Rf_coerceVector(data, CPLXSXP));
    nprot++;
  }

  // initialize ret
  if( ret == R_NilValue || ret == R_MissingArg ){
    PROTECT(ret = Rf_allocVector(CPLXSXP, ret_len));
    nprot++;
  } else {
    if( TYPEOF(ret) != CPLXSXP ){
      stop("ravetools `fftw_c2c_2d`: `ret` should be complex");
    }
    if( Rf_xlength(ret) != ret_len ){
      stop("ravetools `fftw_c2c_2d`: `ret` length should be " + std::to_string(ret_len));
    }
  }

  SEXP reDim = PROTECT(Rf_allocVector(INTSXP, 2));
  nprot++;
  INTEGER(reDim)[0] = nR;
  INTEGER(reDim)[1] = nC;

  Rf_setAttrib(ret, R_DimSymbol, reDim);

  if( nR > 0 && nC > 0 ) {
    cfft_c2c_2d(
      &nC, &nR,
      reinterpret_cast<fftw_complex*>(&COMPLEX(data)[0]),
      reinterpret_cast<fftw_complex*>(&COMPLEX(ret)[0]),
      &inverse, &fftwplanopt
    );
  }

  UNPROTECT( nprot );
  return ret;

}

// [[Rcpp::export]]
SEXP fftw_r2c_3d(SEXP data, int HermConj = 1, int fftwplanopt = 0,
                 SEXP ret = R_NilValue) {
  // make sure data has 2 dimensions
  if(!Rf_isArray(data)) {
    Rcpp::stop("C++ `fftw_r2c_3d`: `data` must be an array.");
  }
  int nprot = 0;
  SEXP ret_dim_ = PROTECT(Rf_getAttrib(data, R_DimSymbol));
  nprot++;

  R_xlen_t dim_len = Rf_xlength(ret_dim_);

  if( dim_len != 3 ) {
    Rcpp::stop("C++ `fftw_r2c_3d`: `data` must be a 3-dimensional array.");
  }

  SEXP ret_dim;
  if(TYPEOF(ret_dim_) != INTSXP) {
    ret_dim = PROTECT(Rf_coerceVector(ret_dim_, INTSXP));
  } else {
    ret_dim = PROTECT(Rf_allocVector(INTSXP, 3));
    Rf_copyVector(ret_dim, ret_dim_);
  }
  nprot++;

  int nR = INTEGER(ret_dim)[0];
  int nC = INTEGER(ret_dim)[1];
  int nS = INTEGER(ret_dim)[2];

  int nRc;

  if( nR % 2 == 0 ) {
    nRc = nR / 2 + 1;
  } else {
    nRc = (nR + 1) / 2;
  }

  R_xlen_t ret_len = (R_xlen_t)nR * nC * nS;

  if( HermConj == 0 && nR > 0 ) {
    ret_len = (R_xlen_t)nRc * nC * nS;
    INTEGER(ret_dim)[0] = nRc;
  }

  // Make sure data is double
  if( TYPEOF(data) != REALSXP ){
    // in this case, data is copied anyway, and hence not destroyed
    PROTECT(data = Rf_coerceVector(data, REALSXP));
    nprot++;
  }

  // initialize ret
  if( ret == R_NilValue || ret == R_MissingArg ){
    PROTECT(ret = Rf_allocVector(CPLXSXP, ret_len));
    nprot++;
  } else {
    if( TYPEOF(ret) != CPLXSXP ){
      stop("ravetools `fftw_r2c_3d`: `ret` should be complex");
    }
    if( Rf_xlength(ret) != ret_len ){
      stop("ravetools `fftw_r2c_3d`: `ret` length should be " + std::to_string(ret_len));
    }
  }

  Rf_setAttrib(ret, R_DimSymbol, ret_dim);
  if( nR > 0 && nC > 0 && nS > 0 ) {

    cfft_r2c_3d(
      &nS, &nC, &nR, REAL(data),
      reinterpret_cast<fftw_complex*>(&COMPLEX(ret)[0]),
      &fftwplanopt
    );

    if( HermConj == 1 ) {

      // res[(nRc + 1):nR, , ] <- Conj(out$res[idx1Append, c(1, nC:2), c(1, nS:2)])
      Rcomplex *ptr1, *ptr2, *ptr3;

      for(int colIdx = (nC * nS) - 1; colIdx >= 0; colIdx--) {

        ptr1 = COMPLEX(ret) + (colIdx * nR);
        ptr2 = COMPLEX(ret) + (colIdx * nRc);

        for(int rowIdx = nRc - 1; rowIdx >= 0; rowIdx--) {
          *(ptr1 + rowIdx) = *(ptr2 + rowIdx);
        }

      }

      for(int sliceIdx = 0; sliceIdx < nS; sliceIdx++) {

        for(int colIdx = 0; colIdx < nC; colIdx++) {

          ptr1 = COMPLEX(ret) + (nR * (colIdx + nC * sliceIdx));
          ptr2 = COMPLEX(ret) + 1;
          if( colIdx > 0 ) {
            ptr2 += (nC - colIdx) * nR;
          }
          if( sliceIdx > 0 ) {
            ptr2 += (nS - sliceIdx) * nR * nC;
          }

          for(int rowIdx = nR - 1; rowIdx >= nRc; rowIdx--, ptr2++) {
            ptr3 = ptr1 + rowIdx;
            ptr3->r = ptr2->r;
            ptr3->i = -ptr2->i;
          }

        }
      }

    }

  }

  UNPROTECT( nprot );
  return ret;
}


// [[Rcpp::export]]
SEXP fftw_c2c_3d(SEXP data, int inverse = 0, int fftwplanopt = 0,
                 SEXP ret = R_NilValue) {
  // make sure data has 2 dimensions
  if(!Rf_isArray(data)) {
    Rcpp::stop("C++ `fftw_c2c_3d`: `data` must be an array.");
  }

  int nprot = 0;
  SEXP ret_dim_ = PROTECT(Rf_getAttrib(data, R_DimSymbol));
  nprot++;

  R_xlen_t dim_len = Rf_xlength(ret_dim_);

  if( dim_len != 3 ) {
    Rcpp::stop("C++ `fftw_r2c_3d`: `data` must be a 3-dimensional array.");
  }

  SEXP ret_dim;
  if(TYPEOF(ret_dim_) != INTSXP) {
    ret_dim = PROTECT(Rf_coerceVector(ret_dim_, INTSXP));
  } else {
    ret_dim = PROTECT(Rf_allocVector(INTSXP, 3));
    Rf_copyVector(ret_dim, ret_dim_);
  }
  nprot++;

  int nR = INTEGER(ret_dim)[0];
  int nC = INTEGER(ret_dim)[1];
  int nS = INTEGER(ret_dim)[2];

  R_xlen_t ret_len = Rf_xlength(data);

  // Make sure data is double
  if( TYPEOF(data) != CPLXSXP ){
    // in this case, data is copied anyway, and hence not destroyed
    PROTECT(data = Rf_coerceVector(data, CPLXSXP));
    nprot++;
  }

  // initialize ret
  if( ret == R_NilValue || ret == R_MissingArg ){
    PROTECT(ret = Rf_allocVector(CPLXSXP, ret_len));
    nprot++;
  } else {
    if( TYPEOF(ret) != CPLXSXP ){
      stop("ravetools `fftw_c2c_3d`: `ret` should be complex");
    }
    if( Rf_xlength(ret) != ret_len ){
      stop("ravetools `fftw_c2c_3d`: `ret` length should be " + std::to_string(ret_len));
    }
  }

  Rf_setAttrib(ret, R_DimSymbol, ret_dim);

  if( nR > 0 && nC > 0 && nS > 0 ) {
    cfft_c2c_3d(
      &nS, &nC, &nR,
      reinterpret_cast<fftw_complex*>(&COMPLEX(data)[0]),
      reinterpret_cast<fftw_complex*>(&COMPLEX(ret)[0]),
      &inverse, &fftwplanopt
    );
  }

  UNPROTECT( nprot );
  return ret;
}


// [[Rcpp::export]]
SEXP conjugate(SEXP data) {
  if(TYPEOF(data) != CPLXSXP){
    stop("`conjugate`: data must be complex");
  }

  int xlen = Rf_length(data);
  int i = 0;
  for(Rcomplex* ptr = COMPLEX(data); i < xlen; i++, ptr++){
    ptr->i = -(ptr->i);
  }

  return R_NilValue;
}


/*** R
x <- rnorm(1000)
ret = double(1000)
a <- fftw_c2r(x, ret = ret)
b <- fftwtools::fftw_c2r(x, 1)
# max(Mod(b-Conj(a)))
range(b-a)
*/
