#include "utils.h"
#include "TinyParallel.h"
#include "fastQuantile.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

template <typename T>
struct ColumnQuantile : public TinyParallel::Worker
{
  const R_xlen_t &nrows;
  const R_xlen_t &ncols;
  const double &prob;
  const bool &naRm;
  const T na;

  const int &ncores;
  T* &xptr;
  T* &bufptr;
  double* &yptr;
  R_xlen_t paritionSize;


  ColumnQuantile(
    const R_xlen_t &nrows,
    const R_xlen_t &ncols,
    const int &ncores,
    const double &prob,
    const bool &naRm,
    const T na,
    T* &xptr,
    T* &bufptr,
    double* &yptr
  ): nrows(nrows), ncols(ncols), prob(prob), naRm(naRm), na(na), ncores(ncores),
  xptr(xptr), bufptr(bufptr), yptr(yptr) {
    R_xlen_t psize = this->ncols / this->ncores;
    if(psize * this->ncores < this->ncols) {
      psize++;
    }
    this->paritionSize = psize;
  }

  ~ColumnQuantile() {
    this->xptr = NULL;
    this->bufptr = NULL;
    this->yptr = NULL;
  }

  void operator()(std::size_t begin, std::size_t end) {

    R_xlen_t colStart, colEnds;
    T* buf0;
    T* buf1;
    T* buf2;
    double* yptr;
    bool hasNA;

    for(std::size_t thread = begin; thread < end; thread++){
      colStart = (R_xlen_t) (thread * this->paritionSize);
      colEnds = colStart + this->paritionSize;
      if( colStart > this->ncols ) { return; }
      if( colEnds > this->ncols ) { colEnds = this->ncols; }

      // get buffer
      buf0 = this->bufptr + (thread * this->nrows) * 2;

      yptr = this->yptr + colStart;
      hasNA = false;

      R_xlen_t rowLen;
      R_xlen_t i;
      R_xlen_t col_ = colStart;
      for( ; col_ < colEnds; col_++, yptr++ ) {
        buf1 = this->xptr + (col_ * this->nrows);
        buf2 = buf0;
        hasNA = false;
        rowLen = 0;

        // copy and remove NAs to buf0 (1~rowLen)
        for(i = 0; i < this->nrows; i++, buf1++) {
          if(*buf1 == this->na) {
            if(!this->naRm) {
              hasNA = true;
              break;
            }
          } else {
            *buf2++ = *buf1;
            rowLen++;
          }
        }

        if(hasNA){
          *yptr = NA_REAL;
          continue;
        }

        buf1 = buf0;
        buf2 = buf1 + this->nrows;
        *yptr = quickQuantileInternal(buf1, buf2, rowLen, this->prob);
      }

    }
    buf0 = NULL;
    buf1 = NULL;
    buf2 = NULL;
    yptr = NULL;
  }

};





// [[Rcpp::export]]
SEXP columnQuantile(SEXP &x, const double &prob, const bool &naRm) {


  // R_xlen_t xlen = XLENGTH(x);
  R_xlen_t nrows = Rf_nrows(x);
  R_xlen_t ncols = Rf_ncols(x);

  SEXP re = PROTECT(Rf_allocVector(REALSXP, ncols));
  if(ncols == 0) {
    UNPROTECT(1);
    return re;
  }

  if(nrows == 0) {
    SEXP tmp = PROTECT(Rf_allocVector(REALSXP, 1));
    *(REAL(tmp)) = NA_REAL;
    Rf_copyVector(tmp, re);
    UNPROTECT(2);
    return re;
  }

  int ncores = 8;
  SEXP buffers;
  double* reptr = REAL(re);
  switch(TYPEOF(x)) {
    case REALSXP: {
      buffers = PROTECT(Rf_allocVector(REALSXP, ncores * nrows * 2));
      double* xptr = REAL(x);
      double* bptr = REAL(buffers);
      ColumnQuantile<double> columnQuantileWorker(
          nrows, ncols, ncores, prob, naRm, NA_REAL, xptr, bptr, reptr);
      parallelFor(0, ncores, columnQuantileWorker);
      UNPROTECT(1);
      break;
    }
    case INTSXP: {
      buffers = PROTECT(Rf_allocVector(INTSXP, ncores * nrows * 2));
      int* xptr = INTEGER(x);
      int* bptr = INTEGER(buffers);
      ColumnQuantile<int> columnQuantileWorker(
          nrows, ncols, ncores, prob, naRm, NA_INTEGER, xptr, bptr, reptr);
      parallelFor(0, ncores, columnQuantileWorker);
      UNPROTECT(1);
      break;
    }
    case LGLSXP:
    case RAWSXP: {
      SEXP x_ = PROTECT(Rf_coerceVector(x, INTSXP));
      buffers = PROTECT(Rf_allocVector(INTSXP, ncores * nrows * 2));
      int* xptr = INTEGER(x_);
      int* bptr = INTEGER(buffers);
      ColumnQuantile<int> columnQuantileWorker(
          nrows, ncols, ncores, prob, naRm, NA_INTEGER, xptr, bptr, reptr);
      parallelFor(0, ncores, columnQuantileWorker);
      UNPROTECT(2);
      break;
    }
    default: {
      SEXP x_ = PROTECT(Rf_coerceVector(x, REALSXP));
      buffers = PROTECT(Rf_allocVector(REALSXP, ncores * nrows * 2));
      double* xptr = REAL(x_);
      double* bptr = REAL(buffers);
      ColumnQuantile<double> columnQuantileWorker(
          nrows, ncols, ncores, prob, naRm, NA_REAL, xptr, bptr, reptr);
      parallelFor(0, ncores, columnQuantileWorker);
      UNPROTECT(2);
      break;
    }
  }

  UNPROTECT(1); // re
  return re;
}

// [[Rcpp::export]]
SEXP columnMedian(SEXP &x, const bool &naRm) {
  SEXP re = PROTECT(columnQuantile(x, 0.5, naRm));
  UNPROTECT(1);
  return re;
}


/*** R
dim <- c(400, 80)
a = array((rnorm(prod(dim)) * 1000), dim)
ta <- t(a)
# dif <- ravetools:::columnQuantile(a, 0.5, FALSE) - apply(a, 2, median)
# range(dif)

# range(Biobase::rowMedians(t(a)) - ravetools:::columnQuantile(a, 0.5, FALSE))

microbenchmark::microbenchmark(
  baseMedian = {
    apply(a, 2, median)
  },
  columnMedian = {
    ravetools:::columnQuantile(a, 0.5, FALSE)
  },
  quickMedian = {
    apply(a, 2, ravetools:::quickMedian, naRm = FALSE)
  },
  Biobase = {
    Biobase::rowMedians(ta)
  },
  times = 100
)

# replicate(100, ravetools::collapse(a, keep = 1))
*/
