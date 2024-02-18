#include "fastColMeans.h"
#include "TinyParallel.h"

using namespace Rcpp;
// using namespace TinyParallel;

// [[Rcpp::interfaces(r, cpp)]]

template <typename T1, typename T2>
struct FastCov : public TinyParallel::Worker
{
  const SEXP &x1;
  const SEXP &x2;
  const SEXP &col1;
  const SEXP &col2;
  const SEXP &colMeans1;
  const SEXP &colMeans2;
  const R_xlen_t &ncol1;
  const R_xlen_t &ncol2;
  const R_xlen_t &nObs;
  const double &df;
  const SEXP &re;

  T1* x1_ptr;
  T2* x2_ptr;
  R_xlen_t col1_len;
  R_xlen_t col2_len;
  bool col1Null;
  bool col2Null;

  FastCov(
    const SEXP &x1,
    const SEXP &x2,
    const SEXP &col1,
    const SEXP &col2,
    const SEXP &colMeans1,
    const SEXP &colMeans2,
    const R_xlen_t &ncol1,
    const R_xlen_t &ncol2,
    const R_xlen_t &nObs,
    const double &df,
    const SEXP &re
  ): x1(x1), x2(x2), col1(col1), col2(col2),
  colMeans1(colMeans1), colMeans2(colMeans2),
  ncol1(ncol1), ncol2(ncol2), nObs(nObs), df(df),
  re(re) {
    this->x1_ptr = get_sexp_pointer<T1>(x1);
    this->x2_ptr = get_sexp_pointer<T2>(x2);
    this->col1_len = Rf_xlength(colMeans1);
    this->col2_len = Rf_xlength(colMeans2);

    if(col1 == R_NilValue){
      this->col1Null = true;
    } else {
      this->col1Null = false;
    }
    if(col2 == R_NilValue){
      this->col2Null = true;
    } else {
      this->col2Null = false;
    }

  }

  void operator()(std::size_t begin, std::size_t end) {

    T1* x1_ptr2;
    T2* x2_ptr2;

    int* col1_ptr;
    int fake_col1 = 1;
    if(this->col1Null){
      col1_ptr = &(fake_col1);
      *col1_ptr = 1;
    } else {
      col1_ptr = INTEGER(this->col1);
    }

    int* col2_ptr;
    int fake_col2 = 1;
    if(this->col2Null){
      col2_ptr = &(fake_col2);
      *col2_ptr = 1 + (int)(begin);
      // Rcout << *col2_ptr << "\n";
    } else {
      col2_ptr = INTEGER(this->col2) + begin;
    }

    double* colMeans1_ptr = REAL(this->colMeans1);
    double* colMeans2_ptr = REAL(this->colMeans2);
    double* re_ptr = REAL(re);
    R_xlen_t ii, jj, kk, col1Idx, col2Idx;

    double tmp = 0.0, tmp2 = 0.0;
    double* tmp_ptr = &tmp;
    double* tmp2_ptr = &tmp2;

    // --------- Main iteration --------
    colMeans2_ptr = REAL(this->colMeans2) + (int)begin;
    re_ptr = REAL(re) + begin * col1_len;

    // Rcout << *colMeans2_ptr << "\n";

    // if(col2Null){
    //   *col2_ptr = (int)begin + 1;
    // } else {
    //   col2_ptr += begin;
    // }

    for(ii = (R_xlen_t)begin; ii < (R_xlen_t)end; ii++, colMeans2_ptr++){
      // Rcout << fake_col2 << "\n";

      // gcc-UBSAN check error:
      // runtime error: signed integer overflow: -2147483648 - 1 cannot be represented in type 'int'

      // Cannot do this here! *col2_ptr might be NA_INTEGER, which is
      // -2147483648, minus one will cause undefined behavior!!!
      // col2Idx = (*col2_ptr) - 1;

      if(R_finite(*col2_ptr) && *col2_ptr >= 1 && *col2_ptr <= ncol2) {
        col2Idx = (*col2_ptr) - 1;
        colMeans1_ptr = REAL(colMeans1);
        if(!col1Null){
          col1_ptr = INTEGER(this->col1);
        } else {
          *col1_ptr = 1;
        }

        for(jj = 0; jj < col1_len; jj++, colMeans1_ptr++){

          if(R_finite(*col1_ptr) && *col1_ptr > 0 && *col1_ptr <= ncol1) {
            col1Idx = (*col1_ptr) - 1;

            x1_ptr2 = (T1*)(x1_ptr + col1Idx * nObs);
            x2_ptr2 = (T2*)(x2_ptr + col2Idx * nObs);
            // Rcout << "-------\n";

            // cov c1, c2 columns
            *tmp_ptr = 0;
            for(kk = 0; kk < nObs; kk++, x1_ptr2++, x2_ptr2++){
              // Rcout << *x1_ptr2 << " " << *x2_ptr2 << "\n";
              // if(*x1_ptr2 == na1) {
              //   *tmp_ptr = NA_REAL;
              //   break;
              // }
              // if(*x2_ptr2 == na2) {
              //   *tmp_ptr = NA_REAL;
              //   break;
              // }

              // Rcout << *x1_ptr2 << " " << *x2_ptr2 << "\n";

              *tmp2_ptr = (*x1_ptr2) * (*x2_ptr2);
              // if( *tmp2_ptr == NA_REAL ){
              //   *tmp_ptr = NA_REAL;
              //   break;
              // }
              if( *tmp_ptr == NA_REAL ){
                break;
              }

              // Rcout << "         + " << *tmp2_ptr << "\n";
              *tmp_ptr += *tmp2_ptr;
            }
            // Rcout << "         = " << *tmp_ptr << "\n";
            *re_ptr++ = (*tmp_ptr - *colMeans1_ptr * *colMeans2_ptr * nObs) / df;
          } else {
            *re_ptr++ = NA_REAL;
          }

          if(col1Null){
            *col1_ptr += 1;
          } else {
            col1_ptr++;
          }
        }
      } else {
        for(jj = 0; jj < col1_len; jj++, re_ptr++){
          *re_ptr = NA_REAL;
        }
      }

      if(col2Null){
        *col2_ptr += 1;
      } else {
        col2_ptr++;
      }
    }

  }

};



template <typename T1, typename T2>
SEXP fastcov_template(
    const SEXP &x1,
    const SEXP &x2,
    const SEXP &col1,
    const SEXP &col2,
    const T1 &na1,
    const T2 &na2,
    double df = -1) {


  SEXP re = R_NilValue;

  R_xlen_t nObs, nObs_, ncol1, ncol2;

  SEXP x1Dim = PROTECT(Rf_getAttrib(x1, R_DimSymbol));
  if(TYPEOF(x1Dim) == REALSXP){
    nObs = (R_xlen_t)(REAL(x1Dim)[0]);
    ncol1 = (R_xlen_t)(REAL(x1Dim)[1]);
  } else {
    nObs = INTEGER(x1Dim)[0];
    ncol1 = INTEGER(x1Dim)[1];
  }
  UNPROTECT(1); // x1Dim

  SEXP x2Dim = PROTECT(Rf_getAttrib(x2, R_DimSymbol));
  if(TYPEOF(x2Dim) == REALSXP){
    nObs_ = (R_xlen_t)(REAL(x2Dim)[0]);
    ncol2 = (R_xlen_t)(REAL(x2Dim)[1]);
  } else {
    nObs_ = INTEGER(x2Dim)[0];
    ncol2 = INTEGER(x2Dim)[1];
  }
  UNPROTECT(1); // x2Dim

  // check
  if( nObs != nObs_ ){
    PROTECT(re = make_error("C++ `fastcov`: `x1` and `x2` have different number of observations"));
    UNPROTECT(1); // re
    return re;
  }
  if( df <= 0.0 ){
    df = nObs - 1;
  }

  // bool col1Null = true;
  SEXP col1_ = R_NilValue;
  if(col1 != R_NilValue){
    // col1Null = false;
    if(TYPEOF(col1) == INTSXP){
      PROTECT(col1_ = col1);
    } else {
      PROTECT(col1_ = Rf_coerceVector(col1, INTSXP));
    }
  } else {
    PROTECT(col1_ = col1);
  }

  // bool col2Null = true;
  SEXP col2_ = R_NilValue;
  if(col2 != R_NilValue){
    // col2Null = false;
    if(TYPEOF(col2) == INTSXP){
      PROTECT(col2_ = col2);
    } else {
      PROTECT(col2_ = Rf_coerceVector(col2, INTSXP));
    }
  } else {
    PROTECT(col2_ = col2);
  }


  SEXP colMeans1 = PROTECT(fastColMeans(x1, col1_, R_NilValue));
  SEXP colMeans2 = PROTECT(fastColMeans(x2, col2_, R_NilValue));

  R_xlen_t col1_len = Rf_xlength(colMeans1);
  R_xlen_t col2_len = Rf_xlength(colMeans2);
  re = PROTECT(Rf_allocVector(REALSXP, col1_len * col2_len));
  SEXP reDim = PROTECT(Rf_allocVector(INTSXP, 2));
  *(INTEGER(reDim)) = col1_len;
  *(INTEGER(reDim) + 1) = col2_len;
  Rf_setAttrib(re, R_DimSymbol, reDim);


  FastCov<T1, T2> fcov(x1, x2, col1_, col2_, colMeans1, colMeans2, ncol1, ncol2, nObs, df, re);
  TinyParallel::parallelFor(0, col2_len, fcov);


  // const T1* x1_ptr = get_sexp_pointer<T1>(x1);
  // const T2* x2_ptr = get_sexp_pointer<T2>(x2);
  // T1* x1_ptr2;
  // T2* x2_ptr2;
  //
  // int* col1_ptr;
  // if(col1Null){
  //   int fake_col1 = 1;
  //   col1_ptr = &(fake_col1);
  //   *col1_ptr = 1;
  // } else {
  //   col1_ptr = INTEGER(col1_);
  // }
  //
  // int* col2_ptr;
  // if(col2Null){
  //   int fake_col2 = 1;
  //   col2_ptr = &(fake_col2);
  //   *col2_ptr = 1;
  // } else {
  //   col2_ptr = INTEGER(col2_);
  // }
  //
  // double* colMeans1_ptr = REAL(colMeans1);
  // double* colMeans2_ptr = REAL(colMeans2);
  // double* re_ptr = REAL(re);
  // R_xlen_t ii, jj, kk, col1Idx, col2Idx;
  //
  // double tmp = 0.0, tmp2 = 0.0;
  // double* tmp_ptr = &tmp;
  // double* tmp2_ptr = &tmp2;
  //
  // // --------- Main iteration --------
  // R_xlen_t begin = 0, end = col2_len;
  // colMeans2_ptr = REAL(colMeans2) + begin;
  // re_ptr = REAL(re) + begin * col1_len;
  //
  // if(col2Null){
  //   *col2_ptr = (int)begin + 1;
  // } else {
  //   col2_ptr += begin;
  // }
  //
  // for(ii = begin; ii < end; ii++, colMeans2_ptr++){
  //   col2Idx = (*col2_ptr) - 1;
  //
  //   if(R_finite(col2Idx) && col2Idx >= 0 && col2Idx < ncol2) {
  //     colMeans1_ptr = REAL(colMeans1);
  //     if(!col1Null){
  //       col1_ptr = INTEGER(col1_);
  //     } else {
  //       *col1_ptr = 1;
  //     }
  //
  //     for(jj = 0; jj < col1_len; jj++, colMeans1_ptr++){
  //       col1Idx = (*col1_ptr) - 1;
  //
  //       if(R_finite(col1Idx) && col1Idx >= 0 && col1Idx < ncol1) {
  //         x1_ptr2 = (T1*)(x1_ptr + col1Idx * nObs);
  //         x2_ptr2 = (T2*)(x2_ptr + col2Idx * nObs);
  //         // Rcout << "-------\n";
  //
  //
  //         // cov c1, c2 columns
  //         *tmp_ptr = 0;
  //         for(kk = 0; kk < nObs; kk++, x1_ptr2++, x2_ptr2++){
  //           // Rcout << *x1_ptr2 << " " << *x2_ptr2 << "\n";
  //           // if(*x1_ptr2 == na1) {
  //           //   *tmp_ptr = NA_REAL;
  //           //   break;
  //           // }
  //           // if(*x2_ptr2 == na2) {
  //           //   *tmp_ptr = NA_REAL;
  //           //   break;
  //           // }
  //
  //           *tmp2_ptr = (*x1_ptr2) * (*x2_ptr2);
  //           // if( *tmp2_ptr == NA_REAL ){
  //           //   *tmp_ptr = NA_REAL;
  //           //   break;
  //           // }
  //           if( *tmp_ptr == NA_REAL ){
  //             break;
  //           }
  //
  //           // Rcout << "         + " << *tmp2_ptr << "\n";
  //           *tmp_ptr += *tmp2_ptr;
  //         }
  //         // Rcout << "         = " << *tmp_ptr << "\n";
  //         *re_ptr++ = (*tmp_ptr - *colMeans1_ptr * *colMeans2_ptr * nObs) / df;
  //       } else {
  //         *re_ptr++ = NA_REAL;
  //       }
  //
  //       if(col1Null){
  //         *col1_ptr += 1;
  //       } else {
  //         col1_ptr++;
  //       }
  //     }
  //   } else {
  //     for(jj = 0; jj < col1_len; jj++, re_ptr++){
  //       *re_ptr = NA_REAL;
  //     }
  //   }
  //
  //   if(col2Null){
  //     *col2_ptr += 1;
  //   } else {
  //     col2_ptr++;
  //   }
  // }



  UNPROTECT(6); // colMeans1, colMeans2, col1_, col2_, re, reDim
//
//   const R_xlen_t re_nrow = col1.length();
//   const R_xlen_t re_ncol = col2.length();
//   SEXP re = PROTECT(Rf_allocVector(REALSXP, re_nrow * re_ncol));
//   SEXP dm = PROTECT(Rf_allocVector(INTSXP, 2));
//   INTEGER(dm)[0] = re_nrow;
//   INTEGER(dm)[1] = re_ncol;
//   Rf_setAttrib(re, R_DimSymbol, dm);
//   // double* re_ptr = REAL(re);
//
//   FastCov fcov(x1, x2, col1, col2, cm1, cm2, nrow, df, re);
//   parallelFor(0, re_ncol, fcov);
//
//   UNPROTECT(2);
  return(re);
}


SEXP as_numeric(const SEXP& x) {
  SEXPTYPE xType = TYPEOF(x);
  if(xType == INTSXP || xType == REALSXP || xType == LGLSXP ||
     xType == RAWSXP || xType == CPLXSXP) {
    return(x);
  }
  if(xType != VECSXP){
    SEXP re = PROTECT(Rf_coerceVector(x, REALSXP));
    UNPROTECT(1);
    return re;
  }
  R_xlen_t ncols = Rf_xlength(x);
  if(ncols == 0) {
    SEXP re = PROTECT(Rf_allocVector(REALSXP, 0));
    UNPROTECT(1);
    return re;
  }
  R_xlen_t nrows = Rf_xlength(VECTOR_ELT(x, 0));
  SEXP re = PROTECT(Rf_allocVector(REALSXP, ncols * nrows));
  SEXP colData, colData_;
  double* re_ptr = REAL(re);
  double* colData_ptr;

  for(R_xlen_t c = 0; c < ncols; c++){
    colData = VECTOR_ELT(x, c);
    if(Rf_xlength(colData) != nrows){
      stop("Cannot simplify a list object into a matrix");
    }
    if(TYPEOF(colData) != REALSXP){
      colData_ = PROTECT(Rf_coerceVector(colData, REALSXP));
    } else {
      colData_ = PROTECT(colData);
    }
    colData_ptr = REAL(colData_);
    memcpy(re_ptr, colData_ptr, nrows * sizeof(double));
    UNPROTECT(1);
    re_ptr += nrows;
  }
  SEXP reDim = PROTECT(Rf_allocVector(INTSXP, 2));
  *(INTEGER(reDim)) = nrows;
  *(INTEGER(reDim) + 1) = ncols;
  Rf_setAttrib(re, R_DimSymbol, reDim);
  UNPROTECT(2);
  return re;
}

// [[Rcpp::export]]
SEXP fastcov(const SEXP &x1,
             const SEXP &x2,
             const SEXP &col1 = R_NilValue,
             const SEXP &col2 = R_NilValue,
             const double &df = -1.0) {
  SEXPTYPE x1Type = TYPEOF(x1);
  SEXPTYPE x2Type = TYPEOF(x2);
  SEXP re = R_NilValue;

  SEXP x1_ = R_NilValue;
  if(x1Type == VECSXP){
    x1_ = PROTECT(as_numeric(x1));
    x1Type = TYPEOF(x1_);
  } else {
    x1_ = PROTECT(x1);
  }
  SEXP x2_ = x2;
  if(x2Type == VECSXP){
    x2_ = PROTECT(as_numeric(x2));
    x2Type = TYPEOF(x2_);
  } else {
    x2_ = PROTECT(x2);
  }
  // Rcout << x1Type << " " << x2Type << "\n";

  if(x1Type == INTSXP && x2Type == INTSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_INTEGER, NA_INTEGER, df));
  } else if (x1Type == REALSXP && x2Type == INTSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_REAL, NA_INTEGER, df));
  } else if (x1Type == LGLSXP && x2Type == INTSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_LOGICAL, NA_INTEGER, df));
  } else if(x1Type == INTSXP && x2Type == REALSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_INTEGER, NA_REAL, df));
  } else if (x1Type == REALSXP && x2Type == REALSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_REAL, NA_REAL, df));
  } else if (x1Type == LGLSXP && x2Type == REALSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_LOGICAL, NA_REAL, df));
  } else if(x1Type == INTSXP && x2Type == LGLSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_INTEGER, NA_LOGICAL, df));
  } else if (x1Type == REALSXP && x2Type == LGLSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_REAL, NA_LOGICAL, df));
  } else if (x1Type == LGLSXP && x2Type == LGLSXP){
    PROTECT(re = fastcov_template(x1_, x2_, col1, col2, NA_LOGICAL, NA_LOGICAL, df));
  } else {
    PROTECT(re = make_error("C++ `fastcov`: Unsupported input data type. Please make sure the inputs are numerical."));
  }
  UNPROTECT(3);
  return re;
}

/*** R
ravetools_threads(n_threads = 2)
a = matrix(1:10, nrow = 5)
b = matrix(1:50, nrow = 5)
y <- fast_cov(a, b, col_x = c(2,1), NULL)

fastColMeans(a, c(2L,1L), NULL)

z <- cov(a[,2:1], b)
y - z

fastcov(a, b, col1 = NULL, NULL)

devtools::load_all()
RcppParallel::ravetools_threads(n_threads = 8)

x <- matrix(rnorm(100000), nrow = 1000)
y <- matrix(rnorm(100000), nrow = 1000)
col1 <- 1:100; sample(99)
col2 <- 1:100; sample(99)

a <- cov(x[,col1], y[,col2])
b <- fastcov(x, y, col1 = col1, col2 = col2)
range(a-b)

microbenchmark::microbenchmark(
  cpp = {
    fastcov(x, y, col1 = col1, col2 = col2)
  },
  r = {
    cov(x[,col1], y[,col2])
  },
  unit = 'ms', times = 100
)


*/
