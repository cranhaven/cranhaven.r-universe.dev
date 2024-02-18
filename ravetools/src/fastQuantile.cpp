#include "fastQuantile.h"

using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// choose random index (not really random but should be enough for quantiles)
static unsigned long seed1=123456789, seed2=362436069, seed3=521288629;

R_xlen_t randIndex(const R_xlen_t &totalSize) {
  unsigned long t;
  seed1 ^= seed1 << 16;
  seed1 ^= seed1 >> 5;
  seed1 ^= seed1 << 1;

  t = seed1;
  seed1 = seed2;
  seed2 = seed3;
  seed3 = t ^ seed1 ^ seed2;
  return seed3 % totalSize;
}

// Get quick-select pivot value
template <typename T>
double getPivot(T* &bufptr, const R_xlen_t &xlen) {
  return (double) *(bufptr + randIndex(xlen));
}

// Get max/min from a vector, and move the result to the end of the vector
// so the inputs will be mutated
template <typename T>
T findMax(T* &bufptr, R_xlen_t &xlen, R_xlen_t &i, T* &pt0, T* &pt1) {
  pt0 = bufptr;
  pt1 = bufptr + (xlen - 1);
  T tmp;
  for(i = 0; i < (xlen-1); i++, pt0++ ) {
    if(*pt0 > *pt1) {
      tmp = *pt0;
      *pt0 = *pt1;
      *pt1 = tmp;
    }
  }
  return *pt1;
}
template <typename T>
T findMin(T* &bufptr, R_xlen_t &xlen, R_xlen_t &i, T* &pt0, T* &pt1) {
  pt0 = bufptr;
  pt1 = bufptr + (xlen - 1);
  T tmp;
  for(i = 0; i < (xlen-1); i++, pt0++ ) {
    if(*pt0 < *pt1) {
      tmp = *pt0;
      *pt0 = *pt1;
      *pt1 = tmp;
    }
  }
  return *pt1;
}

// Get quantile value:
// bufptr: the vector containing data (will mutate)
// buf2ptr: buffer vector for temporary storage (will mutate)
// xlen: length of the vector
// xlenOdd: whether the values used to compute quantile single
// k the integer index of the values to compute the quantile (if not xlenOdd,
// then k will be index of the larger one)
template <typename T>
double quickSelectQuantile(
    T* &bufptr, T* &buf2ptr, R_xlen_t &xlen, R_xlen_t &xlenOdd, R_xlen_t &k,
    const double &quantile) {
  // Initialize, reuse the memory

  T* pt0 = bufptr;
  T* pt1 = bufptr;
  T* pt2 = buf2ptr;
  R_xlen_t lsize = 0;
  R_xlen_t rsize = 0;
  R_xlen_t i = 0;

  double pivot = getPivot(bufptr, xlen);

  // iterate bufptr through buf, find lowers and uppers
  for(i = 0; i < xlen; i++, pt0++) {
    if(*pt0 < pivot) {
      *pt1++ = *pt0;
      lsize++;
    } else if(*pt0 > pivot) {
      *pt2++ = *pt0;
      rsize++;
    }
  }

  //check size
  if( lsize > 0 ) {
    if(k < lsize) {
      xlen = lsize;
      return quickSelectQuantile(bufptr, buf2ptr, xlen, xlenOdd, k, quantile);
    }

    if( k == lsize ) {
      //   Rcout << "k == lsize [" << k << " " << lsize <<"] " << xlenOdd;
      if( xlenOdd ) {
        // find max from buf (1 ~ lsize)
        return findMax(bufptr, lsize, i, pt0, pt1);
      } else {
        // find max 2 from buf (1 ~ lsize)

        // this will move largest value to the end
        double medianR = (double) findMax<T>(bufptr, lsize, i, pt0, pt1);
        lsize--;
        double medianL = (double) findMax<T>(bufptr, lsize, i, pt0, pt1);
        return (medianL * quantile + medianR * (1.0l - quantile));
      }
    }

    if( k - 1 == lsize ) {
      //   Rcout << "k - 1 == lsize [" << k << " " << lsize <<"] " << xlenOdd;
      double medianR;
      double medianL = 0.0l; // avoid not initialized warnings
      if( !xlenOdd ) {
        // medianL is the max of buf
        medianL = findMax(bufptr, lsize, i, pt0, pt1);
      }

      if( xlen - lsize - rsize > 0 ) {
        // pivot is in the array, return that
        medianR = pivot;
      } else {
        // find the minimum from buf2
        medianR = findMin(buf2ptr, rsize, i, pt0, pt1);
      }
      if( xlenOdd ) {
        return medianR;
      } else {
        return (medianL * quantile + medianR * (1.0l - quantile));
      }
    }
  }


  // from now on, nothing interesting in buf
  if( xlen - rsize - lsize > 0 ) {
    if( k <= xlen - rsize ) {
      //   Rcout << "k <= xlen - rsize [" << k << " " << lsize <<"] " << xlenOdd;
      // pivot is the median [buf pivot ... pivot].length >= k
      return pivot;
    }

    if( k - 1 == xlen - rsize ) {
      // medianR is min(buf2)
      //   Rcout << "k - 1 == xlen - rsize [" << k << " " << lsize <<"] " << xlenOdd;
      double medianR = findMin(buf2ptr, rsize, i, pt0, pt1);
      if( xlenOdd ) {
        return medianR;
      } else {
        return (pivot * quantile + medianR * (1.0l - quantile));
      }
    }
  }


  if( !xlenOdd && k - 2 == xlen - rsize ) {

    //   Rcout << "k - 2 == xlen - rsize [" << k << " " << lsize <<"] " << xlenOdd;
    // this will move largest value to the end
    double medianL = findMin(buf2ptr, rsize, i, pt0, pt1);
    rsize--;
    double medianR = findMin(buf2ptr, rsize, i, pt0, pt1);
    return (medianL * quantile + medianR * (1.0l - quantile));
  }

  // now k-2 >= xlen - rsize, and median is in buf2
  k -= xlen - rsize;
  xlen = rsize;
  return quickSelectQuantile(buf2ptr, bufptr, xlen, xlenOdd, k, quantile);

}

template <typename T>
double quickQuantileInternal(T* &xptr, T* &bufptr, R_xlen_t &xlen, const double &prob) {
  if(xlen == 0) {
    return NA_REAL;
  } else if (xlen == 1) {
    return *xptr;
  }

  R_xlen_t xlenOdd = 1;
  R_xlen_t k = (R_xlen_t) std::floor((xlen+1) * prob);

  if( (xlen+1) * prob - (R_xlen_t)((xlen+1) * prob) > 0 ) {
    xlenOdd = 0;
    k++;
  }

  double re = NA_REAL;
  R_xlen_t i = 0;
  if(k == 0) {
    T *pt0, *pt1;
    re = (double) findMin(xptr, xlen, i, pt0, pt1);
    return re;
  } else if(k > xlen){
    T *pt0, *pt1;
    re = (double) findMax(xptr, xlen, i, pt0, pt1);
    return re;
  }

  re = quickSelectQuantile<T>(xptr, bufptr, xlen, xlenOdd, k, prob);

  return re;

}


SEXP quickQuantile_double(const SEXP &x, const double &prob, const bool &naRm,
                          const bool &inplace) {

  // allocate a new vector that is mutable
  R_xlen_t xlen_ = XLENGTH(x);

  if(xlen_ == 1) { return x; }
  SEXP re = PROTECT(Rf_allocVector(REALSXP, 1));
  *(REAL(re)) = NA_REAL;
  if( xlen_ == 0 ) {
    UNPROTECT(1);
    return re;
  }

  // Create buffer & remove NAs
  SEXP buf = PROTECT(Rf_allocVector(REALSXP, xlen_));
  double* pt0 = REAL(x);
  double* pt1 = REAL(buf);

  R_xlen_t xlen = 0;
  R_xlen_t i = 0;

  for(i = 0; i < xlen_; i++, pt0++) {
    if(*pt0 != NA_REAL) {
      *pt1++ = *pt0;
      xlen++;
    } else if(!naRm) {
      UNPROTECT(2);
      return re;
    }
  }
  if(xlen == 0) {
    UNPROTECT(2);
    return re;
  } else if(xlen == 1) {
    *(REAL(re)) = *(REAL(buf));
    UNPROTECT(2);
    return re;
  }

  SEXP buf2;
  if( inplace ) {
    buf2 = PROTECT(x);
  } else {
    buf2 = PROTECT(Rf_allocVector(REALSXP, xlen));
  }


  // reuse pointer
  pt0 = REAL(buf);
  pt1 = REAL(buf2);
  *(REAL(re)) = quickQuantileInternal(pt0, pt1, xlen, prob);

  UNPROTECT(3);
  return re;
}

SEXP quickQuantile_integer(const SEXP &x, const double &prob, const bool &naRm,
                           const bool &inplace) {

  // allocate a new vector that is mutable
  R_xlen_t xlen_ = XLENGTH(x);

  if(xlen_ == 1) { return x; }
  SEXP re = PROTECT(Rf_allocVector(REALSXP, 1));
  *(REAL(re)) = NA_REAL;
  if( xlen_ == 0 ) {
    UNPROTECT(1);
    return re;
  }

  // Create buffer & remove NAs
  SEXP buf = PROTECT(Rf_allocVector(INTSXP, xlen_));
  int* pt0 = INTEGER(x);
  int* pt1 = INTEGER(buf);

  R_xlen_t xlen = 0;
  R_xlen_t i = 0;

  for(i = 0; i < xlen_; i++, pt0++) {
    if(*pt0 != NA_INTEGER) {
      *pt1++ = *pt0;
      xlen++;
    } else if(!naRm) {
      UNPROTECT(2);
      return re;
    }
  }
  if(xlen == 0) {
    UNPROTECT(2);
    return re;
  } else if(xlen == 1) {
    *(REAL(re)) = (double) *(INTEGER(buf));
    UNPROTECT(2);
    return re;
  }

  SEXP buf2;
  if( inplace ) {
    buf2 = PROTECT(x);
  } else {
    buf2 = PROTECT(Rf_allocVector(INTSXP, xlen));
  }

  // reuse pointer
  pt0 = INTEGER(buf);
  pt1 = INTEGER(buf2);
  *(REAL(re)) = quickQuantileInternal(pt0, pt1, xlen, prob);

  UNPROTECT(3);
  return re;
}


// [[Rcpp::export]]
SEXP quickQuantile(const SEXP &x, const double &prob, const bool &naRm) {

  switch(TYPEOF(x)) {
    case LGLSXP:
    case RAWSXP: {
      SEXP x_ = PROTECT(Rf_coerceVector(x, INTSXP));
      SEXP re = PROTECT(quickQuantile_integer(x_, prob, naRm, true));
      UNPROTECT(2);
      return re;
      break;
    }
    case INTSXP: {
      SEXP re = PROTECT(quickQuantile_integer(x, prob, naRm, false));
      UNPROTECT(1);
      return re;
      break;
    }
    case REALSXP: {
      SEXP re = PROTECT(quickQuantile_double(x, prob, naRm, false));
      UNPROTECT(1);
      return re;
      break;
    }
    default: {
      SEXP x_ = PROTECT(Rf_coerceVector(x, REALSXP));
      SEXP re = PROTECT(quickQuantile_double(x_, prob, naRm, true));
      UNPROTECT(2);
      return re;
      break;
    }
  }
}

// [[Rcpp::export]]
SEXP quickMedian(const SEXP &x, const bool &naRm) {
  SEXP re = PROTECT(quickQuantile(x, 0.5l, naRm));
  UNPROTECT(1);
  return re;
}


/*** R
ravetools:::quickMedian(1:100, FALSE)
*/
