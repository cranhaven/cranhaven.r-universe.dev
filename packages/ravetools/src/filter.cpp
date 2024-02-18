#include "filter.h"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]


// assumes a, b are in the same length & they are double vectors
// [[Rcpp::export]]
SEXP cpp_filter(SEXP b, SEXP a, SEXP x, SEXP z) {

  R_xlen_t na = XLENGTH(a);
  R_xlen_t nb = XLENGTH(b);

  if( nb - na != 0 ) {
    Rcpp::stop("C++ `cpp_filter`: filter a and filter b must share the same length");
  }

  if( TYPEOF(a) != REALSXP ) {
    Rcpp::stop("C++ `cpp_filter`: filter a must be double vector");
  }
  if( TYPEOF(b) != REALSXP ) {
    Rcpp::stop("C++ `cpp_filter`: filter b must be double vector");
  }
  if( TYPEOF(x) != REALSXP ) {
    Rcpp::stop("C++ `cpp_filter`: input x must be double vector");
  }
  if( TYPEOF(z) != REALSXP ) {
    Rcpp::stop("C++ `cpp_filter`: initial condition z must be double vector");
  }
  if( na - 1 - XLENGTH(z) > 0 ) {
    Rcpp::stop("C++ `cpp_filter`: initial condition z must have length at least length(a)-1");
  }

  SEXP a1 = PROTECT(Rf_allocVector(REALSXP, na));
  SEXP b1 = PROTECT(Rf_allocVector(REALSXP, na));
  SEXP z1 = PROTECT(Rf_allocVector(REALSXP, na - 1));
  double* ptr_a = REAL(a);
  double* ptr_b = REAL(b);
  double* ptr_z = REAL(z);
  double* ptr_a1 = REAL(a1);
  double* ptr_b1 = REAL(b1);
  double* ptr_z1 = REAL(z1);

  double a0 = *ptr_a;

  R_xlen_t ii = 0;
  for(ii = 0; ii < na; ii++) {

    *ptr_b1++ = *ptr_b++ / a0;
    *ptr_a1++ = *ptr_a++ / a0;

    if( ii < na-1 ) {
      *ptr_z1++ = *ptr_z++;
    }

  }

  R_xlen_t nx = XLENGTH(x);

  SEXP re = PROTECT(Rf_allocVector(REALSXP, nx));

  ptr_a1 = REAL(a1);
  ptr_b1 = REAL(b1);
  ptr_z1 = REAL(z1);
  double* ptr_re = REAL(re);
  double* ptr_x = REAL(x);

  for(R_xlen_t m = 0; m < nx; m++, ptr_x++, ptr_re++ ) {

    *ptr_re = *ptr_b1 * *ptr_x + *ptr_z1;

    for( ii = 1 ; ii < na-1 ; ii++ ) {
      *(ptr_z1 + (ii-1)) = *(ptr_b1 + ii) * *ptr_x + *(ptr_z1 + ii) - *(ptr_a1 + ii) * *ptr_re;
    }
    *(ptr_z1 + (na-2)) = *(ptr_b1 + (na-1)) * *ptr_x - *(ptr_a1 + (na-1)) * *ptr_re;
  }

  SEXP l = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(l, 0, re);
  SET_VECTOR_ELT(l, 1, z1);

  UNPROTECT(5);
  return(l);
}


/***R
initialize_filter <- function(b, a, x) {
  # make sure a, b share the same order
  na <- length(a)
  nb <- length(b)

  if( na > nb ) {
    b <- c(b, rep(0, na - nb))
    n <- na
  } else {
    a <- c(a, rep(0, nb - na))
    n <- nb
  }

  # length of edge transients
  nf <- max(1, 3 * (n - 1))

  # compute the initial condition if n > 1
  if( n > 1 ) {
    z1 <- diag(1, n - 1) - cbind( -a[-1], rbind(diag(1, n - 2), 0) )
    z2 <- b[-1] - b[1] * a[-1]
    z <- solve(z1, z2)
  } else {
    z <- numeric(0)
  }
  list(
    a = a,
    b = b,
    z = z,
    nfilt = n,
    nfact = nf
  )
}
myFilter <- function(b, a, x, z) {
  # make sure a, b share the same order
  na <- length(a)
  nb <- length(b)
  if( na > nb ) {
    b <- c(b, rep(0, na - nb))
    n <- na
  } else {
    a <- c(a, rep(0, nb - na))
    n <- nb
  }
  b <- b / a[1]
  a <- a / a[1]
  y <- rep(0, length(x))
  if(missing(z)) {
    z <- rep(0, n - 1)
  }

  for(m in 1:length(y)) {
    xm <- x[m]
    y[m] <- b[1] * xm + z[1]
    ym <- y[m]
    for( i in 2: (n-1)) {
      z[ i-1 ] = b[i] * xm + z[i] - a[i] * ym
    }
    z[n-1] = b[n] * xm - a[n] * ym
  }
  list(y, z)
}

bf <- signal::butter(2, c(0.15, 0.3))
t <- seq(0, 1, by = 0.01)                     # 1 second sample
x <- as.double(sin(2*pi*t*2.3))
b <- as.double(bf$b)
a <- as.double(bf$a)
nx <- length(x)
init <- initialize_filter(b, a, x)
nfact <- init$nfact
z <- as.double(init$z)


myFilter(b,a,x,z)
cpp_filter(b,a,x,z)

if(FALSE) {
  microbenchmark::microbenchmark(
    R = { myFilter(b,a,x,z) },
    cpp = { cpp_filter(b,a,x,c(z,0)) }
  )
}

*/

