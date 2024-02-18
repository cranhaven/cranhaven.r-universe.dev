#include <queue>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP bucketFillVolume(SEXP volume, R_xlen_t x, R_xlen_t y, R_xlen_t z, int fill) {

  if(TYPEOF(volume) != INTSXP) {
    Rcpp::stop("ravetools: `bucketFillVolume` input `volume` must be integer");
  }

  SEXP dim = PROTECT(Rf_getAttrib(volume, R_DimSymbol));
  if(LENGTH(dim) != 3) {
    Rcpp::stop("ravetools: `bucketFillVolume` input `volume` must be a 3D array");
  }

  R_xlen_t dim_x = (R_xlen_t)(INTEGER(dim)[0]);
  R_xlen_t dim_y = (R_xlen_t)(INTEGER(dim)[1]);
  R_xlen_t dim_z = (R_xlen_t)(INTEGER(dim)[2]);

  UNPROTECT(1);

  R_xlen_t x_ = x - 1;
  R_xlen_t y_ = y - 1;
  R_xlen_t z_ = z - 1;

  if(
    x_ >= 0 && x_ < dim_x &&
      y_ >= 0 && y_ < dim_y &&
      z_ >= 0 && z_ < dim_z
  ) {

    R_xlen_t idx = x_ + dim_x * (y_ + dim_y * z_);
    int* ptr = INTEGER(volume);
    int value0 = *(ptr + idx);

    std::queue<int> q;
    q.push( x_ );
    q.push( y_ );
    q.push( z_ );
    while( !q.empty() ) {

      x_ = q.front();
      q.pop();

      y_ = q.front();
      q.pop();

      z_ = q.front();
      q.pop();

      idx = x_ + dim_x * (y_ + dim_y * z_);
      if( *(ptr + idx) == value0 ) {

        *(ptr + idx) = fill;

        if( x_ > 0 ) {
          q.push( x_ - 1 );
          q.push( y_ );
          q.push( z_ );
        }

        if( y_ > 0 ) {
          q.push( x_ );
          q.push( y_ - 1 );
          q.push( z_ );
        }

        if( z_ > 0 ) {
          q.push( x_ );
          q.push( y_ );
          q.push( z_ - 1 );
        }

        if( x_ < dim_x - 1 ) {
          q.push( x_ + 1 );
          q.push( y_ );
          q.push( z_ );
        }

        if( y_ < dim_y - 1 ) {
          q.push( x_ );
          q.push( y_ + 1 );
          q.push( z_ );
        }

        if( z_ < dim_z - 1 ) {
          q.push( x_ );
          q.push( y_ );
          q.push( z_ + 1 );
        }

      }

    }


  }

  return volume;
}
