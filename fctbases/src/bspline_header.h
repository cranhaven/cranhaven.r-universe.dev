# ifndef _bspline_header
# define _bspline_header

#include <RcppArmadillo.h>
#include "bspline.h"
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
SEXP init_bspline(int spline_order, const arma::vec& spline_knots) {
  
  if (spline_order < 1) stop("Spline order must be strictly positive!");

  bspline* bs = new bspline(spline_order, spline_knots);
  XPtr<bspline> bs_ptr(bs, true);  
  return bs_ptr;
  
};

//[[Rcpp::export]]
SEXP init_bspline_u4(double e_left, double e_right, int n_intervals) {
  
  if (n_intervals < 3) stop("not good..");
  if (e_left >= e_right) stop ("left end point must be smaller than right end point!");
  
  vec knots(n_intervals + 1);
  for (int i=0; i <= n_intervals; i++) {
    knots(i) = (e_left*(n_intervals -i) + e_right*i) / n_intervals;
  }
  
  bspline_u4* bs = new bspline_u4(knots);
  XPtr<bspline_u4> bs_ptr(bs, true);  
  return bs_ptr;
  
};


# endif
