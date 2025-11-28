#ifndef __MATRIX_MINMAX__
#define __MATRIX_MINMAX__


#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;


//' col_min_idx
//' @description
//'  calculate colvec min value index within limited range
//' @param u
//'  u: a numeric colvec
//' @param wlmt
//'  wlmt: limit search on colvec on indices within wlmt
//' @return id
//'  an index of min value in u within wlmt w.r.t to original index
//' @note
//'   cpp use index start from 0 vs r use index start from 1
//' @note
//'   in case of equal std:min/std:max take first index seen
//' @family matrix_minmax
//' @export
// [[Rcpp::export]]
arma::uword col_min_idx(const arma::colvec& u, const arma::ucolvec& wlmt);


//' col_max_idx
//' @description
//'  calculate colvec max value index within limited range
//' @inheritParams col_min_idx
//' @return id
//'  an index of max value in u within wlmt w.r.t to original index
//' @note
//'  cpp use index start from 0 vs r use index start from 1
//' @note
//'  in case of equal std:min/std:max take first index seen
//' @family matrix_minmax
//' @export
// [[Rcpp::export]]
arma::uword col_max_idx(const arma::colvec& u, const arma::ucolvec& wlmt);


//' col_min_val
//' @description
//'  calculate colvec min value within limited range
//' @inheritParams col_min_idx
//' @return vd
//'  min value in u within wlmt w.r.t to original index
//' @family matrix_minmax
//' @export
// [[Rcpp::export]]
double col_min_val(const arma::colvec& u, const arma::ucolvec& wlmt);


//' col_max_val
//' @description
//'  calculate colvec max value within limited range
//' @inheritParams col_min_idx
//' @return vd
//'  min value in u within wlmt w.r.t to original index
//' @family matrix_minmax
//' @export
// [[Rcpp::export]]
double col_max_val(const arma::colvec& u, const arma::ucolvec& wlmt);


//' col_rgn_val
//' @description
//'  calculate colvec range = max - min value within limited range
//' @inheritParams col_min_idx
//' @return vd
//'  max - min value in u within wlmt w.r.t to original index
//' @family matrix_minmax
//' @export
// [[Rcpp::export]]
double col_rgn_val(const arma::colvec& u, const arma::ucolvec& wlmt);


#endif // __MATRIX_MINMAX__
