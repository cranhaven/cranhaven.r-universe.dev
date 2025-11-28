#ifndef __SKM_GDP__
#define __SKM_GDP__


#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


//' skm_gdp_cpp
//' @description
//'  solve selective kmeans via a greedy propagation.
//' @details
//' skm_gdp_cpp init with an input m x n matrix x and want to select an index set s
//'  of size k from x row index started from 0 such that
//'
//'  minimize sum(min(x.subview(i in s, all j), min over all i), sum over all j)
//'
//' skm_gdp_cpp solve the problem with greedy propagation via selecting the current
//'  best addon index from the index set left, addon index is defined as such index
//'  when addon to the selected one can bring the most improvement.
//'
//' since skm_gbp_cpp would select index one by one, and no return, e.g., if select
//'  index A for k = 1, then selection on k = 2 would build on k = 1, so index A is
//'  always present in the solution, so all index can be ranked w.r.t when it would
//'  be considered as the best addon.
//' as a result skm_gbp_cpp a parameter k is not always required, so default k = 0
//'  will resturn a vector of size m, and user can select to top k as solution for k.
//' @param x
//'  an m x n matrix of s - t - dist
//' @param k
//'  number of index to be selected from x row index start from 0.
//' @return s
//'  a ranked index 0 - m - 1 where the top k would
//'   minimize sum(min(x.subview(i in s(0..k-1), all j), min over all i), sum over all j)
//' @family skm_gdp
//' @export
// [[Rcpp::export]]
arma::uvec skm_gdp_cpp(arma::mat x, arma::uword k = 0);


#endif // __SKM__
