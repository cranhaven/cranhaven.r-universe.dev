#include "skm_gdp.h"
#include "matrix_minmax.h"


// //' skm_gdp_cpp
// //' @description
// //'  solve selective kmeans via a greedy propagation.
// //' @details
// //' skm_gdp_cpp init with an input m x n matrix x and want to select an index set s
// //'  of size k from x row index started from 0 such that
// //'
// //'  minimize sum(min(x.subview(i in s, all j), min over all i), sum over all j)
// //'
// //' skm_gdp_cpp solve the problem with greedy propagation via selecting the current
// //'  best addon index from the index set left, addon index is defined as such index
// //'  when addon to the selected one can bring the most improvement.
// //'
// //' since skm_gbp_cpp would select index one by one, and no return, e.g., if select
// //'  index A for k = 1, then selection on k = 2 would build on k = 1, so index A is
// //'  always present in the solution, so all index can be ranked w.r.t when it would
// //'  be considered as the best addon.
// //' as a result skm_gbp_cpp a parameter k is not always required, so default k = 0
// //'  will resturn a vector of size m, and user can select to top k as solution for k.
// //' @param x
// //'  x: m x n matrix: s - t - dist
// //' @return s
// //'  s: a ranked index 0 - m - 1 where the top k would
// //'   minimize sum(min(x.subview(i in s(0..k-1), all j), min over all i), sum over all j)
// //' @family skm_gdp
// //' @export
// // [[Rcpp::export]]
arma::uvec skm_gdp_cpp(arma::mat x, arma::uword k) {

  // init
  if (k == 0 || k > x.n_rows) { k = x.n_rows; }

  // init
  arma::uvec s = arma::zeros<arma::uvec>(k);

  arma::uvec ulmt = arma::linspace<arma::uvec>(0, x.n_rows - 1, x.n_rows);

  arma::uvec vlmt = arma::linspace<arma::uvec>(0, x.n_cols - 1, x.n_cols);

  arma::rowvec cmin = arma::min(x, 0);

  arma::rowvec xs;

  arma::uword i; arma::vec rs; arma::uword id; arma::uvec cs;

  for (i = 0; i < k; i++) {

    Rcpp::Rcout << "skm_gdp_cpp: optimize at it <" << i << "> ..." << std::endl;

    if ( vlmt.size() > 0 ) {

      // rs: rowsum for finding current best addon index rs.min(current-best-addon-index)
      rs = arma::sum(x, 1); rs.min(id);

      // Rcpp::Rcout << "skm_gdp_cpp: optimize at it <" << i << "> ... update rs: " << std::endl << rs.t() << std::endl;

      s(i) = ulmt(id); xs = x.row(id);

      // Rcpp::Rcout << "skm_gdp_cpp: optimize at it <" << i << "> ... update s: " << std::endl << s.t() << std::endl;

      // update x to reflect s(i) is selected into index set
      for (arma::uword i1 = 0; i1 < x.n_rows; i1++) {
        x.row(i1) = arma::min(x.row(i1), xs); // so x(i', j) = min(x(i', j), x(s(i), j)) for all i' and j
      }

      // Rcpp::Rcout << "skm_gdp_cpp: optimize at it <" << i << "> ... update x: " << std::endl << x << std::endl;

      // update ulmt - active row index set
      // row index i is active if i is not in s yet
      ulmt = ulmt(arma::find(ulmt != s(i)));

      // Rcpp::Rcout << "skm_gdp_cpp: optimize at it <" << i << "> ... update ulmt: " << std::endl << ulmt.t() << std::endl;

      // update vlmt - active col index set
      // col index j is active if x(i in s, j) > min(x(i, j), all i)
      vlmt = vlmt(arma::find(cmin != xs));

      // Rcpp::Rcout << "skm_gdp_cpp: optimize at it <" << i << "> ... update vlmt: " << std::endl << vlmt.t() << std::endl;

      // shel_col must loop reverse because when shed col 1 then col 2 become col 1 loop advance will cause error
      x.shed_row(id); cs = arma::find(cmin == xs); for (arma::uword i2 = cs.size(); i2 > 0; i2--) {x.shed_col(cs(i2 - 1)); }

      cmin = arma::trans(cmin(arma::find(cmin != xs)));

    } else {

      // Rcpp::Rcout << "skm_gdp_cpp: optimize at it <" << i << "> ... break ..." << std::endl;

      break;

    }

  }

  if ( i < k ) {

    s.tail(k - i) = ulmt.head(k - i);

  }

  return s;
}

