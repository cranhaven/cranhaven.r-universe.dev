// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::List cpp_weighted_knn(arma::mat x,
                            arma::mat query,
                            arma::vec weights,
                            const std::size_t k) {
  
  std::size_t nVars = weights.size();
  std::size_t nQuery = query.n_rows;
  
  arma::mat retDist(nQuery, k);
  arma::umat retOrder(nQuery, k);
  
  arma::colvec tmpDist(x.n_rows);
  tmpDist.fill(0.0);
  arma::uvec order(x.n_rows);
  
  for (std::size_t i=0;i<nQuery;++i) {
    /** Vectorized distance calculation for each element in query **/
    for (std::size_t j=0;j <nVars;++j) {
      tmpDist = tmpDist + abs(weights(j) * (x.col(j) - query(i, j)));
    }
    
    /** Order according to distance and extract k elements **/
    order = arma::sort_index(tmpDist, "ascend");
    for (std::size_t l=0; l<k;++l) {
      retDist(i, l) = tmpDist(order(l));
      retOrder(i, l) = order(l) + 1;
    }
    tmpDist.fill(0);
  }
  return Rcpp::List::create(
    Rcpp::Named("distance") = retDist,
    Rcpp::Named("order")    = retOrder
  );
}
