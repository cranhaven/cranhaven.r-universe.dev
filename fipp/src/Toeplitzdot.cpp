#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
arma::vec logTopdot(
    arma::vec ln_x,
    arma::vec ln_y){
  int r = ln_x.n_elem;
  arma::vec out(r);
  auto lambda = [&](double a, double b){return std::max(a,b) + std::log1p(std::exp(-std::abs(a-b))); };
  int i;
  for(i = 0; i<r; i++){
    arma::vec rowi = arma::vectorise(ln_x(arma::span(0,(r-i-1))));
    arma::vec c_k_i = rowi + ln_y(arma::span((i+1),r));
    c_k_i = c_k_i.elem(find_finite(c_k_i));
    //c_k_i.t().print();
    out(i) =  std::accumulate(c_k_i.begin(), c_k_i.end(), -arma::datum::inf, lambda);
  }
  return out;
}

