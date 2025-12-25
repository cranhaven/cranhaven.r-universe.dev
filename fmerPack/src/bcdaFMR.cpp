#include <RcppArmadillo.h>
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat bcdaRcpp(arma::vec y, arma::mat X, int n, int p, int m, arma::mat B_Start, arma::mat pij, arma::vec rho, 
             double lambda, arma::mat w, 
             double eps = 1e-6, int maxit = 200, double mu = 1) {
  // initial values for Bregman iteration
  // double mu = 1;
  
  arma::mat Bs_0 = B_Start;
  arma::vec b_0(p, arma::fill::zeros);
  // get crossprod yx and xx
  arma::mat yx = trans(pij.each_col() % y) * X;
  arma::cube xx(p, p, m);
  for(int i = 0;i < m;i++) {
    xx.slice(i) = trans(X.each_col() % pij.col(i)) * X;
  }
  // Bregman iteration for solving constraint lasso problem
  bool conv = false;
  int iter = 0;
  // define variables in loop
  arma::mat Bs_1;
  double t;
  arma::mat xxk;
  arma::rowvec xxkj;
  while((! conv) && iter < maxit) {
    Bs_1 = Bs_0;
    // update beta0
    for(int k = 0; k < p; k++) {
      Bs_1(k, 0) = 0;
      xxk = xx(span(k), span(), span());
      if(m == 1) {
        t = sum(rho % yx.col(k)) - as_scalar(xxk * (Bs_1.cols(1, m).each_col() + Bs_1.col(0)));
      } else {
        t = sum(rho % yx.col(k)) - accu(xxk % (Bs_1.cols(1, m).each_col() + Bs_1.col(0)));
      }
      if(t > n * w(k, 0) * lambda) {
        Bs_1(k, 0) = (t - n * w(k, 0) * lambda) / accu(xx(span(k), span(k), span()));
      }
      if(t < - n * w(k, 0) * lambda) {
        Bs_1(k, 0) = (t + n * w(k, 0) * lambda) / accu(xx(span(k), span(k), span()));
      }
    }
    // update betaj
    for(int j = 1; j <= m; j++) {
      for(int k = 0; k < p; k++) {
        Bs_1(k, j) = 0;
        xxkj = xx(span(k), span(), span(j - 1));
        t = rho(j - 1) * yx(j - 1, k) - 
            as_scalar(xxkj * (Bs_1.col(j) + Bs_1.col(0))) -
            as_scalar(mu * sum(Bs_1(k, span(1, m))) - b_0(k));
        if(t > n * w(k, j) * lambda) {
          Bs_1(k, j) = (t - n * w(k, j) * lambda) / (xx(k, k, j - 1) + mu);
        }
        if(t < - n * w(k, j) * lambda) {
          Bs_1(k, j) = (t + n * w(k, j) * lambda) / (xx(k, k, j - 1) + mu);
        }
      }
    }
    if(norm(Bs_1 - Bs_0) < (norm(Bs_0) * eps + eps)) {
      conv = true;
    }
    // update initial values for next iteration
    b_0 = b_0 - sum(Bs_1.cols(1, m), 1) * mu;
    Bs_0 = Bs_1;
    mu = mu * 1.1;
    iter++;
  }
  return Bs_0;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat postProb(int n, int m, arma::vec pi_0, arma::vec rho_0, arma::vec y, arma::mat X, arma::mat phi) {
  arma::mat out = exp(-square(trans(repmat(rho_0, 1, n)) % repmat(y, 1, m) - X * phi) * 0.5) % trans(repmat(pi_0 % rho_0, 1, n));
  return out.each_col() / sum(out, 1);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec estRho(int n, int m, arma::mat pij, arma::vec y, arma::mat X, arma::mat phi, bool equal_var) {
  arma::mat ytXt = trans(pij.each_col() % y) * (X * phi);
  arma::vec yy = trans(sum(pij.each_col() % square(y), 0));
  arma::vec rho(m);
  if(!equal_var) {
    // assuming unequal variances
    rho = (ytXt.diag() + sqrt(square(ytXt.diag()) + 4 *  yy % trans(sum(pij, 0)))) / (2 * yy);
  } else {
    // assuming equal variance
    double t = as_scalar(trace(ytXt) + sqrt(square(sum(ytXt.diag(), 0)) + 4 *  sum(yy) * n)) / (2 * sum(yy));
    rho.fill(t);
  }
  return rho;
}
