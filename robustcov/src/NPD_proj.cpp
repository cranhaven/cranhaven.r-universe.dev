// nearest patterned positive definite matrix as in (Higham, 2002), 
// adopted from https://github.com/Julius-V/nearPPSD

#include "NPD_proj.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

mat ProjPattern(mat X, const mat X0, const vec un) {
  uvec idx;
  for (int i = 0; i < un.size(); i++) {
    idx = find(X0 == un(i));
    X.elem(idx).fill(mean(X.elem(idx)));
  }
  X.elem(find(X0 == 0)).fill(0);
  return X;
}

mat ProjPSD(const mat R, const int n, const float eigenTol) {
  vec eigval;
  mat eigvec;
  eig_sym(eigval, eigvec, R);
  uvec p = find(eigval > eigenTol * eigval(n - 1));
  eigvec = eigvec.cols(p);
  return eigvec * diagmat(eigval(p)) * eigvec.t();
}

//' @name nearPPSD
//' @title nearest positive semi-definite projection of a matrix
//' @description This routine calculate the nearest positive semi0definite projection
//' @param X the matrix
//' @param eigenTol tolerance in eigen system, used in finding nearest positive matrix
//' @param convTol tolerance in cov, used in finding nearest positive matrix
//' @param psdTol tolerance in psd, used in finding nearest positive matrix
//' @param maxit max iterations in finding nearest positive matrix
//' @return a matrix which is the nearest positive semi-definite matrix of input X
//' @export
// [[Rcpp::export]]
arma::mat nearPPSD(arma::mat X, const float eigenTol = 1e-06, const float convTol = 1e-07, 
             const float psdTol = 1e-08, const int maxit = 1000) {
  const int n = X.n_cols;
  const mat X0 = X;
  mat Y = X;
  mat DS(n, n, fill::zeros);
  mat R;
  int iter = 0;
  bool converged;
  uvec idx;
  vec un = unique(X0);
  do {
    R = Y - DS;
    X = ProjPSD(R, n, eigenTol);
    Y = ProjPattern(X, X0, un);
    DS = X - R;
    iter++;
    converged = (norm(Y - X) / norm(Y)) <= convTol;
  } while (iter < maxit && (!converged || min(eig_sym(X)) < 0));
  
  if (!converged)
    warning("'nearPPSD' did not converge in %d iterations", iter);
  return X;
}
