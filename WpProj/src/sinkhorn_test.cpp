#include "WpProj_types.h"

// [[Rcpp::export]]
Rcpp::List sinkhorn_(Rcpp::NumericVector p_, Rcpp::NumericVector q_, Rcpp::NumericMatrix cost_matrix_,
                  double epsilon, int niterations){
  // compute distance between p and q
  // p corresponds to the weights of a N-sample
  // each q corresponds to the weights of a M-sample
  // Thus cost_matrix must be a N x M cost matrix
  // epsilon is a regularization parameter, equal to 1/lambda in some references
  int N = p_.size();
  int M = q_.size();
  
  matMap q(Rcpp::as<matMap >(q_));
  matMap cost_matrix(Rcpp::as<matMap >(cost_matrix_));
  vecMap p(Rcpp::as<vecMap >(p_));
  // avoid to take exp(k) when k is less than -500,
  // as K then contains zeros, and then the upcoming computations divide by zero
  matrix K = (cost_matrix.array() * (-1./epsilon)).exp(); // K =  exp(- M / epsilon)
  
  // matrix K = (cost_matrix.array() * (-1./epsilon)); // K =  exp(- M / epsilon)
  // for (int i = 0; i < N; i++){
  // for (int j = 0; j < M; j++){
  //     if (K(i,j) < -500){
  //       K(i,j) = exp(-500);
  //     } else {
  // K(i,j) = exp(K(i,j));
  //     }
  // }
  // }
  
  matrix K_tilde = p.array().inverse().matrix().asDiagonal() * K; // diag(1/p) %*% K
  matrix u = vector::Constant(N, 1./double(N));
  for (int iteration = 0; iteration < niterations; iteration ++){
    // cerr << iteration << endl;
    //  u is set to 1 / (K_tilde %*% (qs / (K.transpose() %*% u)))
    u = 1. / (K_tilde * (q.array() / (K.transpose() * u).array()).matrix()).array();
    // for (int i = 0; i < N; i ++) cerr << u(i,0) << endl;
  }
  matrix v = q.array() / (K.transpose() * u).array();
  // compute the optimal transport matrix between p and the first q
  matrix transportmatrix = u.col(0).asDiagonal() * K * v.col(0).asDiagonal();
  matrix uXIv = u.array() * ((K.array() * cost_matrix.array()).matrix() * v).array();
  Rcpp::NumericVector d = Rcpp::wrap(uXIv.colwise().sum());
  return Rcpp::List::create(Rcpp::Named("distances")=d,
                            Rcpp::Named("transportmatrix") = Rcpp::wrap(transportmatrix),
                            Rcpp::Named("u") = Rcpp::wrap(u),
                            Rcpp::Named("v") = Rcpp::wrap(v));
}
