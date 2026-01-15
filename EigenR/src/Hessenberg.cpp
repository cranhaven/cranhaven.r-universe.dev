#include "EigenR.h"

// [[Rcpp::export]]
Rcpp::List EigenR_Hessenberg_real(const Eigen::MatrixXd& M) {
  Eigen::HessenbergDecomposition<Eigen::MatrixXd> hd(M.rows());
  hd.compute(M);
  const Eigen::MatrixXd H = hd.matrixH();
  const Eigen::MatrixXd Q = hd.matrixQ();
  return Rcpp::List::create(Rcpp::Named("H") = H,
                            Rcpp::Named("Q") = Q);
}

// [[Rcpp::export]]
Rcpp::List EigenR_Hessenberg_cplx(const Eigen::MatrixXd& Re, 
                                  const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  Eigen::HessenbergDecomposition<Eigen::MatrixXcd> hd(M.rows());
  hd.compute(M);
  const Eigen::MatrixXcd H = hd.matrixH();
  const Eigen::MatrixXcd Q = hd.matrixQ();
  return Rcpp::List::create(Rcpp::Named("H") = cplxMatrixToList(H),
                            Rcpp::Named("Q") = cplxMatrixToList(Q));
}
