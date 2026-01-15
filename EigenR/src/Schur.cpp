#include "EigenR.h"

// [[Rcpp::export]]
Rcpp::List EigenR_realSchur(const Eigen::MatrixXd& M) {
  Eigen::RealSchur<Eigen::MatrixXd> schur(M); 
  const Eigen::MatrixXd U = schur.matrixU();
  const Eigen::MatrixXd T = schur.matrixT();
  return Rcpp::List::create(Rcpp::Named("T") = T,
                            Rcpp::Named("U") = U);
}

// [[Rcpp::export]]
Rcpp::List EigenR_complexSchur(const Eigen::MatrixXd& Re, 
                               const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  Eigen::ComplexSchur<Eigen::MatrixXcd> schur(M.rows());
  schur.compute(M);
  const Eigen::MatrixXcd U = schur.matrixU();
  const Eigen::MatrixXcd T = schur.matrixT();
  return Rcpp::List::create(Rcpp::Named("U") = cplxMatrixToList(U),
                            Rcpp::Named("T") = cplxMatrixToList(T));
}
