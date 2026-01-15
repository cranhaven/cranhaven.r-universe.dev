#include "EigenR.h"

/* inverse ------------------------------------------------------------------ */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> inverse(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  const Eigen::FullPivLU<Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    lu(M);
  if(lu.isInvertible()) {
    return lu.inverse();
  } else {
    throw Rcpp::exception("The matrix is not invertible.");
  }
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_inverse_real(const Eigen::MatrixXd& M) {
  return inverse<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_inverse_cplx(const Eigen::MatrixXd& Re,
                               const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Minv = inverse<std::complex<double>>(M);
  return cplxMatrixToList(Minv);
}

/* pseudo-inverse ----------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> pseudoInverse(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  Eigen::CompleteOrthogonalDecomposition<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    cod(M);
  return cod.pseudoInverse();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_pseudoInverse_real(const Eigen::MatrixXd& M) {
  return pseudoInverse<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_pseudoInverse_cplx(const Eigen::MatrixXd& Re,
                                     const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd pinvM = pseudoInverse<std::complex<double>>(M);
  return cplxMatrixToList(pinvM);
}
