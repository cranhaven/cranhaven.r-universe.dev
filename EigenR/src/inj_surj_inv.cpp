#include "EigenR.h"

/* injective, surjective, invertible ---------------------------------------- */
template <typename Number>
bool isInjective(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  Eigen::CompleteOrthogonalDecomposition<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    cod(M);
  return cod.isInjective();
}

// [[Rcpp::export]]
bool EigenR_isInjective_real(const Eigen::MatrixXd& M) {
  return isInjective<double>(M);
}

// [[Rcpp::export]]
bool EigenR_isInjective_cplx(const Eigen::MatrixXd& Re,
                             const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  return isInjective<std::complex<double>>(M);
}

template <typename Number>
bool isSurjective(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  Eigen::CompleteOrthogonalDecomposition<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    cod(M);
  return cod.isSurjective();
}

// [[Rcpp::export]]
bool EigenR_isSurjective_real(const Eigen::MatrixXd& M) {
  return isSurjective<double>(M);
}

// [[Rcpp::export]]
bool EigenR_isSurjective_cplx(const Eigen::MatrixXd& Re,
                              const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  return isSurjective<std::complex<double>>(M);
}

template <typename Number>
bool isInvertible(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  Eigen::CompleteOrthogonalDecomposition<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    cod(M);
  return cod.isInvertible();
}

// [[Rcpp::export]]
bool EigenR_isInvertible_real(const Eigen::MatrixXd& M) {
  return isInvertible<double>(M);
}

// [[Rcpp::export]]
bool EigenR_isInvertible_cplx(const Eigen::MatrixXd& Re,
                              const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  return isInvertible<std::complex<double>>(M);
}
