#include "EigenR.h"

/* Least-squares SVD -------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> lsSolve(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& A,
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& b) {
  return A.bdcSvd(Eigen::ComputeThinU | Eigen::ComputeThinV).solve(b);
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_lsSolve_real(const Eigen::MatrixXd& A,
                                    const Eigen::MatrixXd& b) {
  return lsSolve<double>(A, b);
}

// [[Rcpp::export]]
Rcpp::List EigenR_lsSolve_cplx(const Eigen::MatrixXd& ReA,
                               const Eigen::MatrixXd& ImA,
                               const Eigen::MatrixXd& Reb,
                               const Eigen::MatrixXd& Imb) {
  const Eigen::MatrixXcd A = matricesToMatrixXcd(ReA, ImA);
  const Eigen::MatrixXcd b = matricesToMatrixXcd(Reb, Imb);
  const Eigen::MatrixXcd X = lsSolve<std::complex<double>>(A, b);
  return cplxMatrixToList(X);
}

/* Least-squares COD -------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> lsSolve_cod(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& A,
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& b) {
  Eigen::CompleteOrthogonalDecomposition<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    cod(A);
  return cod.solve(b);
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_lsSolve_cod_real(const Eigen::MatrixXd& A,
                                        const Eigen::MatrixXd& b) {
  return lsSolve_cod<double>(A, b);
}

// [[Rcpp::export]]
Rcpp::List EigenR_lsSolve_cod_cplx(const Eigen::MatrixXd& ReA,
                                   const Eigen::MatrixXd& ImA,
                                   const Eigen::MatrixXd& Reb,
                                   const Eigen::MatrixXd& Imb) {
  const Eigen::MatrixXcd A = matricesToMatrixXcd(ReA, ImA);
  const Eigen::MatrixXcd b = matricesToMatrixXcd(Reb, Imb);
  const Eigen::MatrixXcd X = lsSolve_cod<std::complex<double>>(A, b);
  return cplxMatrixToList(X);
}
