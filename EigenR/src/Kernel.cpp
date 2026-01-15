#include "EigenR.h"

/* kernel COD --------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> kernel_COD(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  // https://stackoverflow.com/a/53598471/1100107
  Eigen::CompleteOrthogonalDecomposition<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    cod(M);
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> P =
    cod.colsPermutation();
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> V =
    cod.matrixZ().transpose();
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> Kernel =
    P * V.rightCols(V.cols() - cod.rank());
  return Kernel;
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_kernel_COD_real(const Eigen::MatrixXd& M) {
  return kernel_COD<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_kernel_COD_cplx(const Eigen::MatrixXd& Re,
                                  const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Kernel = kernel_COD<std::complex<double>>(M);
  return cplxMatrixToList(Kernel);
}

/* kernel LU ---------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> kernel_LU(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  const Eigen::FullPivLU<Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    lu(M);
  return lu.kernel();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_kernel_LU_real(const Eigen::MatrixXd& M) {
  return kernel_LU<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_kernel_LU_cplx(const Eigen::MatrixXd& Re,
                                 const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Kernel = kernel_LU<std::complex<double>>(M);
  return cplxMatrixToList(Kernel);
}

/* kernel dimension --------------------------------------------------------- */
template <typename Number>
unsigned kernelDimension(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  Eigen::CompleteOrthogonalDecomposition<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    cod(M);
  return cod.dimensionOfKernel();
}

// [[Rcpp::export]]
unsigned EigenR_kernelDimension_real(const Eigen::MatrixXd& M) {
  return kernelDimension<double>(M);
}

// [[Rcpp::export]]
unsigned EigenR_kernelDimension_cplx(const Eigen::MatrixXd& Re,
                                     const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  return kernelDimension<std::complex<double>>(M);
}
