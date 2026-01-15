#include "EigenR.h"

/* image LU ----------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> image_LU(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  const Eigen::FullPivLU<Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    lu(M);
  return lu.image(M);
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_image_LU_real(const Eigen::MatrixXd& M) {
  return image_LU<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_image_LU_cplx(const Eigen::MatrixXd& Re,
                                const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Image = image_LU<std::complex<double>>(M);
  return cplxMatrixToList(Image);
}

/* image QR ----------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> image_QR(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  const Eigen::ColPivHouseholderQR<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    qr = M.colPivHouseholderQr();
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> Q =
    qr.householderQ().setLength(qr.nonzeroPivots());
  return Q.leftCols(qr.rank());
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_image_QR_real(const Eigen::MatrixXd& M) {
  return image_QR<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_image_QR_cplx(const Eigen::MatrixXd& Re,
                                const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Image = image_QR<std::complex<double>>(M);
  return cplxMatrixToList(Image);
}

/* image COD ---------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> image_COD(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  Eigen::CompleteOrthogonalDecomposition<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    cod(M);
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> Q =
    cod.householderQ();
  return Q.leftCols(cod.rank());
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_image_COD_real(const Eigen::MatrixXd& M) {
  return image_COD<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_image_COD_cplx(const Eigen::MatrixXd& Re,
                                 const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Image = image_COD<std::complex<double>>(M);
  return cplxMatrixToList(Image);
}
