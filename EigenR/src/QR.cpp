#include "EigenR.h"

/* QR ----------------------------------------------------------------------- */
template <typename Number>
std::vector<Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>> QR(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  const Eigen::HouseholderQR<
    Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    qr = M.householderQr();
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> R =
    qr.matrixQR().template triangularView<Eigen::Upper>();
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> Q =
    qr.householderQ();
  return {Q, R};
}

// [[Rcpp::export]]
Rcpp::List EigenR_QR_real(const Eigen::MatrixXd& M) {
  const std::vector<Eigen::MatrixXd> QRdecomp = QR<double>(M);
  return Rcpp::List::create(Rcpp::Named("Q") = QRdecomp[0],
                            Rcpp::Named("R") = QRdecomp[1]);
}

// [[Rcpp::export]]
Rcpp::List EigenR_QR_cplx(const Eigen::MatrixXd& Re,
                          const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const std::vector<Eigen::MatrixXcd> QRdecomp = QR<std::complex<double>>(M);
  return Rcpp::List::create(Rcpp::Named("Q") = cplxMatrixToList(QRdecomp[0]),
                            Rcpp::Named("R") = cplxMatrixToList(QRdecomp[1]));
}
