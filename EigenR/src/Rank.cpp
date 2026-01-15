#include "EigenR.h"

/* rank --------------------------------------------------------------------- */
template <typename Number>
unsigned rank(const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.colPivHouseholderQr().rank();
}

// [[Rcpp::export]]
unsigned EigenR_rank_real(const Eigen::MatrixXd& M) {
  return rank<double>(M);
}

// [[Rcpp::export]]
unsigned EigenR_rank_cplx(const Eigen::MatrixXd& Re,
                          const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  return rank<std::complex<double>>(M);
}
