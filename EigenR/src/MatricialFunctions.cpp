#include "EigenR.h"

/* exponential -------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> expm(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.exp();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_exp_real(const Eigen::MatrixXd& M) {
  return expm<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_exp_cplx(const Eigen::MatrixXd& Re,
                           const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Mexp = expm<std::complex<double>>(M);
  return cplxMatrixToList(Mexp);
}

/* logarithm ---------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> logm(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.log();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_log_real(const Eigen::MatrixXd& M) {
  return logm<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_log_cplx(const Eigen::MatrixXd& Re,
                           const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Mlog = logm<std::complex<double>>(M);
  return cplxMatrixToList(Mlog);
}

/* cosine ------------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> cosm(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.cos();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_cos_real(const Eigen::MatrixXd& M) {
  return cosm<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_cos_cplx(const Eigen::MatrixXd& Re,
                           const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Mcos = cosm<std::complex<double>>(M);
  return cplxMatrixToList(Mcos);
}

/* sine --------------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> sinm(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.sin();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_sin_real(const Eigen::MatrixXd& M) {
  return sinm<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_sin_cplx(const Eigen::MatrixXd& Re,
                           const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Msin = sinm<std::complex<double>>(M);
  return cplxMatrixToList(Msin);
}

/* hyperbolic cosine -------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> coshm(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.cosh();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_cosh_real(const Eigen::MatrixXd& M) {
  return coshm<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_cosh_cplx(const Eigen::MatrixXd& Re,
                            const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Mcosh = coshm<std::complex<double>>(M);
  return cplxMatrixToList(Mcosh);
}

/* hyperbolic sine ---------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> sinhm(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.sinh();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_sinh_real(const Eigen::MatrixXd& M) {
  return sinhm<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_sinh_cplx(const Eigen::MatrixXd& Re,
                            const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Msinh = sinhm<std::complex<double>>(M);
  return cplxMatrixToList(Msinh);
}

/* power -------------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> powm(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M,
    const Number& p) {
  return M.pow(p);
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_pow_real(const Eigen::MatrixXd& M, const double& p) {
  return powm<double>(M, p);
}

// [[Rcpp::export]]
Rcpp::List EigenR_pow_cplx(const Eigen::MatrixXd& Re,
                           const Eigen::MatrixXd& Im,
                           const std::complex<double>& p) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Mpow = powm<std::complex<double>>(M, p);
  return cplxMatrixToList(Mpow);
}

/* square root -------------------------------------------------------------- */
template <typename Number>
Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> sqrtm(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.sqrt();
}

// [[Rcpp::export]]
Eigen::MatrixXd EigenR_sqrt_real(const Eigen::MatrixXd& M) {
  return sqrtm<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_sqrt_cplx(const Eigen::MatrixXd& Re,
                            const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Eigen::MatrixXcd Msqrt = sqrtm<std::complex<double>>(M);
  return cplxMatrixToList(Msqrt);
}
