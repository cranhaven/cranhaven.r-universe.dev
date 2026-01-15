#include "EigenR.h"

/* -------------------------------------------------------------------------- */
Eigen::MatrixXcd matricesToMatrixXcd(const Eigen::MatrixXd& Re,
                                     const Eigen::MatrixXd& Im) {
  const std::complex<double> I_{0.0, 1.0};
  return Re.cast<std::complex<double>>() + I_ * Im.cast<std::complex<double>>();
}

Eigen::VectorXcd vectorsToVectorXcd(const Eigen::VectorXd& Re,
                                    const Eigen::VectorXd& Im) {
  const std::complex<double> I_{0.0, 1.0};
  return Re.cast<std::complex<double>>() + I_ * Im.cast<std::complex<double>>();
}

Rcpp::List cplxMatrixToList(const Eigen::MatrixXcd& M) {
  return Rcpp::List::create(Rcpp::Named("real") = M.real(),
                            Rcpp::Named("imag") = M.imag());
}

Rcpp::List cplxVectorToList(const Eigen::VectorXcd& V) {
  return Rcpp::List::create(Rcpp::Named("real") = V.real(),
                            Rcpp::Named("imag") = V.imag());
}

/* -------------------------------------------------------------------------- */
/*
 Rcpp::List cplxRcppMatrixToList(const Rcpp::ComplexMatrix M) {
 Rcpp::NumericMatrix realPart(M.nrow(), M.ncol());
 Rcpp::NumericMatrix imagPart(M.nrow(), M.ncol());
 for(auto i = 0; i < M.nrow(); i++) {
 for(auto j = 0; j < M.ncol(); j++) {
 const std::complex<double> z = M(i, j);
 realPart(i, j) = real(z);
 imagPart(i, j) = imag(z);
 }
 }
 return Rcpp::List::create(Rcpp::Named("real") = realPart,
 Rcpp::Named("imag") = imagPart);
 }
 */

Rcpp::ComplexVector cplxMatrixToRcpp(const Eigen::MatrixXcd& M) {
  Eigen::MatrixXd Mreal = M.real();
  Eigen::MatrixXd Mimag = M.imag();
  SEXP MrealS = Rcpp::wrap(Mreal);
  SEXP MimagS = Rcpp::wrap(Mimag);
  Rcpp::NumericMatrix outReal(MrealS);
  Rcpp::NumericMatrix outImag(MimagS);
  Rcpp::ComplexMatrix outRealCplx(outReal);
  Rcpp::ComplexMatrix outImagCplx(outImag);
  Rcomplex I;
  I.r = 0.0;
  I.i = 1.0;
  Rcpp::ComplexVector out = outRealCplx + I * outImagCplx;
  out.attr("dim") = Rcpp::Dimension(M.rows(), M.cols());
  return out;
}

Rcpp::NumericMatrix dblMatrixToRcpp(const Eigen::MatrixXd& M) {
  SEXP Ms = Rcpp::wrap(M);
  Rcpp::NumericMatrix out(Ms);
  return out;
}
