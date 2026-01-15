#include "EigenR.h"

/* determinant -------------------------------------------------------------- */
template <typename Number>
Number determinant(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  return M.determinant();
}

template <typename Number>
Number determinant_sparse(Eigen::SparseMatrix<Number>& M) {
  Eigen::SparseLU<Eigen::SparseMatrix<Number>> solver;
  M.makeCompressed();
  solver.analyzePattern(M);
  solver.factorize(M);
  if(solver.info() != Eigen::Success) {
    throw Rcpp::exception("LU factorization has failed.");
  }
  return solver.determinant();
}

// [[Rcpp::export]]
double EigenR_det_real(const Eigen::MatrixXd& M) {
  return determinant<double>(M);
}

// [[Rcpp::export]]
std::complex<double> EigenR_det_cplx(const Eigen::MatrixXd& Re,
                                     const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  return determinant<std::complex<double>>(M);
}

// [[Rcpp::export]]
double EigenR_det_sparse_real(const std::vector<size_t>& i,
                              const std::vector<size_t>& j,
                              const std::vector<double>& Mij,
                              const size_t nrows,
                              const size_t ncols) {
  Eigen::SparseMatrix<double> M = realSparseMatrix(i, j, Mij, nrows, ncols);
  return determinant_sparse<double>(M);
}

// [[Rcpp::export]]
std::complex<double> EigenR_det_sparse_cplx(
    const std::vector<size_t>& i,
    const std::vector<size_t>& j,
    const std::vector<std::complex<double>>& Mij,
    const size_t nrows,
    const size_t ncols) {
  Eigen::SparseMatrix<std::complex<double>> M =
    cplxSparseMatrix(i, j, Mij, nrows, ncols);
  return determinant_sparse<std::complex<double>>(M);
}

// [[Rcpp::export]]
double EigenR_absdet(const Eigen::MatrixXd& M) {
  Eigen::CompleteOrthogonalDecomposition<Eigen::MatrixXd> cod(M);
  return cod.absDeterminant();
}

// [[Rcpp::export]]
double EigenR_logabsdet(const Eigen::MatrixXd& M) {
  Eigen::CompleteOrthogonalDecomposition<Eigen::MatrixXd> cod(M);
  return cod.logAbsDeterminant();
}
