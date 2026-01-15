#include "EigenR.h"

/* Sparse stuff ------------------------------------------------------------- */
Eigen::SparseMatrix<double> realSparseMatrix(const std::vector<size_t>& i,
                                             const std::vector<size_t>& j,
                                             const std::vector<double>& Mij,
                                             const size_t nrows,
                                             const size_t ncols) {
  Eigen::SparseMatrix<double> out(nrows, ncols);
  out.reserve(Mij.size());
  for(auto k = 0; k < i.size(); k++) {
    out.insert(i[k], j[k]) = Mij[k];
  }
  return out;
}

Eigen::SparseMatrix<std::complex<double>> cplxSparseMatrix(
    const std::vector<size_t>& i,
    const std::vector<size_t>& j,
    const std::vector<std::complex<double>>& Mij,
    const size_t nrows,
    const size_t ncols) {
  Eigen::SparseMatrix<std::complex<double>> out(nrows, ncols);
  out.reserve(Mij.size());
  for(auto k = 0; k < i.size(); k++) {
    out.insert(i[k], j[k]) = Mij[k];
  }
  return out;
}
