#ifndef EIGENRHEADER
#define EIGENRHEADER

#include <RcppEigen.h>
#include <unsupported/Eigen/MatrixFunctions>
// [[Rcpp::depends(RcppEigen)]]

Eigen::MatrixXcd matricesToMatrixXcd(const Eigen::MatrixXd& Re,
                                     const Eigen::MatrixXd& Im);

Eigen::VectorXcd vectorsToVectorXcd(const Eigen::VectorXd& Re,
                                    const Eigen::VectorXd& Im);

Rcpp::List cplxMatrixToList(const Eigen::MatrixXcd& M);

Rcpp::List cplxVectorToList(const Eigen::VectorXcd& V);

Rcpp::ComplexVector cplxMatrixToRcpp(const Eigen::MatrixXcd& M);

Rcpp::NumericMatrix dblMatrixToRcpp(const Eigen::MatrixXd& M);

Eigen::SparseMatrix<double> realSparseMatrix(const std::vector<size_t>& i,
                                             const std::vector<size_t>& j,
                                             const std::vector<double>& Mij,
                                             const size_t nrows,
                                             const size_t ncols);

Eigen::SparseMatrix<std::complex<double>> cplxSparseMatrix(
    const std::vector<size_t>& i,
    const std::vector<size_t>& j,
    const std::vector<std::complex<double>>& Mij,
    const size_t nrows,
    const size_t ncols);

#endif
