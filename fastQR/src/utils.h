#ifndef UTILS_H
#define UTILS_H

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Eigen/Core>

#include "QRdecomposition.h"
#include "support_functions.h"
#include "QRupdate.h"
#include "thinQRupdate.h"

#define EIGEN_USE_BLAS
#define EIGEN_USE_LAPACKE

#define DOUBLE_EPS 2.220446e-16
#define SAFE_LOG(a) (((a) <= 0.0) ? log(DOUBLE_EPS) : log(a))
#define SAFE_ZERO(a) ((a) == 0 ? DOUBLE_EPS : (a))
#define SQRT_DOUBLE_EPS sqrt(DOUBLE_EPS)

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppEigen)]]

using namespace Eigen;
//using namespace std;

void subtractone(int& val);

arma::uvec set_diff (arma::uvec x,
                     arma::uvec y);

Eigen::VectorXd vec2scalar_prod (const Eigen::VectorXd& y,
                                 const double& x);
Eigen::VectorXd mat2vec (const Eigen::MatrixXd& X);
Eigen::VectorXd symmat2vech (const Eigen::MatrixXd& X,
                             const bool byrow);
Eigen::VectorXd symmat2vech_byrow (const Eigen::MatrixXd& X,
                                   const int n,
                                   const int p);
Eigen::VectorXd symmat2vech_bycol (const Eigen::MatrixXd& X,
                                   const int n,
                                   const int p);
Eigen::MatrixXd mat_slicing_byrow (const Eigen::MatrixXd& X,
                                   const Eigen::ArrayXi index);
Eigen::MatrixXd mat_slicing_byrow2 (const Eigen::MatrixXd& X,
                                    const std::vector<int> index);
Eigen::MatrixXd mat_slicing_byrow3 (const Eigen::MatrixXd& X,
                                    std::vector<int> index);












#endif
