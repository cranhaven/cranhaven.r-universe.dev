#ifndef QRDECOMPOSITION_H
#define QRDECOMPOSITION_H

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Eigen/Core>

#include <RcppEigen.h>
#include "givens_house.h"

#define EIGEN_USE_BLAS
#define EIGEN_USE_LAPACKE

#define DOUBLE_EPS 2.220446e-16
#define SAFE_LOG(a) (((a) <= 0.0) ? log(DOUBLE_EPS) : log(a))
#define SAFE_ZERO(a) ((a) == 0 ? DOUBLE_EPS : (a))
#define SQRT_DOUBLE_EPS sqrt(DOUBLE_EPS)

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Eigen;
using namespace arma;

Rcpp::List householderQR (const Eigen::MatrixXd& X,
                          int complete);

Rcpp::List givensQR (const Eigen::MatrixXd& X,
                     int complete);

Rcpp::List rbQR (const Eigen::MatrixXd& X,
                 const int& nb,
                 const int& complete);

Eigen::MatrixXd householderR (const Eigen::MatrixXd& X);





#endif

