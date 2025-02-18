#ifndef THINQRUPDATE_H
#define THINQRUPDATE_H

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Eigen/Core>

#include "support_functions.h"
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

Eigen::MatrixXd thinqraddcol (const Eigen::MatrixXd& R,
                              const Eigen::MatrixXd& X,
                              const Eigen::VectorXd& u);

Eigen::MatrixXd thinqraddmcols (const Eigen::MatrixXd& R,
                                const Eigen::MatrixXd& X,
                                const Eigen::MatrixXd& U);

Eigen::MatrixXd thinqrdeletecol (const Eigen::MatrixXd& R,
                                 const int& k);

Eigen::MatrixXd thinqrdeletemcols_adj (const Eigen::MatrixXd& R,
                                       const int& k,
                                       const int& m);

Eigen::MatrixXd thinqrdeletemcols (const Eigen::MatrixXd& R,
                                   const Eigen::VectorXi& k);

Eigen::MatrixXd thinqraddrow (const Eigen::MatrixXd& R,
                              const Eigen::VectorXd& u);

Eigen::MatrixXd thinqraddmrows (const Eigen::MatrixXd& R,
                                const Eigen::MatrixXd& U);

Eigen::MatrixXd thinqrdeleterow (const Eigen::MatrixXd& R,
                                 const Eigen::VectorXd& u);

Eigen::MatrixXd thinqrdeletemrows (const Eigen::MatrixXd& R,
                                   const Eigen::MatrixXd& U);




#endif
