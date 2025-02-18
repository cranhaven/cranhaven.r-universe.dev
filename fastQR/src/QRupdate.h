#ifndef QRUPDATE_H
#define QRUPDATE_H

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

Rcpp::List qraddcol (const Eigen::MatrixXd& Q,
                     const Eigen::MatrixXd& R,
                     const int& k,
                     const Eigen::VectorXd& u);

Rcpp::List qraddmcols (const Eigen::MatrixXd& Q,
                       const Eigen::MatrixXd& R,
                       const int& k, const
                       Eigen::MatrixXd& U);

Rcpp::List qraddrow (const Eigen::MatrixXd& Q,
                     const Eigen::MatrixXd& R,
                     const int& k,
                     const Eigen::RowVectorXd& u);

Rcpp::List qraddmrows (const Eigen::MatrixXd& Q,
                       const Eigen::MatrixXd& R,
                       const int& k,
                       const Eigen::MatrixXd& U);

Rcpp::List qrdeleterow (const Eigen::MatrixXd& Q,
                        const Eigen::MatrixXd& R,
                        const int& k);

Rcpp::List qrdeletemrows (const Eigen::MatrixXd& Q,
                          const Eigen::MatrixXd& R,
                          const int& k,
                          const int& m);

Rcpp::List qrdeletecol (const Eigen::MatrixXd& Q,
                        const Eigen::MatrixXd& R,
                        const int& k);

Rcpp::List qrdeletemcols_adj (const Eigen::MatrixXd& Q,
                              const Eigen::MatrixXd& R,
                              const int& k,
                              const int& m);

Rcpp::List qrdeletemcols (const Eigen::MatrixXd& Q,
                          const Eigen::MatrixXd& R,
                          const Eigen::VectorXi& k);






#endif
