#ifndef QRUTILS_H
#define QRUTILS_H

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


Eigen::MatrixXd QRchol (const Eigen::MatrixXd& X);
Eigen::MatrixXd rbQRchol (const Eigen::MatrixXd& X,
                          const int& nb);

Eigen::VectorXd QRsolve (const Eigen::MatrixXd& A,
                         const Eigen::VectorXd& b);
Eigen::VectorXd rbQRsolve (const Eigen::MatrixXd& A,
                           const Eigen::VectorXd& b,
                           const int& nb);

Eigen::VectorXd Rsolve (const Eigen::MatrixXd& A,
                        const Eigen::VectorXd& b);

Rcpp::List qrls_pred1 (const Eigen::VectorXd& y,
                       const Eigen::MatrixXd& X);

Rcpp::List qrls_pred2 (const Eigen::VectorXd& y,
                       const Eigen::MatrixXd& X,
                       const Eigen::MatrixXd& X_test);

Rcpp::List rls_pred1 (const Eigen::VectorXd& y,
                      const Eigen::MatrixXd& X);

Rcpp::List rls_pred2 (const Eigen::VectorXd& y,
                      const Eigen::MatrixXd& X,
                      const Eigen::MatrixXd& X_test);

Rcpp::List qrridge_pred1 (const Eigen::VectorXd& y,
                          const Eigen::MatrixXd& X,
                          const double lambda);

Rcpp::List qrridge_pred2 (const Eigen::VectorXd& y,
                          const Eigen::MatrixXd& X,
                          const Eigen::MatrixXd& X_test,
                          const double lambda);

Eigen::MatrixXd rridge_R (const Eigen::MatrixXd& X,
                          const double lambda);

Rcpp::List rridge_pred1 (const Eigen::VectorXd& y,
                         const Eigen::MatrixXd& X,
                         const double lambda);

Rcpp::List rridge_pred2 (const Eigen::VectorXd& y,
                         const Eigen::MatrixXd& X,
                         const Eigen::MatrixXd& X_test,
                         const double lambda);

Rcpp::List rridge_downdate (const Eigen::VectorXd& y,
                            const Eigen::MatrixXd& X,
                            const Eigen::VectorXd& y_test,
                            const Eigen::MatrixXd& X_test,
                            const double lambda,
                            const Eigen::MatrixXd& R,
                            const Eigen::MatrixXd& U);

double multivariateR2 (const Eigen::MatrixXd& Y,
                       const Eigen::MatrixXd& X,
                       const Eigen::MatrixXd& B);
double multivariatePMSE (const Eigen::MatrixXd& Y,
                         const Eigen::MatrixXd& X,
                         const Eigen::MatrixXd& B);
Rcpp::List qrmls_pred1 (const Eigen::MatrixXd& Y,
                        const Eigen::MatrixXd& X);
Rcpp::List qrmls_pred2 (const Eigen::MatrixXd& Y,
                        const Eigen::MatrixXd& X,
                        const Eigen::MatrixXd& X_test);
Rcpp::List rmls_pred1 (const Eigen::MatrixXd& Y,
                       const Eigen::MatrixXd& X);
Rcpp::List rmls_pred2 (const Eigen::MatrixXd& Y,
                       const Eigen::MatrixXd& X,
                       const Eigen::MatrixXd& X_test);

Rcpp::List qrmridge_pred1 (const Eigen::MatrixXd& Y,
                           const Eigen::MatrixXd& X,
                           const double lambda);
Rcpp::List qrmridge_pred2 (const Eigen::MatrixXd& Y,
                           const Eigen::MatrixXd& X,
                           const Eigen::MatrixXd& X_test,
                           const double lambda);
Rcpp::List rmridge_pred1 (const Eigen::MatrixXd& Y,
                          const Eigen::MatrixXd& X,
                          const double lambda);
Rcpp::List rmridge_pred2 (const Eigen::MatrixXd& Y,
                          const Eigen::MatrixXd& X,
                          const Eigen::MatrixXd& X_test,
                          const double lambda);

Rcpp::List rmridge_downdate (const Eigen::MatrixXd& Y,
                             const Eigen::MatrixXd& X,
                             const Eigen::MatrixXd& Y_test,
                             const Eigen::MatrixXd& X_test,
                             const double lambda,
                             const Eigen::MatrixXd& R,
                             const Eigen::MatrixXd& U);








#endif
