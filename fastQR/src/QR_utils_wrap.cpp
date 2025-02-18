// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                       :::::::::::::::::::: //
// ::::::::::::::::::::    QR utils wrap      :::::::::::::::::::: //
// ::::::::::::::::::::                       :::::::::::::::::::: //
// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //

#include "utils.h"
#include "QRutils.h"
#include "QRdecomposition.h"

#define EIGEN_USE_BLAS
#define EIGEN_USE_LAPACKE

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(Rcpp)]]

using namespace Rcpp;
using namespace Eigen;

/* This function computes the ols estimates for the linear regression model.   */

//' @name qrls
//' @title Ordinary least squares for the linear regression model
//' @description qrls, or LS for linear regression models, solves the following optimization problem
//' \deqn{\textrm{min}_\beta ~ \frac{1}{2}\|y-X\beta\|_2^2,}
//' for \eqn{y\in\mathbb{R}^n} and \eqn{X\in\mathbb{R}^{n\times p}}, to obtain a coefficient vector \eqn{\widehat{\beta}\in\mathbb{R}^p}. The design matrix \eqn{X\in\mathbb{R}^{n\times p}}
//' contains the observations for each regressor.
//' @param y a vector of length-\eqn{n} response vector.
//' @param X an \eqn{(n\times p)} full column rank matrix of predictors.
//' @param X_test an \eqn{(q\times p)} full column rank matrix. Test set. By default it set to NULL.
//' @param type either "QR" or "R". Specifies the type of decomposition to use: "QR" for the QR decomposition or "R" for the Cholesky factorization of \eqn{A^\top A}. The default is "QR".
//' @return A named list containing \describe{
//' \item{coeff}{a length-\eqn{p} vector containing the solution for the parameters \eqn{\beta}.}
//' \item{fitted}{a length-\eqn{n} vector of fitted values, \eqn{\widehat{y}=X\widehat{\beta}}.}
//' \item{residuals}{a length-\eqn{n} vector of residuals, \eqn{\varepsilon=y-\widehat{y}}.}
//' \item{residuals_norm2}{the L2-norm of the residuals, \eqn{\Vert\varepsilon\Vert_2^2.}}
//' \item{y_norm2}{the L2-norm of the response variable. \eqn{\Vert y\Vert_2^2.}}
//' \item{XTX_Qmat}{\eqn{Q} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{XTX_Rmat}{\eqn{R} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{QXTy}{\eqn{QX^\top y}, where \eqn{Q} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{R2}{\eqn{R^2}, coefficient of determination, measure of goodness-of-fit of the model.}
//' \item{predicted}{predicted values for the test set, \eqn{X_{\text{test}}\widehat{\beta}}. It is only available if X_test is not NULL.}
//' }
//' @examples
//'
//' ## generate sample data
//' set.seed(10)
//' n         <- 30
//' p         <- 6
//' X         <- matrix(rnorm(n * p, 1), n, p)
//' X[,1]     <- 1
//' eps       <- rnorm(n, sd = 0.5)
//' beta      <- rep(0, p)
//' beta[1:3] <- 1
//' beta[4:5] <- 2
//' y         <- X %*% beta + eps
//' X_test    <- matrix(rnorm(5 * p, 1), 5, p)
//' output    <-  fastQR::qrls(y = y, X = X, X_test = X_test)
//' output$coeff
//'
// [[Rcpp::export]]
Rcpp::List qrls(const Eigen::VectorXd& y,
                const Eigen::MatrixXd& X,
                Rcpp::Nullable<Rcpp::NumericMatrix> X_test = R_NilValue,
                Rcpp::Nullable<std::string> type = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List out;
  std::string type_ = "QR";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
  } else {
    type_ = "QR";
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  if (X_test.isNotNull()) {
    /* Nullable output declaration */
    Rcpp::NumericMatrix X_test_tmp(X_test);
    
    // Transform Rcpp vector "vec_rcpp" into an Armadillo vector
    Eigen::MatrixXd X_test_ = Rcpp::as<Eigen::MatrixXd>(wrap(X_test_tmp));
    
    // run OLS with prediction
    if (type_ == "R") {
      out = rls_pred2(y, X, X_test_);
    } else {
      out = qrls_pred2(y, X, X_test_);
    }
  } else {
    // run OLS
    if (type_ == "R") {
      out = rls_pred1(y, X);
    } else {
      out = qrls_pred1(y, X);
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return out;
}

//' @name qrridge
//' @title RIDGE estimation for the linear regression model
//' @description lmridge, or RIDGE for linear regression models, solves the following penalized optimization problem
//' \deqn{\textrm{min}_\beta ~ \frac{1}{n}\|y-X\beta\|_2^2+\lambda\Vert\beta\Vert_2^2,}
//' to obtain a coefficient vector \eqn{\widehat{\beta}\in\mathbb{R}^{p}}. The design matrix \eqn{X\in\mathbb{R}^{n\times p}}
//' contains the observations for each regressor.
//' @param y a vector of length-\eqn{n} response vector.
//' @param X an \eqn{(n\times p)} matrix of predictors.
//' @param lambda a vector of lambdas.
//' @param X_test an \eqn{(q\times p)} full column rank matrix. Test set. By default it set to NULL.
//' @param type either "QR" or "R". Specifies the type of decomposition to use: "QR" for the QR decomposition or "R" for the Cholesky factorization of \eqn{A^\top A}. The default is "QR".
//' @return A named list containing \describe{
//' \item{mean_y}{mean of the response variable.}
//' \item{mean_X}{a length-\eqn{p} vector containing the mean of each column of the design matrix.}
//' \item{path}{the whole path of estimated regression coefficients.}
//' \item{ess}{explained sum of squares for the whole path of estimated coefficients.}
//' \item{GCV}{generalized cross-validation for the whole path of lambdas.}
//' \item{GCV_min}{minimum value of GCV.}
//' \item{GCV_idx}{inded corresponding to the minimum values of GCV.}
//' \item{coeff}{a length-\eqn{p} vector containing the solution for the parameters \eqn{\beta} which corresponds to the minimum of GCV.}
//' \item{lambda}{the vector of lambdas.}
//' \item{scales}{the vector of standard deviations of each column of the design matrix.}
//' }
//' @examples
//'
//' ## generate sample data
//' set.seed(10)
//' n         <- 30
//' p         <- 6
//' X         <- matrix(rnorm(n * p, 1), n, p)
//' X[,1]     <- 1
//' eps       <- rnorm(n, sd = 0.5)
//' beta      <- rep(0, p)
//' beta[1:3] <- 1
//' beta[4:5] <- 2
//' y         <- X %*% beta + eps
//' X_test    <- matrix(rnorm(5 * p, 1), 5, p)
//' output    <-  fastQR::qrridge(y = y, X = X,
//'                               lambda = 0.2,
//'                               X_test = X_test)
//' output$coeff
//'
// [[Rcpp::export]]
Rcpp::List qrridge(const Eigen::VectorXd& y,
                   const Eigen::MatrixXd& X,
                   const double lambda,
                   Rcpp::Nullable<Rcpp::NumericMatrix> X_test = R_NilValue,
                   Rcpp::Nullable<std::string> type = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List out;
  std::string type_ = "QR";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL                                           */
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
  } else {
    type_ = "QR";
  }
  if (X_test.isNotNull()) {
    /* Nullable output declaration */
    Rcpp::NumericMatrix X_test_tmp(X_test);
    
    // Transform Rcpp vector "vec_rcpp" into an Armadillo vector
    Eigen::MatrixXd X_test_ = Rcpp::as<Eigen::MatrixXd>(wrap(X_test_tmp));
    
    // run OLS with prediction
    if (type_ == "R") {
      out = rridge_pred2(y, X, X_test_, lambda);
    } else {
      out = qrridge_pred2(y, X, X_test_, lambda);
    }
  } else {
    // run OLS
    if (type_ == "R") {
      out = rridge_pred1(y, X, lambda);
    } else {
      out = qrridge_pred1(y, X, lambda);
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return out;
}

//' @name qrchol
//' @title Cholesky decomposition via QR factorization.
//' @description qrchol, provides the Cholesky decomposition of the symmetric and positive definite matrix \eqn{X^\top X\in\mathbb{R}^{p\times p}}, where \eqn{X\in\mathbb{R}^{n\times p}} is the input matrix.
//' @param X an \eqn{(n\times p)} matrix.
//' @param nb number of blocks for the recursive block QR decomposition, default is NULL.
//' @return an upper triangular matrix of dimension \eqn{p\times p} which represents the Cholesky decomposition of \eqn{X^\top X}.
// [[Rcpp::export]]
Eigen::MatrixXd qrchol(const Eigen::MatrixXd& X,
                       Rcpp::Nullable<int> nb = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  int nb_ = 0;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = X.rows();
  const int p = X.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd R(p, p);          R.setZero();        // full R matrix

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (nb.isNotNull()) {
    nb_ = Rcpp::as<int>(nb);
    if (nb_ == 0) {
      warning("* fastQR : if the parameter nb is set to zero, use the QR decomposition!\n");
    }
    /* recursive block update is a valid option only when n>p */
    if (p > n) {
      nb_ = 0;
      warning("* fastQR : the parameter nb is set to NULL if p > n!\n");
    }
  } else {
    nb_ = 0;
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  if (nb_ == 0) {
    R = QRchol(X);
  } else {
    //Rcpp::Rcout << "Recursive block QR decomposition.\n";
    R = rbQRchol(X, nb_);
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return R;
}

//' @name qrsolve
//' @title Solution of linear system of equations, via the QR decomposition.
//' @description solves systems of equations \eqn{Ax=b}, for \eqn{A\in\mathbb{R}^{n\times p}} and \eqn{b\in\mathbb{R}^n}, via the QR decomposition.
//' @param A an \eqn{(n\times p)} full column rank matrix.
//' @param b a vector of dimension \eqn{n}.
//' @param type either "QR" or "R". Specifies the type of decomposition to use: "QR" for the QR decomposition or "R" for the Cholesky factorization of \eqn{A^\top A}. The default is "QR".
//' @param nb number of blocks for the recursive block QR decomposition, default is NULL.
//' @return x a vector of dimension \eqn{p} that satisfies \eqn{Ax=b}.
//' @examples
//'
//' ## generate sample data
//' set.seed(1234)
//' n <- 10
//' p <- 4
//' A <- matrix(rnorm(n * p, 1), n, p)
//' b <- rnorm(n)
//'
//' ## solve the system of linear equations using qr
//' x1 <- fastQR::qrsolve(A = A, b = b)
//' x1
//'
//' ## solve the system of linear equations using rb qr
//' x2 <- fastQR::qrsolve(A = A, b = b, nb = 2)
//' x2
//'
//' ## check
//' round(x1 - solve(crossprod(A)) %*% crossprod(A, b), 5)
//' round(x2 - solve(crossprod(A)) %*% crossprod(A, b), 5)
//'
//' @references
//' \insertRef{golub_van_loan.2013}{fastQR}
//'
//' \insertRef{bjorck.2015}{fastQR}
//'
//' \insertRef{bjorck.2024}{fastQR}
//'
//' \insertRef{bernardi_etal.2024}{fastQR}
//'
// [[Rcpp::export]]
Eigen::VectorXd qrsolve(const Eigen::MatrixXd& A,
                        const Eigen::VectorXd& b,
                        Rcpp::Nullable<std::string> type = R_NilValue,
                        Rcpp::Nullable<int> nb = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  int nb_ = 0;
  std::string type_ = "QR";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = A.rows();
  const int p = A.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::VectorXd x(p);             x.setZero();          // full x vector

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
  } else {
    type_ = "QR";
  }
  if (nb.isNotNull()) {
    nb_ = Rcpp::as<int>(nb);
    if (nb_ == 0) {
      warning("* fastQR : if the parameter nb is set to zero, use the QR decomposition!\n");
    }
    /* recursive block update is a valid option only when n>p */
    if (p > n) {
      nb_ = 0;
      warning("* fastQR : the parameter nb is set to NULL if p > n!\n");
    }
  } else {
    nb_ = 0;
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  if (type_ == "QR") {
    if (nb_ == 0) {
      x = QRsolve(A, b);
    } else {
      //Rcpp::Rcout << "Recursive block QR decomposition.\n";
      x = rbQRsolve(A, b, nb_);
    }
  } else {
    x = Rsolve(A, b);
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return x;
}

//' @name rchol
//' @title Cholesky decomposition via R factorization.
//' @description rchol, provides the Cholesky decomposition of the symmetric and positive definite matrix \eqn{X^\top X\in\mathbb{R}^{p\times p}}, where \eqn{X\in\mathbb{R}^{n\times p}} is the input matrix.
//' @param X an \eqn{(n\times p)} matrix, with \eqn{n\geq p}. If \eqn{n< p} an error message is returned.
//' @return an upper triangular matrix of dimension \eqn{p\times p} which represents the Cholesky decomposition of \eqn{X^\top X}.
//' @examples
//'
//' set.seed(1234)
//' n <- 10
//' p <- 6
//' X <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## compute the Cholesky decomposition of X^TX
//' S <- fastQR::rchol(X = X)
//' S
//'
//' ## check
//' round(S - chol(crossprod(X)), 5)
//'
//' @references
//' \insertRef{golub_van_loan.2013}{fastQR}
//'
//' \insertRef{bjorck.2015}{fastQR}
//'
//' \insertRef{bjorck.2024}{fastQR}
//'
//' \insertRef{bernardi_etal.2024}{fastQR}
//'

// [[Rcpp::export]]
Eigen::MatrixXd rchol(const Eigen::MatrixXd& X) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return (householderR(X));
}

Rcpp::List groups_cv(int n,
                     int k = 10,
                     Rcpp::Nullable<int> seed = R_NilValue) {
  
  /* Variable delcaration           */
  double maxf = 0.0;
  Rcpp::List output;
  int glen = 0, glen_cum = 0, seed_ = 1234;
  
  /* Definition of vectors and matrices     */
  arma::uvec dummyorder(n, fill::zeros);
  arma::vec f(n, fill::zeros);
  arma::uvec groups_all(n, fill::zeros);
  arma::uvec foldid(n, fill::zeros);
  
  if (k == 0) {
    k = n;
  }
  if (k < 0 || k > n) {
    Rcpp::stop("Invalid values of 'k'. Must be between 0 (for leave-one-out CV) and 'n'.");
  }
  if (seed.isNotNull()) {
    seed_ = Rcpp::as<int>(seed);
  } else {
    seed_ = 1234;
  }
  arma_rng::set_seed(seed_);
  //if (seed.isNotNull()) {
    //Rcpp::IntegerVector seed_vec(seed);
    //std::srand(seed_vec[0]);
  //}
  
  // Create a random permutation of the sequence 1 to n
  dummyorder = arma::randperm(n);
  f          = arma::ceil(arma::linspace(1, n, n) / (double(n) / double(k)));
  maxf       = max(f);
    
  // Create groups for prediction
  arma::field<uvec> groups(maxf);
  arma::vec groups_nelem(maxf);
  glen_cum = 0;
  glen     = 0;
  for (int i=1; i<=maxf; i++) {
    arma::uvec indi = find(f == i);
    groups(i-1)     = dummyorder(indi);
    glen            = indi.n_elem;
    if (i < maxf) {
      groups_all.subvec(glen_cum, glen_cum+glen-1) = dummyorder(indi);
      glen_cum                                    += glen;
    } else {
      groups_all.subvec(glen_cum, glen_cum+glen-1) = dummyorder(indi);
    }
    groups_nelem(i-1) = groups(i-1).n_elem;
  }
  // Create groups for cross-validation
  arma::field<uvec> groups_cv(maxf);
  for (int i = 1; i <= maxf; i++) {
    groups_cv(i-1) = set_diff(groups_all, groups(i-1)) + 1;
  }
  
  // Define foldid
  for (int i = 0; i <maxf; i++) {
    arma::uvec idx_ = groups(i);
    arma::uvec vv(idx_.n_elem, fill::ones);
    vv           *= i;
    foldid(idx_)  = vv + 1;
    groups(i)    += 1;
  }
  
  /* Get output         */
  output = Rcpp::List::create(Rcpp::Named("groups_pred")  = groups,
                              Rcpp::Named("groups_all")   = groups_all+1,
                              Rcpp::Named("shuffle")      = groups_cv,
                              Rcpp::Named("foldid")       = foldid,
                              Rcpp::Named("n_groups")     = maxf,
                              Rcpp::Named("groups_nelem") = groups_nelem);
    
  /* Return output      */
  return output;
}

Rcpp::List groups_cv2(int n,
                      int k = 10,
                      int seed = 1234) {
  
  /* Variable delcaration           */
  double maxf = 0.0;
  Rcpp::List output;
  int glen = 0, glen_cum = 0;
  
  /* Definition of vectors and matrices     */
  arma::uvec dummyorder(n, fill::zeros);
  arma::vec f(n, fill::zeros);
  arma::uvec groups_all(n, fill::zeros);
  arma::uvec foldid(n, fill::zeros);
  
  if (k == 0) {
    k = n;
  }
  if (k < 0 || k > n) {
    Rcpp::stop("Invalid values of 'k'. Must be between 0 (for leave-one-out CV) and 'n'.");
  }
  
  // set seed
  //Rcpp::IntegerVector seed_vec(seed);
  //std::srand(seed_vec[0]);
  arma_rng::set_seed(seed);
  
  // Create a random permutation of the sequence 1 to n
  dummyorder = arma::randperm(n);
  f          = arma::ceil(arma::linspace(1, n, n) / (double(n) / double(k)));
  maxf       = max(f);
    
  // Create groups for prediction
  arma::field<uvec> groups(maxf);
  arma::vec groups_nelem(maxf);
  glen_cum = 0;
  glen     = 0;
  for (int i=1; i<=maxf; i++) {
    arma::uvec indi = find(f == i);
    groups(i-1)     = dummyorder(indi);
    glen            = indi.n_elem;
    if (i < maxf) {
      groups_all.subvec(glen_cum, glen_cum+glen-1) = dummyorder(indi);
      glen_cum                                    += glen;
    } else {
      groups_all.subvec(glen_cum, glen_cum+glen-1) = dummyorder(indi);
    }
    groups_nelem(i-1) = groups(i-1).n_elem;
  }
  // Create groups for cross-validation
  arma::field<uvec> groups_cv(maxf);
  for (int i = 1; i <= maxf; i++) {
    groups_cv(i-1) = set_diff(groups_all, groups(i-1)) + 1;
  }
  
  // Define foldid
  for (int i = 0; i <maxf; i++) {
    arma::uvec idx_ = groups(i);
    arma::uvec vv(idx_.n_elem, fill::ones);
    vv           *= i;
    foldid(idx_)  = vv + 1;
    groups(i)    += 1;
  }
  
  /* Get output         */
  output = Rcpp::List::create(Rcpp::Named("groups_pred")  = groups,
                              Rcpp::Named("groups_all")   = groups_all+1,
                              Rcpp::Named("shuffle")      = groups_cv,
                              Rcpp::Named("foldid")       = foldid,
                              Rcpp::Named("n_groups")     = maxf,
                              Rcpp::Named("groups_nelem") = groups_nelem);
    
  /* Return output      */
  return output;
}

//' @name qrridge_cv
//' @title Cross-validation of the RIDGE estimator for the linear regression model
//' @description qrridge_cv, or LS for linear multivariate regression models, solves the following optimization problem
//' \deqn{\textrm{min}_\beta ~ \frac{1}{2}\|Y-XB\|_2^2,}
//' for \eqn{Y\in\mathbb{R}^{n \times q}} and \eqn{X\in\mathbb{R}^{n\times p}}, to obtain a coefficient matrix \eqn{\widehat{B}\in\mathbb{R}^{p\times q}}. The design matrix \eqn{X\in\mathbb{R}^{n\times p}}
//' contains the observations for each regressor.
//' @param y a vector of length-\eqn{n} response vector.
//' @param X an \eqn{(n\times p)} full column rank matrix of predictors.
//' @param lambda a vector of lambdas.
//' @param k an integer vector defining the number of groups for CV.
//' @param seed ad integer number defining the seed for random number generation.
//' @param X_test an \eqn{(q\times p)} full column rank matrix. Test set. By default it set to NULL.
//' @param type either "QR" or "R". Specifies the type of decomposition to use: "QR" for the QR decomposition or "R" for the Cholesky factorization of \eqn{A^\top A}. The default is "QR".
//' @return A named list containing \describe{
//' \item{coeff}{a length-\eqn{p} vector containing the solution for the parameters \eqn{\beta}.}
//' \item{fitted}{a length-\eqn{n} vector of fitted values, \eqn{\widehat{y}=X\widehat{\beta}}.}
//' \item{residuals}{a length-\eqn{n} vector of residuals, \eqn{\varepsilon=y-\widehat{y}}.}
//' \item{residuals_norm2}{the L2-norm of the residuals, \eqn{\Vert\varepsilon\Vert_2^2.}}
//' \item{y_norm2}{the L2-norm of the response variable. \eqn{\Vert y\Vert_2^2.}}
//' \item{XTX}{the matrix \eqn{X^\top X}.}
//' \item{XTy}{\eqn{X^\top y}.}
//' \item{sigma_hat}{estimated  residual variance.}
//' \item{df}{degrees of freedom.}
//' \item{Q}{\eqn{Q} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{R}{\eqn{R} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{QXTy}{\eqn{QX^\top y}, where \eqn{Q} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{R2}{\eqn{R^2}, coefficient of determination, measure of goodness-of-fit of the model.}
//' \item{predicted}{predicted values for the test set, \eqn{X_{\text{test}}\widehat{\beta}}. It is only available if X_test is not NULL.}
//' }
//' @examples
//'
//' ## generate sample data
//' set.seed(10)
//' n         <- 30
//' p         <- 6
//' X         <- matrix(rnorm(n * p, 1), n, p)
//' X[,1]     <- 1
//' eps       <- rnorm(n)
//' beta      <- rep(1, p)
//' y         <- X %*% beta + eps
//' X_test    <- matrix(rnorm(5 * p, 1), 5, p)
//' output    <- fastQR::qrridge_cv(y = y, X = X, lambda = c(1,2), 
//'                                 k = 5, seed = 12, X_test = X_test, type = "QR")
//' output$coeff
//'

// [[Rcpp::export]]
Rcpp::List qrridge_cv(Eigen::VectorXd& y,
                      Eigen::MatrixXd& X,
                      Eigen::VectorXd& lambda,
                      Rcpp::Nullable<int> k = R_NilValue,
                      Rcpp::Nullable<int> seed = R_NilValue,
                      Rcpp::Nullable<Rcpp::NumericMatrix> X_test = R_NilValue,
                      Rcpp::Nullable<std::string> type = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List out, res, output;
  int k_ = 10, seed_ = 1234, n_groups = 1;
  double ave_mse = 0.0, lambda_min = 0.0, mse = 0.0, mse_min = 0.0, y_norm2 = 0.0, res2 = 0.0;
  Eigen::Index mse_min_indi;
  std::string type_ = "QR";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  int n       = X.rows();
  int p       = X.cols();
  int nlambda = lambda.size();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd X_tilde(n + p, p);                    X_tilde.setZero();
  Eigen::MatrixXd D0(p, p);                             D0.setIdentity();
  Eigen::MatrixXd R0(p, p);                             R0.setZero();
  Eigen::VectorXd ave_mse_STORE(nlambda);               ave_mse_STORE.setZero();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL                                           */
  if (k.isNotNull()) {
    k_ = Rcpp::as<int>(k);
  } else {
    k_ = 10;
  }
  if (seed.isNotNull()) {
    seed_ = Rcpp::as<int>(seed);
  } else {
    seed_ = 1234;
  }
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
  } else {
    type_ = "QR";
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   define groups for CV                                         */
  res                         = groups_cv2(n, k_, seed_);
  arma::field<uvec> groups    = res["groups_pred"];
  arma::field<uvec> shuffle   = res["shuffle"];
  n_groups                    = res["n_groups"];
  arma::uvec groups_nelem     = res["groups_nelem"];
  arma::field<uvec> groups_cv = res["groups_all"];
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::VectorXd y_train0(n-groups_nelem(0));           y_train0.setZero();
  Eigen::VectorXd y_test0(groups_nelem(0));              y_test0.setZero();
  Eigen::MatrixXd X_train0(n-groups_nelem(0), p);        X_train0.setZero();
  Eigen::MatrixXd X_test0(groups_nelem(0), p);           X_test0.setZero();
  std::vector<int> idx_shuffle(n-groups_nelem(0));
  std::vector<int> idx_groups(groups_nelem(0));
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Run CV                                           */
  for (int j=0; j<nlambda; j++) {
    // print to screen
    Rcpp::Rcout << ">>> qrridge_cv, processing lambda: " << lambda(j) << "\n";
    
    // define the augmented matrix for all observations
    X_tilde << X, sqrt(lambda(j)) * D0;
    
    // compute initial R matrix
    res = householderQR(X_tilde, false);
    R0  = res["R"];
    
    // compute average predictive MSE
    ave_mse = 0.0;
    for (int i=0; i<n_groups; i++) {

      // convert to std vector
      idx_shuffle = arma::conv_to <std::vector<int>>::from(shuffle(i));
      idx_groups  = arma::conv_to <std::vector<int>>::from(groups(i));
      
      if (i == (n_groups-1)) {
        if (groups_nelem(n_groups-1) != groups_nelem(0)) {
          idx_shuffle.resize(n-groups_nelem(n_groups-1));
          idx_groups.resize(groups_nelem(n_groups-1));
        }
      }
      // subtract one
      std::for_each(idx_shuffle.begin(), idx_shuffle.end(), subtractone);
      std::for_each(idx_groups.begin(), idx_groups.end(), subtractone);
      if (i == (n_groups-1)) {
        if (groups_nelem(n_groups-1) != groups_nelem(0)) {
          y_train0.resize(n-groups_nelem(n_groups-1));
          y_test0.resize(groups_nelem(n_groups-1));
          X_train0.resize(n-groups_nelem(n_groups-1), p);
          X_test0.resize(groups_nelem(n_groups-1), p);
          
          y_train0.setZero();
          y_test0.setZero();
          X_train0.setZero();
          X_test0.setZero();
        }
      }
      y_train0 = y(idx_shuffle);
      y_test0  = y(idx_groups);
      X_train0 = mat_slicing_byrow2(X, idx_shuffle);
      X_test0  = mat_slicing_byrow2(X, idx_groups);
      // downdate R0
      out      = rridge_downdate(y_train0, X_train0,
                                 y_test0, X_test0,
                                 lambda(j), R0,
                                 X_test0.transpose());
      mse      = out["PMSE"];
      ave_mse += mse;
    }
    ave_mse          /= (double)n_groups;
    ave_mse_STORE(j)  = ave_mse;
  }
  /* ::::::::::::::::::::::::::::::::::::::::::::::
    Post-process output                                       */
  mse_min    = ave_mse_STORE.array().minCoeff(&mse_min_indi);
  lambda_min = lambda(mse_min_indi);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
    Get the RIDGE estimate                                */
  if (X_test.isNotNull()) {
    /* Nullable output declaration */
    Rcpp::NumericMatrix X_test_tmp(X_test);
    
    // Transform Rcpp vector "vec_rcpp" into an Armadillo vector
    Eigen::MatrixXd X_test_ = Rcpp::as<Eigen::MatrixXd>(wrap(X_test_tmp));
    
    // run OLS with prediction
    if (type_ == "R") {
      out = rridge_pred2(y, X, X_test_, lambda_min);
    } else {
      out = qrridge_pred2(y, X, X_test_, lambda_min);
    }
  } else {
    // run OLS
    if (type_ == "R") {
      out = rridge_pred1(y, X, lambda_min);
    } else {
      out = qrridge_pred1(y, X, lambda_min);
    }
  }
  Eigen::VectorXd regp   = out["coeff"];
  Eigen::VectorXd fitted = out["fitted"];
  Eigen::VectorXd resid  = out["residuals"];
  Eigen::MatrixXd XTX    = out["XTX"];
  Eigen::VectorXd XTy    = out["XTy"];
  Eigen::MatrixXd Q      = out["Q"];
  Eigen::MatrixXd R      = out["R"];
  y_norm2                = out["y_norm2"];
  res2                   = out["residuals_norm2"];

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("lambda")          = lambda,
                              Rcpp::Named("mse")             = ave_mse_STORE,
                              Rcpp::Named("mse_min")         = mse_min,
                              Rcpp::Named("mse_min_ind")     = mse_min_indi,
                              Rcpp::Named("lambda_min")      = lambda_min,
                              Rcpp::Named("coeff")           = regp,
                              Rcpp::Named("fitted")          = fitted,
                              Rcpp::Named("residuals")       = resid,
                              Rcpp::Named("residuals_norm2") = res2,
                              Rcpp::Named("y_norm2")         = y_norm2,
                              Rcpp::Named("XTX")             = XTX,
                              Rcpp::Named("XTy")             = XTy,
                              Rcpp::Named("sigma2_hat")      = res2 / (n - p),
                              Rcpp::Named("df")              = n-p,
                              Rcpp::Named("Q")               = Q,
                              Rcpp::Named("R")               = R,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2));
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

//' @name qrmls
//' @title Ordinary least squares for the linear multivariate regression model
//' @description qrmls, or LS for linear multivariate regression models, solves the following optimization problem
//' \deqn{\textrm{min}_\beta ~ \frac{1}{2}\|Y-XB\|_2^2,}
//' for \eqn{Y\in\mathbb{R}^{n \times q}} and \eqn{X\in\mathbb{R}^{n\times p}}, to obtain a coefficient matrix \eqn{\widehat{B}\in\mathbb{R}^{p\times q}}. The design matrix \eqn{X\in\mathbb{R}^{n\times p}}
//' contains the observations for each regressor.
//' @param Y a matrix of dimension \eqn{(n\times q} response variables.
//' @param X an \eqn{(n\times p)} full column rank matrix of predictors.
//' @param X_test an \eqn{(q\times p)} full column rank matrix. Test set. By default it set to NULL.
//' @param type either "QR" or "R". Specifies the type of decomposition to use: "QR" for the QR decomposition or "R" for the Cholesky factorization of \eqn{A^\top A}. The default is "QR".
//' @return A named list containing \describe{
//' \item{coeff}{a matrix of dimension \eqn{p\times q} containing the solution for the parameters \eqn{B}.}
//' \item{fitted}{a matrix of dimension \eqn{n\times q} of fitted values, \eqn{\widehat{Y}=X\widehat{B}}.}
//' \item{residuals}{a matrix of dimension \eqn{n\times q} of residuals, \eqn{\varepsilon=Y-\widehat{Y}}.}
//' \item{XTX}{the matrix \eqn{X^\top X}.}
//' \item{Sigma_hat}{a matrix of dimension \eqn{q\times q} containing the estimated  residual variance-covariance matrix.}
//' \item{df}{degrees of freedom.}
//' \item{R}{\eqn{R} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{XTy}{\eqn{X^\top y}.}
//' \item{R2}{\eqn{R^2}, coefficient of determination, measure of goodness-of-fit of the model.}
//' \item{predicted}{predicted values for the test set, \eqn{X_{\text{test}}\widehat{B}}. It is only available if X_test is not NULL.}
//' \item{PMSE}{}
//' }
//' @examples
//'
//' ## generate sample data
//' set.seed(10)
//' n         <- 30
//' p         <- 6
//' q         <- 3
//' X         <- matrix(rnorm(n * p, 1), n, p)
//' X[,1]     <- 1
//' eps       <- matrix(rnorm(n*q), n, q)
//' B         <- matrix(0, p, q)
//' B[,1]     <- rep(1, p)
//' B[,2]     <- rep(2, p)
//' B[,3]     <- rep(-1, p)
//' Y         <- X %*% B + eps
//' X_test    <- matrix(rnorm(5 * p, 1), 5, p)
//' output    <- fastQR::qrmls(Y = Y, X = X, X_test = X_test, type = "QR")
//' output$coeff
//'

// [[Rcpp::export]]
Rcpp::List qrmls(const Eigen::MatrixXd& Y,
                 const Eigen::MatrixXd& X,
                 Rcpp::Nullable<Rcpp::NumericMatrix> X_test = R_NilValue,
                 Rcpp::Nullable<std::string> type = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List out;
  std::string type_ = "QR";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
  } else {
    type_ = "QR";
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  if (X_test.isNotNull()) {
    /* Nullable output declaration */
    Rcpp::NumericMatrix X_test_tmp(X_test);
    
    // Transform Rcpp vector "vec_rcpp" into an Armadillo vector
    Eigen::MatrixXd X_test_ = Rcpp::as<Eigen::MatrixXd>(wrap(X_test_tmp));
    
    // run OLS with prediction
    if (type_ == "R") {
      out = rmls_pred2(Y, X, X_test_);
    } else {
      out = qrmls_pred2(Y, X, X_test_);
    }
  } else {
    // run OLS
    if (type_ == "R") {
      out = rmls_pred1(Y, X);
    } else {
      out = qrmls_pred1(Y, X);
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return out;
}

//' @name qrmridge
//' @title RIDGE estimator for the linear multivariate regression model
//' @description qrmridge, or LS for linear multivariate regression models, solves the following optimization problem
//' \deqn{\textrm{min}_\beta ~ \frac{1}{2}\|Y-XB\|_2^2,}
//' for \eqn{Y\in\mathbb{R}^{n \times q}} and \eqn{X\in\mathbb{R}^{n\times p}}, to obtain a coefficient matrix \eqn{\widehat{B}\in\mathbb{R}^{p\times q}}. The design matrix \eqn{X\in\mathbb{R}^{n\times p}}
//' contains the observations for each regressor.
//' @param Y a matrix of dimension \eqn{(n\times q} response variables.
//' @param X an \eqn{(n\times p)} full column rank matrix of predictors.
//' @param lambda a vector of lambdas.
//' @param X_test an \eqn{(q\times p)} full column rank matrix. Test set. By default it set to NULL.
//' @param type either "QR" or "R". Specifies the type of decomposition to use: "QR" for the QR decomposition or "R" for the Cholesky factorization of \eqn{A^\top A}. The default is "QR".
//' @return A named list containing \describe{
//' \item{coeff}{a matrix of dimension \eqn{p\times q} containing the solution for the parameters \eqn{B}.}
//' \item{fitted}{a matrix of dimension \eqn{n\times q} of fitted values, \eqn{\widehat{Y}=X\widehat{B}}.}
//' \item{residuals}{a matrix of dimension \eqn{n\times q} of residuals, \eqn{\varepsilon=Y-\widehat{Y}}.}
//' \item{XTX}{the matrix \eqn{X^\top X}.}
//' \item{Sigma_hat}{a matrix of dimension \eqn{q\times q} containing the estimated  residual variance-covariance matrix.}
//' \item{df}{degrees of freedom.}
//' \item{R}{\eqn{R} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{XTy}{\eqn{X^\top y}.}
//' \item{R2}{\eqn{R^2}, coefficient of determination, measure of goodness-of-fit of the model.}
//' \item{predicted}{predicted values for the test set, \eqn{X_{\text{test}}\widehat{B}}. It is only available if X_test is not NULL.}
//' \item{PMSE}{}
//' }
//'
//' @examples
//' ## generate sample data
//' set.seed(10)
//' n         <- 30
//' p         <- 6
//' q         <- 3
//' X         <- matrix(rnorm(n * p, 1), n, p)
//' X[,1]     <- 1
//' eps       <- matrix(rnorm(n*q), n, q)
//' B         <- matrix(0, p, q)
//' B[,1]     <- rep(1, p)
//' B[,2]     <- rep(2, p)
//' B[,3]     <- rep(-1, p)
//' Y         <- X %*% B + eps
//' X_test    <- matrix(rnorm(5 * p, 1), 5, p)
//' output    <- fastQR::qrmridge(Y = Y, X = X, lambda = 1, X_test = X_test, type = "QR")
//' output$coeff
//'

// [[Rcpp::export]]
Rcpp::List qrmridge(const Eigen::MatrixXd& Y,
                    const Eigen::MatrixXd& X,
                    const double lambda,
                    Rcpp::Nullable<Rcpp::NumericMatrix> X_test = R_NilValue,
                    Rcpp::Nullable<std::string> type = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List out;
  std::string type_ = "QR";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL                                           */
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
  } else {
    type_ = "QR";
  }
  if (X_test.isNotNull()) {
    /* Nullable output declaration */
    Rcpp::NumericMatrix X_test_tmp(X_test);
    
    // Transform Rcpp vector "vec_rcpp" into an Armadillo vector
    Eigen::MatrixXd X_test_ = Rcpp::as<Eigen::MatrixXd>(wrap(X_test_tmp));
    
    // run OLS with prediction
    if (type_ == "R") {
      out = rmridge_pred2(Y, X, X_test_, lambda);
    } else {
      out = qrmridge_pred2(Y, X, X_test_, lambda);
    }
  } else {
    // run OLS
    if (type_ == "R") {
      out = rmridge_pred1(Y, X, lambda);
    } else {
      out = qrmridge_pred1(Y, X, lambda);
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return out;
}

//' @name qrmridge_cv
//' @title Cross-validation of the RIDGE estimator for the linear multivariate regression model
//' @description qrmridge_cv, or LS for linear multivariate regression models, solves the following optimization problem
//' \deqn{\textrm{min}_\beta ~ \frac{1}{2}\|Y-XB\|_2^2,}
//' for \eqn{Y\in\mathbb{R}^{n \times q}} and \eqn{X\in\mathbb{R}^{n\times p}}, to obtain a coefficient matrix \eqn{\widehat{B}\in\mathbb{R}^{p\times q}}. The design matrix \eqn{X\in\mathbb{R}^{n\times p}}
//' contains the observations for each regressor.
//' @param Y a matrix of dimension \eqn{(n\times q} response variables.
//' @param X an \eqn{(n\times p)} full column rank matrix of predictors.
//' @param lambda a vector of lambdas.
//' @param k an integer vector defining the number of groups for CV.
//' @param seed ad integer number defining the seed for random number generation.
//' @param X_test an \eqn{(q\times p)} full column rank matrix. Test set. By default it set to NULL.
//' @param type either "QR" or "R". Specifies the type of decomposition to use: "QR" for the QR decomposition or "R" for the Cholesky factorization of \eqn{A^\top A}. The default is "QR".
//' @return A named list containing \describe{
//' \item{coeff}{a matrix of dimension \eqn{p\times q} containing the solution for the parameters \eqn{B}.}
//' \item{fitted}{a matrix of dimension \eqn{n\times q} of fitted values, \eqn{\widehat{Y}=X\widehat{B}}.}
//' \item{residuals}{a matrix of dimension \eqn{n\times q} of residuals, \eqn{\varepsilon=Y-\widehat{Y}}.}
//' \item{XTX}{the matrix \eqn{X^\top X}.}
//' \item{Sigma_hat}{a matrix of dimension \eqn{q\times q} containing the estimated  residual variance-covariance matrix.}
//' \item{df}{degrees of freedom.}
//' \item{R}{\eqn{R} matrix of the QR decomposition of the matrix \eqn{X^\top X}.}
//' \item{XTy}{\eqn{X^\top y}.}
//' \item{R2}{\eqn{R^2}, coefficient of determination, measure of goodness-of-fit of the model.}
//' \item{predicted}{predicted values for the test set, \eqn{X_{\text{test}}\widehat{B}}. It is only available if X_test is not NULL.}
//' \item{PMSE}{}
//' }
//'
//' @examples
//' ## generate sample data
//' set.seed(10)
//' n         <- 30
//' p         <- 6
//' q         <- 3
//' X         <- matrix(rnorm(n * p, 1), n, p)
//' X[,1]     <- 1
//' eps       <- matrix(rnorm(n*q), n, q)
//' B         <- matrix(0, p, q)
//' B[,1]     <- rep(1, p)
//' B[,2]     <- rep(2, p)
//' B[,3]     <- rep(-1, p)
//' Y         <- X %*% B + eps
//' X_test    <- matrix(rnorm(5 * p, 1), 5, p)
//' output    <- fastQR::qrmridge_cv(Y = Y, X = X, lambda = c(1,2),
//'                                  k = 5, seed = 12, X_test = X_test, type = "QR")
//' output$coeff
//' 

// [[Rcpp::export]]
Rcpp::List qrmridge_cv(Eigen::MatrixXd& Y,
                       Eigen::MatrixXd& X,
                       Eigen::VectorXd& lambda,
                       Rcpp::Nullable<int> k = R_NilValue,
                       Rcpp::Nullable<int> seed = R_NilValue,
                       Rcpp::Nullable<Rcpp::NumericMatrix> X_test = R_NilValue,
                       Rcpp::Nullable<std::string> type = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List out, res, output;
  int k_ = 10, seed_ = 1234, n_groups = 1;
  double ave_mse = 0.0, lambda_min = 0.0, mse = 0.0, mse_min = 0.0, R2 = 0.0;
  Eigen::Index mse_min_indi;
  std::string type_ = "QR";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  int n       = X.rows();
  int p       = X.cols();
  int q       = Y.cols();
  int nlambda = lambda.size();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd X_tilde(n + p, p);                    X_tilde.setZero();
  Eigen::MatrixXd D0(p, p);                             D0.setIdentity();
  Eigen::MatrixXd R0(p, p);                             R0.setZero();
  Eigen::VectorXd ave_mse_STORE(nlambda);               ave_mse_STORE.setZero();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL                                           */
  if (k.isNotNull()) {
    k_ = Rcpp::as<int>(k);
  } else {
    k_ = 10;
  }
  if (seed.isNotNull()) {
    seed_ = Rcpp::as<int>(seed);
  } else {
    seed_ = 1234;
  }
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
  } else {
    type_ = "QR";
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   define groups for CV                                         */
  res                         = groups_cv2(n, k_, seed_);
  arma::field<uvec> groups    = res["groups_pred"];
  arma::field<uvec> shuffle   = res["shuffle"];
  n_groups                    = res["n_groups"];
  arma::uvec groups_nelem     = res["groups_nelem"];
  arma::field<uvec> groups_cv = res["groups_all"];
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd Y_train0(n-groups_nelem(0), q);           Y_train0.setZero();
  Eigen::MatrixXd Y_test0(groups_nelem(0), q);              Y_test0.setZero();
  Eigen::MatrixXd X_train0(n-groups_nelem(0), p);           X_train0.setZero();
  Eigen::MatrixXd X_test0(groups_nelem(0), p);              X_test0.setZero();
  std::vector<int> idx_shuffle(n-groups_nelem(0));
  std::vector<int> idx_groups(groups_nelem(0));
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Run CV                                           */
  for (int j=0; j<nlambda; j++) {
    // print to screen
    Rcpp::Rcout << ">>> qrridge_cv, processing lambda: " << lambda(j) << "\n";
    
    // define the augmented matrix for all observations
    X_tilde << X, sqrt(lambda(j)) * D0;
    
    // compute initial R matrix
    res = householderQR(X_tilde, false);
    R0  = res["R"];
    
    // compute average predictive MSE
    ave_mse = 0.0;
    for (int i=0; i<n_groups; i++) {

      // convert to std vector
      idx_shuffle = arma::conv_to <std::vector<int>>::from(shuffle(i));
      idx_groups  = arma::conv_to <std::vector<int>>::from(groups(i));
      
      if (i == (n_groups-1)) {
        if (groups_nelem(n_groups-1) != groups_nelem(0)) {
          idx_shuffle.resize(n-groups_nelem(n_groups-1));
          idx_groups.resize(groups_nelem(n_groups-1));
        }
      }
      // subtract one
      std::for_each(idx_shuffle.begin(), idx_shuffle.end(), subtractone);
      std::for_each(idx_groups.begin(), idx_groups.end(), subtractone);
      if (i == (n_groups-1)) {
        if (groups_nelem(n_groups-1) != groups_nelem(0)) {
          Y_train0.resize(n-groups_nelem(n_groups-1), q);
          Y_test0.resize(groups_nelem(n_groups-1), q);
          X_train0.resize(n-groups_nelem(n_groups-1), p);
          X_test0.resize(groups_nelem(n_groups-1), p);
          
          Y_train0.setZero();
          Y_test0.setZero();
          X_train0.setZero();
          X_test0.setZero();
        }
      }
      Y_train0 = mat_slicing_byrow2(Y, idx_shuffle);
      Y_test0  = mat_slicing_byrow2(Y, idx_groups);
      X_train0 = mat_slicing_byrow2(X, idx_shuffle);
      X_test0  = mat_slicing_byrow2(X, idx_groups);
      // downdate R0
      out      = rmridge_downdate(Y_train0, X_train0,
                                  Y_test0, X_test0,
                                  lambda(j), R0,
                                  X_test0.transpose());
      mse      = out["PMSE"];
      ave_mse += mse;
    }
    ave_mse          /= (double)n_groups;
    ave_mse_STORE(j)  = ave_mse;
  }
  /* ::::::::::::::::::::::::::::::::::::::::::::::
    Post-process output                                       */
  mse_min    = ave_mse_STORE.array().minCoeff(&mse_min_indi);
  lambda_min = lambda(mse_min_indi);
  
  // print to screen
  Rcpp::Rcout << ">>> qrridge_cv, fitting for lambda: " << lambda_min << "\n";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
    Get the RIDGE estimate                                */
  if (X_test.isNotNull()) {
    /* Nullable output declaration */
    Rcpp::NumericMatrix X_test_tmp(X_test);
    
    // Transform Rcpp vector "vec_rcpp" into an Armadillo vector
    Eigen::MatrixXd X_test_ = Rcpp::as<Eigen::MatrixXd>(wrap(X_test_tmp));
    
    // run OLS with prediction
    if (type_ == "R") {
      out = rmridge_pred2(Y, X, X_test_, lambda_min);
    } else {
      Rcpp::Rcout << ">>> qua: " << lambda_min << "\n";
      out = qrmridge_pred2(Y, X, X_test_, lambda_min);
    }
  } else {
    // run OLS
    if (type_ == "R") {
      out = rmridge_pred1(Y, X, lambda_min);
    } else {
      out = qrmridge_pred1(Y, X, lambda_min);
    }
  }
  Rcpp::Rcout << ">>> qua1: " << lambda_min << "\n";
  Eigen::MatrixXd regp      = out["coeff"];
  Eigen::MatrixXd fitted    = out["fitted"];
  Eigen::MatrixXd resid     = out["residuals"];
  Eigen::MatrixXd XTX       = out["XTX"];
  Rcpp::Rcout << ">>> qua2: " << lambda_min << "\n";
  Eigen::MatrixXd XTY       = out["XTY"];
  Rcpp::Rcout << ">>> qua3: " << lambda_min << "\n";
  Eigen::MatrixXd Q         = out["Q"];
  Eigen::MatrixXd R         = out["R"];
  Eigen::MatrixXd Sigma_hat = out["Sigma_hat"];
  R2                        = out["R2"];
  Rcpp::Rcout << ">>> qua4: " << lambda_min << "\n";

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("lambda")      = lambda,
                              Rcpp::Named("mse")         = ave_mse_STORE,
                              Rcpp::Named("mse_min")     = mse_min,
                              Rcpp::Named("mse_min_ind") = mse_min_indi,
                              Rcpp::Named("lambda_min")  = lambda_min,
                              Rcpp::Named("coeff")       = regp,
                              Rcpp::Named("fitted")      = fitted,
                              Rcpp::Named("residuals")   = resid,
                              Rcpp::Named("XTX")         = XTX,
                              Rcpp::Named("XTY")         = XTY,
                              Rcpp::Named("Sigma_hat")   = Sigma_hat,
                              Rcpp::Named("df")          = q*(n-p),
                              Rcpp::Named("Q")           = Q,
                              Rcpp::Named("R")           = R,
                              Rcpp::Named("R2")          = R2);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}
