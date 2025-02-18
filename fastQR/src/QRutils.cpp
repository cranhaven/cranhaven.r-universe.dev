// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                                         :::::::::::::::::::: //
// ::::::::::::::::::::    QR, thin QR and L utils function     :::::::::::::::::::: //
// ::::::::::::::::::::                                         :::::::::::::::::::: //
// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //

// Utility routines

// Authors:
//           Bernardi Mauro, University of Padova
//           Last update: october 7, 2024

// [[Rcpp::depends(RcppArmadillo)]]
#include "QRutils.h"

// ::::::::::::::::::::::::::::::::::::::::::::::
// Cholesky factorization via QR factorization
// ::::::::::::::::::::::::::::::::::::::::::::::
Eigen::MatrixXd QRchol (const Eigen::MatrixXd& X) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List output;

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = X.rows();
  const int p = X.cols();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd R(p, p);          R.setZero();        // full R matrix
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (p > n) {
    Rcpp::warning("* qrchol : the number of columns of X is larger than the number of rows!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  output = householderQR(X, false);
  R      = output["R"];
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return R;
}

Eigen::MatrixXd rbQRchol (const Eigen::MatrixXd& X,
                          const int& nb) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List output;

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = X.rows();
  const int p = X.cols();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd R(p, p);          R.setZero();        // full R matrix
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (p > n) {
    Rcpp::warning("* rbqrchol : the number of columns of X is larger than the number of rows!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  //output = householderQR(X, false);
  output = rbQR(X, nb, false);
  R      = output["R"];
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return R;
}

// ::::::::::::::::::::::::::::::::::::::::::::::
// Solution of the system of equations Ax=b wrt x
// ::::::::::::::::::::::::::::::::::::::::::::::

Eigen::VectorXd QRsolve (const Eigen::MatrixXd& A,
                         const Eigen::VectorXd& b) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List output;

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = A.rows();
  const int p = A.cols();
  const int q = b.size();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Q(n, p);          Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);          R.setZero();          // full R matrix
  Eigen::VectorXd x(p);             x.setZero();          // full x vector
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    //Rcpp::stop("* qrsolve : A must be a square matrix!\n");
    Rcpp::stop("* qrsolve : the number of rows of A should be larger or equal to the number of columns of A!\n");
  }
  if (n != q) {
    Rcpp::stop("* qrsolve : the number of rows of A is not equal to the number of elements of b!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  output = householderQR(A, false);
  Q      = output["Q"];
  R      = output["R"];
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  x = R.triangularView<Upper>().solve(Q.transpose() * b);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return x;
}

Eigen::VectorXd rbQRsolve (const Eigen::MatrixXd& A,
                           const Eigen::VectorXd& b,
                           const int& nb) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List output;

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = A.rows();
  const int p = A.cols();
  const int q = b.size();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Q(n, p);          Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);          R.setZero();          // full R matrix
  Eigen::VectorXd x(p);             x.setZero();          // full x vector
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    //Rcpp::stop("* qrsolve : A must be a square matrix!\n");
    Rcpp::stop("* qrsolve : the number of rows of A should be larger or equal to the number of columns of A!\n");
  }
  if (n != q) {
    Rcpp::stop("* qrsolve : the number of rows of A is not equal to the number of elements of b!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  output = rbQR(A, nb, false);
  Q      = output["Q"];
  R      = output["R"];
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  x = R.triangularView<Upper>().solve(Q.transpose() * b);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return x;
}

Eigen::VectorXd Rsolve (const Eigen::MatrixXd& A,
                        const Eigen::VectorXd& b) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = A.rows();
  const int p = A.cols();
  const int q = b.size();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd R(p, p);          R.setZero();          // full R matrix
  Eigen::VectorXd x(p);             x.setZero();          // full x vector
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::stop("* rsolve : the number of rows of A should be larger or equal to the number of columns of A!\n");
  }
  if (n != q) {
    Rcpp::stop("* rsolve : the number of rows of A is not equal to the number of elements of b!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  R = householderR(A);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  x = R.triangularView<Upper>().solve(A.transpose() * b);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return x;
}

// ::::::::::::::::::::::::::::::::::::::::::::::
// Least squares:
// solution of the OLS normal equations
// ::::::::::::::::::::::::::::::::::::::::::::::
Rcpp::List qrls_pred1 (const Eigen::VectorXd& y,
                       const Eigen::MatrixXd& X) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n = X.rows();
  p = X.cols();
  q = y.size();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* qrls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* qrls_pred : the number of rows of X is not equal to the number of elements of y!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::VectorXd XTy(p);                       XTy.setZero();
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd Q(p, p);                      Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTy = X.transpose() * y / (double)n;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  res = householderQR(XTX, false);
  Q   = res["Q"];
  R   = res["R"];
  b   = Q.transpose() * XTy;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted  = X * regp;
  resid   = y - fitted;
  res2    = resid.norm();
  y_mean  = y.mean();
  y_norm2 = y.transpose() * y - n * std::pow(y_mean, 2);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
                              Rcpp::Named("fitted")          = fitted,
                              Rcpp::Named("residuals")       = resid,
                              Rcpp::Named("residuals_norm2") = res2,
                              Rcpp::Named("y_norm2")         = y_norm2,
                              Rcpp::Named("XTX")             = (double)n * XTX,
                              Rcpp::Named("XTy")             = (double)n * XTy,
                              Rcpp::Named("sigma2_hat")      = res2 / (n - p),
                              Rcpp::Named("df")              = n-p,
                              Rcpp::Named("Q")               = (double)n * Q,
                              Rcpp::Named("R")               = (double)n * R,
                              Rcpp::Named("QXTy")            = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2));
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List qrls_pred2 (const Eigen::VectorXd& y,
                       const Eigen::MatrixXd& X,
                       const Eigen::MatrixXd& X_test) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, n0 = 0, p0 = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  q  = y.size();
  n0 = X_test.rows();
  p0 = X_test.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* qrls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* qrls_pred : the number of rows of X is not equal to the number of elements of y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* qrls_pred : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::VectorXd XTy(p);                       XTy.setZero();
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd Q(p, p);                      Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::VectorXd predicted(n0);                predicted.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTy = X.transpose() * y / (double)n;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  res = householderQR(XTX, false);
  Q   = res["Q"];
  R   = res["R"];
  b   = Q.transpose() * XTy;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = y - fitted;
  res2      = resid.norm();
  y_mean    = y.mean();
  y_norm2   = y.transpose() * y - n * std::pow(y_mean, 2);
  predicted = X_test * regp;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
                              Rcpp::Named("fitted")          = fitted,
                              Rcpp::Named("residuals")       = resid,
                              Rcpp::Named("residuals_norm2") = res2,
                              Rcpp::Named("y_norm2")         = y_norm2,
                              Rcpp::Named("XTX")             = (double)n * XTX,
                              Rcpp::Named("XTy")             = (double)n * XTy,
                              Rcpp::Named("sigma2_hat")      = res2 / (n - p),
                              Rcpp::Named("df")              = n-p,
                              Rcpp::Named("Q")               = (double)n * Q,
                              Rcpp::Named("R")               = (double)n * R,
                              Rcpp::Named("QXTy")            = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2),
                              Rcpp::Named("predicted")       = predicted);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rls_pred1 (const Eigen::VectorXd& y,
                      const Eigen::MatrixXd& X) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0;
  Rcpp::List output;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  q  = y.size();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* rls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* rls_pred : the number of rows of X is not equal to the number of elements of y!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X);
  b = X.transpose() * y;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = y - fitted;
  res2      = resid.norm();
  y_mean    = y.mean();
  y_norm2   = y.transpose() * y - n * std::pow(y_mean, 2);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
                              Rcpp::Named("fitted")          = fitted,
                              Rcpp::Named("residuals")       = resid,
                              Rcpp::Named("residuals_norm2") = res2,
                              Rcpp::Named("y_norm2")         = y_norm2,
                              Rcpp::Named("XTX")             = R.transpose() * R,
                              Rcpp::Named("sigma2_hat")      = res2 / (n - p),
                              Rcpp::Named("df")              = n-p,
                              Rcpp::Named("R")               = R,
                              Rcpp::Named("QTy")             = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2));
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rls_pred2 (const Eigen::VectorXd& y,
                      const Eigen::MatrixXd& X,
                      const Eigen::MatrixXd& X_test) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, n0 = 0, p0 = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  q  = y.size();
  n0 = X_test.rows();
  p0 = X_test.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* rls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* rls_pred : the number of rows of X is not equal to the number of elements of y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* rls_pred : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::VectorXd predicted(n0);                predicted.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X);
  b = X.transpose() * y;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = y - fitted;
  res2      = resid.norm();
  y_mean    = y.mean();
  y_norm2   = y.transpose() * y - n * std::pow(y_mean, 2);
  predicted = X_test * regp;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
                              Rcpp::Named("fitted")          = fitted,
                              Rcpp::Named("residuals")       = resid,
                              Rcpp::Named("residuals_norm2") = res2,
                              Rcpp::Named("y_norm2")         = y_norm2,
                              Rcpp::Named("XTX")             = R.transpose() * R,
                              Rcpp::Named("sigma2_hat")      = res2 / (n - p),
                              Rcpp::Named("df")              = n-p,
                              Rcpp::Named("R")               = R,
                              Rcpp::Named("QTy")             = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2),
                              Rcpp::Named("predicted")       = predicted);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

// ::::::::::::::::::::::::::::::::::::::::::::::
// RIDGE:
// solution of the RIDGE normal equations
// ::::::::::::::::::::::::::::::::::::::::::::::
Rcpp::List qrridge_pred1 (const Eigen::VectorXd& y,
                          const Eigen::MatrixXd& X,
                          const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  q  = y.size();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* qrridge_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* qrridge_pred : the number of rows of X is not equal to the number of elements of y!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::VectorXd XTy(p);                       XTy.setZero();
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd Q(p, p);                      Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd D(p, p);                      D.setIdentity();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTy = X.transpose() * y / (double)n;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  res = householderQR(XTX + lambda * D, false);
  Q   = res["Q"];
  R   = res["R"];
  b   = Q.transpose() * XTy;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = y - fitted;
  res2      = resid.norm();
  y_mean    = y.mean();
  y_norm2   = y.transpose() * y - n * std::pow(y_mean, 2);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
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
                              Rcpp::Named("QXTy")            = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2));
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List qrridge_pred2 (const Eigen::VectorXd& y,
                          const Eigen::MatrixXd& X,
                          const Eigen::MatrixXd& X_test,
                          const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, n0 = 0, p0 = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  q  = y.size();
  n0 = X_test.rows();
  p0 = X_test.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* qrridge_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* qrridge_pred : the number of rows of X is not equal to the number of elements of y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* qrridge_pred : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::VectorXd XTy(p);                       XTy.setZero();
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd Q(p, p);                      Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::VectorXd predicted(n0);                predicted.setZero();
  Eigen::MatrixXd D(p, p);                      D.setIdentity();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTy = X.transpose() * y / (double)n;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  //D.diagonal() = repelem(lambda, p);
  res          = householderQR(XTX + lambda * D, false);
  Q            = res["Q"];
  R            = res["R"];
  b            = Q.transpose() * XTy;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = y - fitted;
  res2      = resid.norm();
  y_mean    = y.mean();
  y_norm2   = y.transpose() * y - n * std::pow(y_mean, 2);
  predicted = X_test * regp;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
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
                              Rcpp::Named("QXTy")            = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2),
                              Rcpp::Named("predicted")       = predicted);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Eigen::MatrixXd rridge_R (const Eigen::MatrixXd& X,
                          const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* rridge_R : the number of rows of X is less than the number of columns of X!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd X_tilde(n + p, p);            X_tilde.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd D1(p, p);                     D1.setIdentity();

  // Create a new matrix with enough space for rows of A and B
  D1.diagonal().array()       = std::sqrt(lambda);
  X_tilde.block(0, 0, n, p)   = X;
  X_tilde.block(n, 0, n+p, p) = D1;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X_tilde);

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return R;
}

Rcpp::List rridge_pred1 (const Eigen::VectorXd& y,
                         const Eigen::MatrixXd& X,
                         const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0;
  Rcpp::List output;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                     */
  n  = X.rows();
  p  = X.cols();
  q  = y.size();
 
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                                               */
  if (n < p) {
    Rcpp::warning("* rridge_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* rridge_pred : the number of rows of X is not equal to the number of elements of y!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd X_tilde(n + p, p);            X_tilde.setZero();
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd D1(p, p);                     D1.setIdentity();

  // Create a new matrix with enough space for rows of A and B
  D1.diagonal().array()       = std::sqrt(lambda);
  X_tilde.block(0, 0, n, p)   = X;
  X_tilde.block(n, 0, n+p, p) = D1;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X_tilde);
  b = X.transpose() * y;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = y - fitted;
  res2      = resid.norm();
  y_mean    = y.mean();
  y_norm2   = y.transpose() * y - n * std::pow(y_mean, 2);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
                              Rcpp::Named("fitted")          = fitted,
                              Rcpp::Named("residuals")       = resid,
                              Rcpp::Named("residuals_norm2") = res2,
                              Rcpp::Named("y_norm2")         = y_norm2,
                              Rcpp::Named("XTX")             = R.transpose() * R,
                              Rcpp::Named("sigma2_hat")      = res2 / (n - p),
                              Rcpp::Named("df")              = n-p,
                              Rcpp::Named("R")               = R,
                              Rcpp::Named("QTy")             = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2));
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rridge_pred2 (const Eigen::VectorXd& y,
                         const Eigen::MatrixXd& X,
                         const Eigen::MatrixXd& X_test,
                         const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, n0 = 0, p0 = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0;
  Rcpp::List output;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  q  = y.size();
  n0 = X_test.rows();
  p0 = X_test.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* rridge_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* rridge_pred : the number of rows of X is not equal to the number of elements of y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* rridge_pred : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd X_tilde(n + p, p);            X_tilde.setZero();
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::VectorXd predicted(n0);                predicted.setZero();
  Eigen::MatrixXd D1(p, p);                     D1.setIdentity();

  // Create a new matrix with enough space for rows of A and B
  D1.diagonal().array()       = std::sqrt(lambda);
  X_tilde.block(0, 0, n, p)   = X;
  X_tilde.block(n, 0, n+p, p) = D1;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X_tilde);
  b = X.transpose() * y;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = y - fitted;
  res2      = resid.norm();
  y_mean    = y.mean();
  y_norm2   = y.transpose() * y - n * std::pow(y_mean, 2);
  predicted = X_test * regp;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
                              Rcpp::Named("fitted")          = fitted,
                              Rcpp::Named("residuals")       = resid,
                              Rcpp::Named("residuals_norm2") = res2,
                              Rcpp::Named("y_norm2")         = y_norm2,
                              Rcpp::Named("XTX")             = R.transpose() * R,
                              Rcpp::Named("sigma2_hat")      = res2 / (n - p),
                              Rcpp::Named("df")              = n-p,
                              Rcpp::Named("R")               = R,
                              Rcpp::Named("QTy")             = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2),
                              Rcpp::Named("predicted")       = predicted);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rridge_downdate (const Eigen::VectorXd& y,
                            const Eigen::MatrixXd& X,
                            const Eigen::VectorXd& y_test,
                            const Eigen::MatrixXd& X_test,
                            const double lambda,
                            const Eigen::MatrixXd& R,
                            const Eigen::MatrixXd& U) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, n0 = 0, p0 = 0;
  double res2 = 0.0, y_norm2 = 0.0, y_mean = 0.0, mse = 0.0;
  Rcpp::List output;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  q  = y.size();
  n0 = X_test.rows();
  p0 = X_test.cols();
 
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* rridge_downdate : the number of rows of X is less than the number of columns of X!\n");
  }
  if (n != q) {
    Rcpp::stop("* rridge_downdate : the number of rows of X is not equal to the number of elements of y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* rridge_downdate : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::VectorXd regp(p);                      regp.setZero();
  Eigen::VectorXd resid(p);                     resid.setZero();
  Eigen::VectorXd fitted(n);                    fitted.setZero();
  Eigen::VectorXd b(p);                         b.setZero();
  Eigen::MatrixXd R_update(p, p);               R_update.setZero();          // full R matrix
  Eigen::VectorXd predicted(n0);                predicted.setZero();
  Eigen::VectorXd diff(n0);                     diff.setZero();
  Eigen::VectorXd squareddiff(n0);              squareddiff.setZero();
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R_update = thinqrdeletemrows(R, U.transpose());

  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  b    = X.transpose() * y;
  regp = R_update.triangularView<Upper>().solve(b);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted      = X * regp;
  resid       = y - fitted;
  res2        = resid.norm();
  y_mean      = y.mean();
  y_norm2     = y.transpose() * y - n * std::pow(y_mean, 2);
  predicted   = X_test * regp;
  diff        = predicted - y_test;
  squareddiff = diff.array().square();
  mse         = (diff.array().square()).mean();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")           = regp,
                              Rcpp::Named("fitted")          = fitted,
                              Rcpp::Named("residuals")       = resid,
                              Rcpp::Named("residuals_norm2") = res2,
                              Rcpp::Named("y_norm2")         = y_norm2,
                              Rcpp::Named("XTX")             = R.transpose() * R,
                              Rcpp::Named("sigma2_hat")      = res2 / (n - p),
                              Rcpp::Named("df")              = n-p,
                              Rcpp::Named("R")               = R_update,
                              Rcpp::Named("XTy")             = b,
                              Rcpp::Named("R2")              = 1.0 - (res2 / y_norm2),
                              Rcpp::Named("predicted")       = predicted,
                              Rcpp::Named("PMSE")            = mse);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

// ::::::::::::::::::::::::::::::::::::::::::::::
// Multivariate least squares:
// solution of the OLS normal equations
// ::::::::::::::::::::::::::::::::::::::::::::::

double multivariateR2 (const Eigen::MatrixXd& Y,
                       const Eigen::MatrixXd& X,
                       const Eigen::MatrixXd& B) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  double trace_T = 0.0, trace_E = 0.0, R2 = 0.0;
  int n = 0, q = 0;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n = X.rows();
  q = Y.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd Y_centered(n, q);             Y_centered.setZero();
  Eigen::MatrixXd Y_HAT(n, q);                  Y_HAT.setZero();
  Eigen::RowVectorXd Y_mean(q);                 Y_mean.setZero();
  Eigen::MatrixXd E(n, q);                      E.setZero();
  Eigen::MatrixXd T_sscp(q, q);                 T_sscp.setZero();
  Eigen::MatrixXd E_sscp(q, q);                 E_sscp.setZero();
  
  // Compute column means of Y
  Y_mean = Y.colwise().mean();
  
  // Center Y around its mean
  Y_centered = Y.rowwise() - Y_mean;
  
  // Total sum of squares and cross-products (SSCP) matrix (T)
  T_sscp = Y_centered.transpose() * Y_centered;
  
  // Compute fitted values: Y_hat = X * B
  Y_HAT = X * B;
  
  // Compute residuals: E = Y - Y_hat
  E = Y - Y_HAT;
  
  // residuals sum of squares and cross-products SSCP matrix (E)
  E_sscp = E.transpose() * E;
  
  // Compute R^2 = 1 - trace(E) / trace(T)
  trace_T = T_sscp.trace();
  trace_E = E_sscp.trace();
  R2      = 1.0 - (trace_E / trace_T);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return R2;
}

double multivariatePMSE (const Eigen::MatrixXd& Y,
                         const Eigen::MatrixXd& X,
                         const Eigen::MatrixXd& B) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  double pmse = 0.0;
  int n = 0, q = 0;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n = X.rows();
  q = Y.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd Y_pred(n, q);                 Y_pred.setZero();
  Eigen::MatrixXd residuals(n, q);              residuals.setZero();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   compute predictive MSE                     */
  
  // Step 1: Predict responses for the test set
  Y_pred = X * B;

  // Step 2: Compute residuals
  residuals = Y - Y_pred;

  // Step 4: Compute MSE
  pmse = (residuals.array().square().sum()) / (n * q);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return pmse;
}

Rcpp::List qrmls_pred1 (const Eigen::MatrixXd& Y,
                        const Eigen::MatrixXd& X) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, N = 0;
  double R2 = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n = X.rows();
  p = X.cols();
  N = Y.rows();
  q = Y.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* qrmls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* qrmls_pred : the number of rows of X is not equal to those of Y!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::MatrixXd XTY(p, q);                    XTY.setZero();
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd Q(p, p);                      Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTY = X.transpose() * Y / (double)n;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  res = householderQR(XTX, false);
  Q   = res["Q"];
  R   = res["R"];
  B   = Q.transpose() * XTY;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(B);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted = X * regp;
  resid  = Y - fitted;
  R2     = multivariateR2(Y, X, regp);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = (double)n * XTX,
                              Rcpp::Named("XTy")       = (double)n * XTY,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("Q")         = (double)n * Q,
                              Rcpp::Named("R")         = (double)n * R,
                              Rcpp::Named("QXTy")      = B,
                              Rcpp::Named("R2")        = R2);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List qrmls_pred2 (const Eigen::MatrixXd& Y,
                        const Eigen::MatrixXd& X,
                        const Eigen::MatrixXd& X_test) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, N = 0, p0 = 0, n0 = 0;
  double R2 = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  N  = Y.rows();
  q  = Y.cols();
  n0 = X_test.rows();
  p0 = X_test.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* qrmls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* qrmls_pred : the number of rows of X is not equal to those of Y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* qrls_pred : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::MatrixXd XTY(p, q);                    XTY.setZero();
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd Q(p, p);                      Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  Eigen::MatrixXd predicted(n0, q);             predicted.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTY = X.transpose() * Y / (double)n;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  res = householderQR(XTX, false);
  Q   = res["Q"];
  R   = res["R"];
  B   = Q.transpose() * XTY;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(B);  // enable fast mode
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = Y - fitted;
  R2        = multivariateR2(Y, X, regp);
  predicted = X_test * regp;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = (double)n * XTX,
                              Rcpp::Named("XTy")       = (double)n * XTY,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("Q")         = (double)n * Q,
                              Rcpp::Named("R")         = (double)n * R,
                              Rcpp::Named("QXTy")      = B,
                              Rcpp::Named("R2")        = R2,
                              Rcpp::Named("predicted") = predicted);
    
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rmls_pred1 (const Eigen::MatrixXd& Y,
                       const Eigen::MatrixXd& X) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, N = 0;
  double R2 = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n = X.rows();
  p = X.cols();
  N = Y.rows();
  q = Y.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* rmls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* rmls_pred : the number of rows of X is not equal to those of Y!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::MatrixXd XTY(p, q);                    XTY.setZero();
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTY = X.transpose() * Y / (double)n;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X);
  B = XTY;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(B);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted = X * regp;
  resid  = Y - fitted;
  R2     = multivariateR2(Y, X, regp);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = R.transpose() * R,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("R")         = R,
                              Rcpp::Named("XTy")       = B,
                              Rcpp::Named("R2")        = R2);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rmls_pred2 (const Eigen::MatrixXd& Y,
                       const Eigen::MatrixXd& X,
                       const Eigen::MatrixXd& X_test) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, N = 0, n0 = 0, p0 = 0;
  double R2 = 0.0, pmse = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n = X.rows();
  p = X.cols();
  N = Y.rows();
  q = Y.cols();
  n0 = X_test.rows();
  p0 = X_test.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* rmls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* rmls_pred : the number of rows of X is not equal to those of Y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* qrls_pred : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::MatrixXd XTY(p, q);                    XTY.setZero();
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  Eigen::MatrixXd predicted(n0, q);             predicted.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTY = X.transpose() * Y / (double)n;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X);
  B = XTY;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(B);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = Y - fitted;
  R2        = multivariateR2(Y, X, regp);
  predicted = X_test * regp;
  pmse      = multivariatePMSE(Y, X, regp);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = R.transpose() * R,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("R")         = R,
                              Rcpp::Named("XTy")       = B,
                              Rcpp::Named("R2")        = R2,
                              Rcpp::Named("predicted") = predicted,
                              Rcpp::Named("PMSE")      = pmse);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List qrmridge_pred1 (const Eigen::MatrixXd& Y,
                           const Eigen::MatrixXd& X,
                           const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, N = 0;
  double R2 = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                   */
  n = X.rows();
  p = X.cols();
  N = Y.rows();
  q = Y.cols();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* qrmls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* qrmls_pred : the number of rows of X is not equal to those of Y!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::MatrixXd XTY(p, q);                    XTY.setZero();
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd Q(p, p);                      Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  Eigen::MatrixXd D(p, p);                      D.setIdentity();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTY = X.transpose() * Y / (double)n;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  res = householderQR(XTX + lambda * D, false);
  Q   = res["Q"];
  R   = res["R"];
  B   = Q.transpose() * XTY;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(B);

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                                        */
  fitted = X * regp;
  resid  = Y - fitted;
  R2     = multivariateR2(Y, X, regp);

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = (double)n * XTX,
                              Rcpp::Named("XTy")       = (double)n * XTY,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("Q")         = (double)n * Q,
                              Rcpp::Named("R")         = (double)n * R,
                              Rcpp::Named("XTy")       = B,                                    // attenzione e' QXTy
                              Rcpp::Named("R2")        = R2);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List qrmridge_pred2 (const Eigen::MatrixXd& Y,
                           const Eigen::MatrixXd& X,
                           const Eigen::MatrixXd& X_test,
                           const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, N = 0, n0 = 0, p0 = 0;
  double R2 = 0.0, pmse = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                   */
  n  = X.rows();
  p  = X.cols();
  N  = Y.rows();
  q  = Y.cols();
  n0 = X_test.rows();
  p0 = X_test.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* qrmls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* qrmls_pred : the number of rows of X is not equal to those of Y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* qrls_pred : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd XTX(p, p);                    XTX.setZero();
  Eigen::MatrixXd XTY(p, q);                    XTY.setZero();
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd Q(p, p);                      Q.setZero();          // full Q matrix
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  Eigen::MatrixXd D(p, p);                      D.setIdentity();
  Eigen::MatrixXd predicted(n0, q);             predicted.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Get relevant quantities                              */
  XTX = X.transpose() * X / (double)n;
  XTY = X.transpose() * Y / (double)n;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  res = householderQR(XTX + lambda * D, false);
  Q   = res["Q"];
  R   = res["R"];
  B   = Q.transpose() * XTY;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(B);

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                                        */
  fitted    = X * regp;
  resid     = Y - fitted;
  R2        = multivariateR2(Y, X, regp);
  predicted = X_test * regp;
  pmse      = multivariatePMSE(Y, X, regp);

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = (double)n * XTX,
                              Rcpp::Named("XTy")       = (double)n * XTY,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("Q")         = (double)n * Q,
                              Rcpp::Named("R")         = (double)n * R,
                              Rcpp::Named("XTY")       = B,                                   // attenzione e' QXTy
                              Rcpp::Named("R2")        = R2,
                              Rcpp::Named("predicted") = predicted,
                              Rcpp::Named("PMSE")      = pmse);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rmridge_pred1 (const Eigen::MatrixXd& Y,
                          const Eigen::MatrixXd& X,
                          const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, N = 0;
  double R2 = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                   */
  n  = X.rows();
  p  = X.cols();
  N  = Y.rows();
  q  = Y.cols();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                                               */
  if (n < p) {
    Rcpp::warning("* qrmls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* qrmls_pred : the number of rows of X is not equal to those of Y!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd X_tilde(n + p, p);            X_tilde.setZero();
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd D1(p, p);                     D1.setIdentity();
  
  // Create a new matrix with enough space for rows of A and B
  D1.diagonal().array()       = std::sqrt(lambda);
  X_tilde.block(0, 0, n, p)   = X;
  X_tilde.block(n, 0, n+p, p) = D1;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X_tilde);
  B = X.transpose() * Y;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(B);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted = X * regp;
  resid  = Y - fitted;
  R2     = multivariateR2(Y, X, regp);
    
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = R.transpose() * R,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("R")         = R,
                              Rcpp::Named("XTy")       = B,
                              Rcpp::Named("R2")        = R2);
    
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rmridge_pred2 (const Eigen::MatrixXd& Y,
                          const Eigen::MatrixXd& X,
                          const Eigen::MatrixXd& X_test,
                          const double lambda) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, N = 0, n0 = 0, p0 = 0;
  double R2 = 0.0, pmse = 0.0;
  Rcpp::List output, res;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                   */
  n  = X.rows();
  p  = X.cols();
  N  = Y.rows();
  q  = Y.cols();
  n0 = X_test.rows();
  p0 = X_test.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                                               */
  if (n < p) {
    Rcpp::warning("* qrmls_pred : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* qrmls_pred : the number of rows of X is not equal to those of Y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* qrls_pred : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd X_tilde(n + p, p);            X_tilde.setZero();
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd R(p, p);                      R.setZero();          // full R matrix
  Eigen::MatrixXd D1(p, p);                     D1.setIdentity();
  Eigen::MatrixXd predicted(n0, q);             predicted.setZero();
  
  // Create a new matrix with enough space for rows of A and B
  D1.diagonal().array()       = std::sqrt(lambda);
  X_tilde.block(0, 0, n, p)   = X;
  X_tilde.block(n, 0, n+p, p) = D1;
    
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R = householderR(X_tilde);
  B = X.transpose() * Y;
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  regp = R.triangularView<Upper>().solve(B);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = Y - fitted;
  R2        = multivariateR2(Y, X, regp);
  predicted = X_test * regp;
  pmse      = multivariatePMSE(Y, X, regp);
    
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = R.transpose() * R,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("R")         = R,
                              Rcpp::Named("XTy")       = B,
                              Rcpp::Named("R2")        = R2,
                              Rcpp::Named("predicted") = predicted,
                              Rcpp::Named("PMSE")      = pmse);
    
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

Rcpp::List rmridge_downdate (const Eigen::MatrixXd& Y,
                             const Eigen::MatrixXd& X,
                             const Eigen::MatrixXd& Y_test,
                             const Eigen::MatrixXd& X_test,
                             const double lambda,
                             const Eigen::MatrixXd& R,
                             const Eigen::MatrixXd& U) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0, p = 0, q = 0, n0 = 0, p0 = 0, N = 0;
  double pmse = 0.0, R2 = 0.0;
  Rcpp::List output;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n  = X.rows();
  p  = X.cols();
  q  = Y.cols();
  N  = Y.rows();
  n0 = X_test.rows();
  p0 = X_test.cols();
 
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   checks                              */
  if (n < p) {
    Rcpp::warning("* rmridge_downdate : the number of rows of X is less than the number of columns of X!\n");
  }
  if (N != n) {
    Rcpp::stop("* rmridge_downdate : the number of rows of X is not equal to those of Y!\n");
  }
  if (p0 != p) {
    Rcpp::stop("* rmridge_downdate : dimension of X and X_test not conformable!\n");
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::MatrixXd regp(p, q);                   regp.setZero();
  Eigen::MatrixXd resid(p, q);                  resid.setZero();
  Eigen::MatrixXd fitted(n, q);                 fitted.setZero();
  Eigen::MatrixXd B(p, q);                      B.setZero();
  Eigen::MatrixXd R_update(p, p);               R_update.setZero();          // full R matrix
  Eigen::MatrixXd predicted(n0, q);             predicted.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform QR update of XTX                  */
  R_update = thinqrdeletemrows(R, U.transpose());

  /* :::::::::::::::::::::::::::::::::::::::::
   Compute the vector of regression parameters     */
  B    = X.transpose() * Y;
  regp = R_update.triangularView<Upper>().solve(B);  // enable fast mode

  /* :::::::::::::::::::::::::::::::::::::::::
   Get output                        */
  fitted    = X * regp;
  resid     = Y - fitted;
  predicted = X_test * regp;
  pmse      = multivariatePMSE(Y_test, X_test, regp);
  R2        = multivariateR2(Y, X, regp);

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get output                                           */
  output = Rcpp::List::create(Rcpp::Named("coeff")     = regp,
                              Rcpp::Named("fitted")    = fitted,
                              Rcpp::Named("residuals") = resid,
                              Rcpp::Named("XTX")       = R.transpose() * R,
                              Rcpp::Named("Sigma_hat") = resid.transpose() * resid / (n - p),
                              Rcpp::Named("df")        = q * (n-p),
                              Rcpp::Named("R")         = R_update,
                              Rcpp::Named("XTY")       = B,
                              Rcpp::Named("R2")        = R2,
                              Rcpp::Named("predicted") = predicted,
                              Rcpp::Named("PMSE")      = pmse);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}













// end of file
