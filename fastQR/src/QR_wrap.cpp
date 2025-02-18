// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                 :::::::::::::::::::: //
// ::::::::::::::::::::    QR wrap      :::::::::::::::::::: //
// ::::::::::::::::::::                 :::::::::::::::::::: //
// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //

#include "QRdecomposition.h"
#include "QRupdate.h"

#define EIGEN_USE_BLAS
#define EIGEN_USE_LAPACKE

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(Rcpp)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace Eigen;
using namespace arma;

//' @name qr
//' @title The QR factorization of a matrix
//' @description qr provides the QR factorization of the matrix \eqn{X\in\mathbb{R}^{n\times p}} with \eqn{n>p}. The QR factorization of the matrix \eqn{X} returns the matrices \eqn{Q\in\mathbb{R}^{n\times n}} and \eqn{R\in\mathbb{R}^{n\times p}} such that \eqn{X=QR}. See Golub and Van Loan (2013) for further details on the method.
//' @param X a \eqn{n\times p} matrix.
//' @param type either "givens" or "householder".
//' @param nb integer. Defines the number of block in the block recursive QR decomposition. See Golud and van Loan (2013).
//' @param complete logical expression of length 1. Indicates whether an arbitrary orthogonal completion of the \eqn{Q} matrix is to be made, or whether the \eqn{R} matrix is to be completed by binding zero-value rows beneath the square upper triangle.
//' @return A named list containing \describe{
//' \item{Q}{the Q matrix.}
//' \item{R}{the R matrix.}
//' }
//'
//' @examples
//' ## generate sample data
//' set.seed(1234)
//' n <- 10
//' p <- 6
//' X <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## QR factorization via Givens rotation
//' output <- qr(X, type = "givens", complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//'
//' ## check
//' round(Q %*% R - X, 5)
//' max(abs(Q %*% R - X))
//'
//' ## QR factorization via Householder rotation
//' output <- qr(X, type = "householder", complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//'
//' ## check
//' round(Q %*% R - X, 5)
//' max(abs(Q %*% R - X))
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
Rcpp::List qr(const Eigen::MatrixXd& X,
              Rcpp::Nullable<bool> complete = R_NilValue,
              Rcpp::Nullable<std::string> type = R_NilValue,
              Rcpp::Nullable<int> nb = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List output;
  int nb_ = 0;
  bool complete_ = false;
  std::string type_ = "householder";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = X.rows();
  const int p = X.cols();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (nb.isNotNull()) {
    nb_ = Rcpp::as<int>(nb);
    /* recursive block update is a valid option only when n>p */
    if (p > n) {
      nb_ = 0;
      warning("* fastQR : the parameter nb is set to NULL if p > n!\n");
    }
  } else {
    nb_ = 0;
  }
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
  } else {
    type_ = "householder";
  }
  if (complete.isNotNull()) {
    complete_ = Rcpp::as<bool>(complete);
  } else {
    complete_ = false;
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   QR factorization                                   */
  if (nb_ == 0) {
    if (type_ == "householder") {
      //Rcpp::Rcout << "QR decomposition using Householder reflections.\n";
      output = householderQR(X, complete_);
    } else if (type_ == "givens") {
      //Rcpp::Rcout << "QR decomposition using Givens rotations.\n";
      output = givensQR(X, complete_);
    }
  } else {
    //Rcpp::Rcout << "Recursive block QR decomposition.\n";
    output = rbQR(X, nb_, complete_);
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return output;
}

//' @name qrupdate
//' @title Fast updating of the QR factorization
//' @description qrupdate provides the update of the QR factorization after the addition of \eqn{m>1} rows or columns to the matrix \eqn{X\in\mathbb{R}^{n\times p}} with \eqn{n>p}. The QR factorization of the matrix \eqn{X} returns the matrices \eqn{Q\in\mathbb{R}^{n\times n}} and \eqn{R\in\mathbb{R}^{n\times p}} such that \eqn{X=QR}. The \eqn{Q} and \eqn{R} matrices are factorized as \eqn{Q=\begin{bmatrix}Q_1&Q_2\end{bmatrix}} and \eqn{R=\begin{bmatrix}R_1\\R_2\end{bmatrix}}, with \eqn{Q_1\in\mathbb{R}^{n\times p}}, \eqn{Q_2\in\mathbb{R}^{n\times (n-p)}} such that \eqn{Q_1^{\top}Q_2=Q_2^\top Q_1=0} and \eqn{R_1\in\mathbb{R}^{p\times p}} upper triangular matrix and \eqn{R_2\in\mathbb{R}^{(n-p)\times p}}. qrupdate accepts in input the matrices \eqn{Q} and either the complete matrix \eqn{R} or the reduced one, \eqn{R_1}. See Golub and Van Loan (2013) for further details on the method.
//' @param Q a \eqn{n\times p} matrix.
//' @param R a \eqn{p\times p} upper triangular matrix.
//' @param k position where the columns or the rows are added.
//' @param U either a \eqn{n\times m} matrix or a \eqn{p\times m} matrix of columns or rows to be added.
//' @param type either 'row' of 'column', for adding rows or columns. Default is 'column'.
//' @param fast fast mode: disable to check whether the provided matrices are valid inputs. Default is FALSE.
//' @param complete logical expression of length 1. Indicates whether an arbitrary orthogonal completion of the \eqn{Q} matrix is to be made, or whether the \eqn{R} matrix is to be completed by binding zero-value rows beneath the square upper triangle.
//' @return A named list containing \describe{
//' \item{Q}{the updated Q matrix.}
//' \item{R}{the updated R matrix.}
//' }
//'
//' @examples
//' ## Add one column
//' ## generate sample data
//' set.seed(1234)
//' n <- 12
//' p <- 5
//' X <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- qr(X, complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//'
//' ## create column u to be added
//' k  <- p+1
//' u  <- matrix(rnorm(n), n, 1)
//' X1 <- cbind(X, u)
//'
//' ## update the QR decomposition
//'out <- fastQR::qrupdate(Q = Q, R = R,
//'                        k = k, U = u,
//'                        type = "column",
//'                        fast = FALSE,
//'                        complete = TRUE)
//'
//' ## check
//' round(out$Q %*% out$R - X1, 5)
//' max(abs(out$Q %*% out$R - X1))
//'
//' ## Add m columns
//' ## create data: n > p
//' set.seed(1234)
//' n <- 10
//' p <- 5
//' X <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- fastQR::qr(X, complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//'
//' ## create the matrix of two columns to be added
//' ## in position 2
//' k  <- 2
//' m  <- 2
//' U  <- matrix(rnorm(n*m), n, m)
//' X1 <- cbind(X[,1:(k-1)], U, X[,k:p])
//'
//' # update the QR decomposition
//' out <- fastQR::qrupdate(Q = Q, R = R,
//'                         k = k, U = U, type = "column",
//'                        fast = FALSE, complete = TRUE)
//'
//' ## check
//' round(out$Q %*% out$R - X1, 5)
//' max(abs(out$Q %*% out$R - X1))
//'
//' ## Add one row
//' ## create data: n > p
//' set.seed(1234)
//' n <- 12
//' p <- 5
//' X <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- fastQR::qr(X, complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//' R1     <- R[1:p,]
//'
//' ## create the row u to be added
//' u  <- matrix(data = rnorm(p), p, 1)
//' k  <- n+1
//' if (k<=n) {
//'   X1 <- rbind(rbind(X[1:(k-1), ], t(u)), X[k:n, ])
//' } else {
//'   X1 <- rbind(rbind(X, t(u)))
//' }
//'
//' ## update the QR decomposition
//' out <- fastQR::qrupdate(Q = Q, R = R,
//'                         k = k, U = u,
//'                         type = "row",
//'                         complete = TRUE)
//'
//' ## check
//' round(out$Q %*% out$R - X1, 5)
//' max(abs(out$Q %*% out$R - X1))
//'
//' ## Add m rows
//' ## create data: n > p
//' set.seed(1234)
//' n <- 12
//' p <- 5
//' X <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- fastQR::qr(X, complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//' R1     <- R[1:p,]
//'
//' ## create the matrix of rows U to be added:
//' ## two rows in position 5
//' m  <- 2
//' U  <- matrix(data = rnorm(p*m), p, m)
//' k  <- 5
//' if (k<=n) {
//'   X1 <- rbind(rbind(X[1:(k-1), ], t(U)), X[k:n, ])
//' } else {
//'   X1 <- rbind(rbind(X, t(U)))
//' }
//'
//' ## update the QR decomposition
//' out <- fastQR::qrupdate(Q = Q, R = R,
//'                         k = k, U = U,
//'                         type = "row",
//'                         complete = FALSE)
//'
//' ## check
//' round(out$Q %*% out$R - X1, 5)
//' max(abs(out$Q %*% out$R - X1))
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
Rcpp::List qrupdate(const Eigen::MatrixXd& Q,
                    const Eigen::MatrixXd& R,
                    const int k,
                    const Eigen::MatrixXd& U,
                    Rcpp::Nullable<std::string> type = R_NilValue,
                    Rcpp::Nullable<bool> fast = R_NilValue,
                    Rcpp::Nullable<bool> complete = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List output, res;
  bool complete_ = false, fast_ = false;
  std::string type_ = "column";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int Q_nrow = Q.rows();
  const int Q_ncol = Q.cols();
  const int R_nrow = R.rows();
  const int R_ncol = R.cols();                // p
  const int m      = U.cols();                // m
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
    if ((type_ != "column") && (type_ != "row")) {
      type_ = "column";
    }
  } else {
    type_ = "column";
  }
  if (fast.isNotNull()) {
    fast_ = Rcpp::as<bool>(fast);
  } else {
    fast_ = false;
  }
  if (complete.isNotNull()) {
    complete_ = Rcpp::as<bool>(complete);
  } else {
    complete_ = false;
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Pre-processing: m and p                          */
  if (!fast_) {
    if (type_ == "column") {
      if (m >= (Q_nrow - R_ncol)) {
        stop("* qrupdate : adding too many columns.");
      }
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Pre-processing: Q, R and U matrices:            */
  if (!fast_) {
    if (Q_ncol <= R_ncol) {
      stop("* qrupdate : p is greater than n.");
    }
    if (R_nrow == R_ncol) {
      stop("* qrupdate : the input 'R' is not the 'complete' R matrix.");
    }
    if (Q_nrow != Q_ncol) {
      stop("* qrupdate : the input 'Q' is not a square matrix.");
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Qin(Q_nrow, Q_ncol);          Qin.setZero();        // full Q matrix (input for delete rows)
  Eigen::MatrixXd Rin(R_nrow, R_ncol);          Rin.setZero();        // full R matrix (input for delete rows)
  Eigen::MatrixXd Qs(Q_nrow+m, Q_nrow+m);       Qs.setZero();
  Eigen::MatrixXd Rs(Q_nrow+m, R_ncol+m);       Rs.setZero();
  Eigen::MatrixXd Q_(Q_nrow+m, Q_nrow+m);       Q_.setZero();         // Q_ matrix is the one returned by qraddrow/qraddcol
  Eigen::MatrixXd R_(Q_nrow+m, R_ncol+m);       R_.setZero();         // R_ matrix is the one returned by qraddrow/qraddcol
                                                                      // Claudio restituisce quello che vuole fai attenzione
                                                                      // io la dichiaro sempre la piu' grande possibile
                                                                      // e poi riduco righe o colonne a seconda di quello che faccio
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Update the QR matrix: adding one or more columns:     */
  if (type_ == "column") {
    
    // resize Q_ and R_
    Q_.resize(Q_nrow, Q_nrow);
    R_.resize(R_ncol+m, R_ncol+m);
    
    // resize the input matrices: delete rows requires the reduced QR factorization
    Qin = Q.block(0, 0, Qin.rows(), Qin.cols());
    Rin = R.block(0, 0, Rin.rows(), Rin.cols());
    
    // get the dimension of the matrix U
    if (!fast_) {
      if (U.rows() != Q_nrow) {
        stop("* qrupdate : two inputs 'Q' and 'U' have non-matching dimensions.");
      }
    }
    if (m == 1) {
      // perform add one column
      res = qraddcol(Qin, Rin, k, U);
    } else {
      // perform add m columns
      res = qraddmcols(Qin, Rin, k, U);
    }
    
    // Output
    Q_ = res["Q"];
    R_ = res["R"];
    if (complete_) {
      Qs.resize(Q_nrow, Q_nrow);
      Rs.resize(Q_nrow, R_ncol+m);
      Rs.setZero();
      Qs                                 = Q_.block(0, 0, Qs.rows(), Qs.cols());
      Rs.block(0, 0, R_ncol+m, R_ncol+m) = R_;
      output = Rcpp::List::create(Rcpp::Named("Q") = Qs,
                                  Rcpp::Named("R") = Rs);
    } else {
      Qs.resize(Q_nrow, R_ncol+m);
      Qs     = Q_.block(0, 0, Qs.rows(), Qs.cols());
      output = Rcpp::List::create(Rcpp::Named("Q") = Qs,
                                  Rcpp::Named("R") = R_);
    }
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Update the QR matrix: adding one or more rows:     */
  if (type_ == "row") {
    
    // resize Q_ and R_
    Q_.resize(Q_nrow+m, Q_nrow+m);
    R_.resize(R_ncol, R_ncol);
    
    // resize the input matrices: delete rows requires the reduced QR factorization
    Qin.resize(Q_nrow, R_ncol);
    Rin.resize(R_ncol, R_ncol);
    Qin = Q.block(0, 0, Qin.rows(), Qin.cols());
    Rin = R.block(0, 0, Rin.rows(), Rin.cols());
    
    // get the dimension of the matrix U
    if (!fast_) {
      if (U.rows() != R_ncol) {
        stop("* QRupdate : two inputs 'Q' and 'U' have non-matching dimensions.");
      }
    }
    if (m == 1) {
      // perform add one row
      res = qraddrow(Qin, Rin, k, U.transpose());
    } else {
      // perform add m rows
      res = qraddmrows(Qin, Rin, k, U.transpose());
    }
    
    // Output
    Q_ = res["Q"];
    R_ = res["R"];
    if (complete_) {
      Rs.resize(Q_nrow+m, R_ncol);
      Rs.block(0, 0, R_ncol, R_ncol) = R_;
      output                         = Rcpp::List::create(Rcpp::Named("Q") = Q_,
                                                          Rcpp::Named("R") = Rs);
    } else {
      Qs.resize(Q_nrow+m, R_ncol);
      Qs     = Q_.block(0, 0, Q_nrow+m, R_ncol);
      output = Rcpp::List::create(Rcpp::Named("Q") = Qs,
                                  Rcpp::Named("R") = R_);
    }
  }
  
  /* return output          */
  return output;
}

//' @name qrdowndate
//' @title Fast downdating of the QR factorization
//' @description qrdowndate provides the update of the QR factorization after the deletion of \eqn{m>1} rows or columns to the matrix \eqn{X\in\mathbb{R}^{n\times p}} with \eqn{n>p}. The QR factorization of the matrix \eqn{X\in\mathbb{R}^{n\times p}} returns the matrices \eqn{Q\in\mathbb{R}^{n\times n}} and \eqn{R\in\mathbb{R}^{n\times p}} such that \eqn{X=QR}. The \eqn{Q} and \eqn{R} matrices are factorized as \eqn{Q=\begin{bmatrix}Q_1&Q_2\end{bmatrix}} and \eqn{R=\begin{bmatrix}R_1\\R_2\end{bmatrix}}, with \eqn{Q_1\in\mathbb{R}^{n\times p}}, \eqn{Q_2\in\mathbb{R}^{n\times (n-p)}} such that \eqn{Q_1^{\top}Q_2=Q_2^\top Q_1=0} and \eqn{R_1\in\mathbb{R}^{p\times p}} upper triangular matrix and \eqn{R_2\in\mathbb{R}^{(n-p)\times p}}. qrupdate accepts in input the matrices \eqn{Q} and either the complete matrix \eqn{R} or the reduced one, \eqn{R_1}. See Golub and Van Loan (2013) for further details on the method.
//' @param Q a \eqn{n\times n} matrix.
//' @param R a \eqn{n\times p} upper triangular matrix.
//' @param k position where the columns or the rows are removed.
//' @param m number of columns or rows to be removed. Default is \eqn{m=1}.
//' @param type either 'row' of 'column', for deleting rows or columns. Default is 'column'.
//' @param fast fast mode: disable to check whether the provided matrices are valid inputs. Default is FALSE.
//' @param complete logical expression of length 1. Indicates whether an arbitrary orthogonal completion of the \eqn{Q} matrix is to be made, or whether the \eqn{R} matrix is to be completed by binding zero-value rows beneath the square upper triangle.
//' @return A named list containing \describe{
//' \item{Q}{the updated Q matrix.}
//' \item{R}{the updated R matrix.}
//' }
//'
//' @examples
//' ## Remove one column
//' ## generate sample data
//' set.seed(10)
//' n      <- 10
//' p      <- 6
//' X      <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- fastQR::qr(X, type = "householder",
//'                      nb = NULL,
//'                      complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//'
//' ## select the column to be deleted
//' ## from X and update X
//' k  <- 2
//' X1 <- X[, -k]
//'
//' ## downdate the QR decomposition
//' out <- fastQR::qrdowndate(Q = Q, R = R,
//'                           k = k, m = 1,
//'                           type = "column",
//'                           fast = FALSE,
//'                           complete = TRUE)
//'
//' ## check
//' round(out$Q %*% out$R - X1, 5)
//' max(abs(out$Q %*% out$R - X1))
//'
//' ## Remove m columns
//' ## generate sample data
//' set.seed(10)
//' n      <- 10
//' p      <- 6
//' X      <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- fastQR::qr(X, type = "householder",
//'                      nb = NULL,
//'                      complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//'
//' ## select the column to be deleted from X
//' ## and update X
//' m  <- 2
//' k  <- 2
//' X1 <- X[, -c(k,k+m-1)]
//'
//' ## downdate the QR decomposition
//' out <- fastQR::qrdowndate(Q = Q, R = R,
//'                           k = k, m = 2,
//'                           type = "column",
//'                           fast = TRUE,
//'                           complete = FALSE)
//'
//' ## check
//' round(out$Q %*% out$R - X1, 5)
//' max(abs(out$Q %*% out$R - X1))
//'
//' ## Remove one row
//' ## generate sample data
//' set.seed(10)
//' n      <- 10
//' p      <- 6
//' X      <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- fastQR::qr(X, type = "householder",
//'                      nb = NULL,
//'                      complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//'
//' ## select the row to be deleted from X and update X
//' k  <- 5
//' X1 <- X[-k,]
//'
//' ## downdate the QR decomposition
//' out <- fastQR::qrdowndate(Q = Q, R = R,
//'                           k = k, m = 1,
//'                           type = "row",
//'                           fast = FALSE,
//'                           complete = TRUE)
//'
//' ## check
//' round(out$Q %*% out$R - X1, 5)
//' max(abs(out$Q %*% out$R - X1))
//'
//' ## Remove m rows
//' ## generate sample data
//' set.seed(10)
//' n      <- 10
//' p      <- 6
//' X      <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- fastQR::qr(X, type = "householder",
//'                      nb = NULL,
//'                      complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//'
//' ## select the rows to be deleted from X and update X
//' k  <- 5
//' m  <- 2
//' X1 <- X[-c(k,k+1),]
//'
//' ## downdate the QR decomposition
//' out <- fastQR::qrdowndate(Q = Q, R = R,
//'                           k = k, m = m,
//'                           type = "row",
//'                           fast = FALSE,
//'                           complete = TRUE)
//'
//' ## check
//' round(out$Q %*% out$R - X1, 5)
//' max(abs(out$Q %*% out$R - X1))
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
Rcpp::List qrdowndate(const Eigen::MatrixXd& Q,
                      const Eigen::MatrixXd& R,
                      const int k,
                      Rcpp::Nullable<int> m = R_NilValue,
                      Rcpp::Nullable<std::string> type = R_NilValue,
                      Rcpp::Nullable<bool> fast = R_NilValue,
                      Rcpp::Nullable<bool> complete = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  Rcpp::List output, res;
  bool complete_ = false, fast_ = false;
  std::string type_ = "column";
  int m_ = 1;

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int Q_nrow = Q.rows();
  const int Q_ncol = Q.cols();
  const int R_nrow = R.rows();
  const int R_ncol = R.cols();                // p
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
    if ((type_ != "column") && (type_ != "row")) {
      type_ = "column";
    }
  } else {
    type_ = "column";
  }
  if (fast.isNotNull()) {
    fast_ = Rcpp::as<bool>(fast);
  } else {
    fast_ = false;
  }
  if (complete.isNotNull()) {
    complete_ = Rcpp::as<bool>(complete);
  } else {
    complete_ = false;
  }
  if (m.isNotNull()) {
    m_ = Rcpp::as<int>(m);
  } else {
    m_ = 1;
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Pre-processing: m and p                          */
  if (!fast_) {
    if (type_ == "column") {
      if (R_ncol < 2) {
        stop("* qrdowndate : there is only one column.");
      }
      if (m_ >= R_ncol) {
        stop("* qrdowndate : removing too many columns.");
      }
    } else {
      if (Q_nrow < 2) {
        stop("* qrdowndate : there is only one row.");
      }
      if (m_ >= R_ncol) {
        stop("* qrdowndate : removing too many rows.");
      }
      if (m_ >= (Q_nrow - R_ncol)) {
        stop("* qrdowndate : removing too many rows.");
      }
    }
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Pre-processing: Q, R matrices:                   */
  if (!fast_) {
    if (Q_ncol <= R_ncol) {
      stop("* qrdowndate : p is greater than n.");
    }
    if (R_nrow == R_ncol) {
      stop("* qrdowndate : the input 'R' is not the 'complete' R matrix.");
    }
    if (Q_ncol != R_nrow) {
      stop("* qrdowndate : two inputs 'Q' and 'R' have non-matching dimensions.");
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Qin(Q_nrow, R_ncol);          Qin.setZero();        // reduced Q matrix (input for delete rows)
  Eigen::MatrixXd Rin(R_ncol, R_ncol);          Rin.setZero();        // reduced R matrix (input for delete rows)
  Eigen::MatrixXd Qs(Q_nrow, Q_nrow);           Qs.setZero();
  Eigen::MatrixXd Rs(Q_nrow, R_ncol);           Rs.setZero();
  Eigen::MatrixXd Q_(Q_nrow, R_ncol);           Q_.setZero();         // Q_ matrix is the one returned by qraddrow/qraddcol
  Eigen::MatrixXd R_(R_ncol, R_ncol);           R_.setZero();         // R_ matrix is the one returned by qraddrow/qraddcol
                                                                      // Claudio restituisce quello che vuole fai attenzione
                                                                      // io la dichiaro sempre la piu' grande possibile
                                                                      // e poi riduco righe o colonne a seconda di quello che faccio
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Downdate the QR matrix: removing one or more columns:     */
  if (type_ == "column") {
    // resize Q_ and R_
    Q_.resize(Q_nrow, R_ncol-m_);
    R_.resize(R_ncol-m_, R_ncol-m_);
    
    // resize the input matrices: delete rows requires the reduced QR factorization
    Qin = Q.block(0, 0, Qin.rows(), Qin.cols());
    Rin = R.block(0, 0, Rin.rows(), Rin.cols());
    if (m_ == 1) {
      // perform add one column
      res = qrdeletecol(Qin, Rin, k);
    } else {
      // perform add m column
      res = qrdeletemcols_adj(Qin, Rin, k, m_);
    }
    
    // Output
    Q_ = res["Q"];
    R_ = res["R"];
    if (complete_) {
      Rs.resize(Q_nrow, R_ncol-m_);
      Qs.block(0, 0, Q_nrow, Q_ncol-m_)    = Q_;
      Rs.block(0, 0, R_ncol-m_, R_ncol-m_) = R_;
      output                               = Rcpp::List::create(Rcpp::Named("Q") = Qs,
                                                                Rcpp::Named("R") = Rs);
    } else {
      output = Rcpp::List::create(Rcpp::Named("Q") = Q_,
                                  Rcpp::Named("R") = R_);
    }
  }
    
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Downdate the QR matrix: removing one or more rows:     */
  if (type_ == "row") {
    // resize Q_ and R_
    Q_.resize(Q_nrow-m_, Q_nrow-m_);
    R_.resize(R_ncol, R_ncol);
    
    if (m_ == 1) {
      // perform add one row
      res = qrdeleterow(Q, R, k);
    } else {
      // perform add m rows
      res = qrdeletemrows(Q, R, k, m_);
    }
    
    // Output
    Q_ = res["Q"];
    R_ = res["R"];
    if (complete_) {
      Qs.resize(Q_nrow-m_, Q_nrow-m_);
      Rs.resize(Q_nrow-m_, R_ncol);
      Rs.setZero();
      Qs.block(0, 0, Q_nrow-m_, Q_ncol-m_) = Q_;
      Rs.block(0, 0, R_ncol, R_ncol)       = R_;
      output                               = Rcpp::List::create(Rcpp::Named("Q") = Qs,
                                                                Rcpp::Named("R") = Rs);
    } else {
      Qs.resize(Q_nrow-m_, R_ncol);
      Rs.resize(R_ncol, R_ncol);
      Qs     = Q_.block(0, 0, Qs.rows(), Qs.cols());
      Rs     = R_.block(0, 0, Rs.rows(), Rs.cols());
      output = Rcpp::List::create(Rcpp::Named("Q") = Qs,
                                  Rcpp::Named("R") = Rs);
    }
  }

  /* return output          */
  return output;
}


  

  
 









