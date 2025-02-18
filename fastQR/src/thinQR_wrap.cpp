// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                 :::::::::::::::::::: //
// ::::::::::::::::::::    QR wrap      :::::::::::::::::::: //
// ::::::::::::::::::::                 :::::::::::::::::::: //
// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //

#include "QRdecomposition.h"
#include "thinQRupdate.h"

#define EIGEN_USE_BLAS
#define EIGEN_USE_LAPACKE

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(Rcpp)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace Eigen;
using namespace arma;

//' @name rupdate
//' @title Fast updating of the R matrix
//' @description updates the R factorization when \eqn{m \geq 1} rows or columns are added to the matrix \eqn{X \in \mathbb{R}^{n \times p}}, where \eqn{n > p}. The R factorization of \eqn{X} produces an upper triangular matrix \eqn{R \in \mathbb{R}^{p \times p}} such that \eqn{X^\top X = R^\top R}. For more details on this method, refer to Golub and Van Loan (2013). Columns can only be added in positions \eqn{p+1} through \eqn{p+m}, while the position of added rows does not need to be specified.
//' @param R a \eqn{p\times p} upper triangular matrix.
//' @param U either a \eqn{n\times m} matrix or a \eqn{p\times m} matrix of columns or rows to be added.
//' @param type either 'row' of 'column', for adding rows or columns.
//' @param fast fast mode: disable to check whether the provided matrices are valid inputs. Default is FALSE.
//' @return R the updated R matrix.
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
//' output <- fastQR::qr(X, complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//' R1     <- R[1:p,]
//'
//' ## create column to be added
//' u  <- matrix(rnorm(n), n, 1)
//' X1 <- cbind(X, u)
//'
//' ## update the R decomposition
//' R2 <- fastQR::rupdate(X = X, R = R1, U = u,
//'                       fast = FALSE, type = "column")
//'
//' ## check
//' max(abs(crossprod(R2) - crossprod(X1)))
//'
//' ## Add m columns
//' ## generate sample data
//' set.seed(1234)
//' n <- 10
//' p <- 5
//' X <- matrix(rnorm(n * p, 1), n, p)
//'
//' ## get the initial QR factorization
//' output <- fastQR::qr(X, complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//' R1     <- R[1:p,]
//'
//' ## create the matrix of columns to be added
//' m  <- 2
//' U  <- matrix(rnorm(n*m), n, m)
//' X1 <- cbind(X, U)
//'
//' # QR update
//' R2 <- fastQR::rupdate(X = X, R = R1, U = U,
//'                       fast = FALSE, type = "column")
//'
//' ## check
//' max(abs(crossprod(R2) - crossprod(X1)))
//'
//' ## Add one row
//' ## generate sample data
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
//' k  <- 5
//' if (k<=n) {
//'   X1 <- rbind(rbind(X[1:(k-1), ], t(u)), X[k:n, ])
//' } else {
//'   X1 <- rbind(rbind(X, t(u)))
//' }
//'
//' ## update the R decomposition
//' R2 <- fastQR::rupdate(R = R1, X = X,
//'                       U = u,
//'                       type = "row")
//'
//' ## check
//' max(abs(crossprod(R2) - crossprod(X1)))
//'
//' ## Add m rows
//' ## generate sample data
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
//' ## create the matrix of rows to be added
//' m  <- 2
//' U  <- matrix(data = rnorm(p*m), p, m)
//' k  <- 5
//' if (k<=n) {
//'   X1 <- rbind(rbind(X[1:(k-1), ], t(U)), X[k:n, ])
//' } else {
//'   X1 <- rbind(rbind(X, t(U)))
//' }
//'
//' ## update the R decomposition
//' R2 <- fastQR::rupdate(R = R1, X = X,
//'                       U = U,
//'                       fast = FALSE,
//'                       type = "row")
//'
//' ## check
//' max(abs(crossprod(R2) - crossprod(X1)))
//'
//' @references
//' \insertRef{golub_van_loan.2013}{fastQR}
//'
//' \insertRef{bjorck.2015}{fastQR}
//'
//' \insertRef{bjorck.2024}{fastQR}
//'
//' \insertRef{bernardi_etal.2024}{fastQR}

// [[Rcpp::export]]
Eigen::MatrixXd rupdate (const Eigen::MatrixXd& X,
                         const Eigen::MatrixXd& R,
                         const Eigen::MatrixXd& U,
                         Rcpp::Nullable<bool> fast = R_NilValue,
                         Rcpp::Nullable<std::string> type = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  std::string type_ = "column";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int Q_nrow = X.rows();
  const int Q_ncol = X.rows();
  const int R_nrow = R.rows();
  const int R_ncol = R.cols();                // p
  const int m      = U.cols();                // m
  bool fast_       = false;

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (fast.isNotNull()) {
    fast_ = Rcpp::as<bool>(fast);
  } else {
    fast_ = false;
  }
  if (type.isNotNull()) {
    type_ = Rcpp::as<std::string>(type);
    if ((type_ != "column") && (type_ != "row")) {
      type_ = "column";
    }
  } else {
    type_ = "column";
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Pre-processing: m and p                          */
  if (!fast_) {
    if (type_ == "column") {
      if (m >= (Q_nrow - R_ncol)) {
        stop("* rupdate : adding too many columns.");
      }
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Pre-processing: Q, R and U matrices:            */
  if (!fast_) {
    if (Q_ncol <= R_ncol) {
      stop("* rupdate : p is greater than n.");
    }
    if (R_nrow != R_ncol) {
      stop("* rupdate : the input 'R' is not the 'reduced' R matrix.");
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Rs(R_nrow+m, R_nrow+m);               Rs.setZero();
  Eigen::MatrixXd R_in(Q_nrow, R_ncol);                 R_in.setZero();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Update the QR matrix: adding one or more columns:     */
  if (type_ == "column") {
    
    // get the dimension of the matrix U
    if (!fast_) {
      if (U.rows() != Q_nrow) {
        stop("* rupdate : two inputs 'X' and 'U' have non-matching dimensions.");
      }
    }
    if (m == 1) {
      // perform add one column
      Rs = thinqraddcol(R, X, U);
    } else {
      // perform add m columns
      Rs = thinqraddmcols(R, X, U);
    }
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Update the QR matrix: adding one or more rows:     */
  if (type_ == "row") {
    // resize input matrix R
    R_in.block(0, 0, R_ncol, R_ncol) = R;
    
    // resize output
    Rs.resize(R_nrow, R_ncol);
    
    // get the dimension of the matrix U
    if (!fast_) {
      if (U.rows() != R_ncol) {
        stop("* rupdate : two inputs 'Q' and 'U' have non-matching dimensions.");
      }
    }
    if (m == 1) {
      // perform add one row
      Rs = thinqraddrow(R_in, U);
    } else {
      // perform add m rows
      Rs = thinqraddmrows(R_in, U.transpose());
    }
  }
  
  /* return output          */
  return Rs;
}

//' @name rdowndate
//' @title Fast downdating of the R matrix
//' @description rdowndate provides the update of the thin R matrix of the QR factorization after the deletion of \eqn{m\geq 1} rows or columns to the matrix \eqn{X\in\mathbb{R}^{n\times p}} with \eqn{n>p}. The R factorization of the matrix \eqn{X} returns the upper triangular matrix \eqn{R\in\mathbb{R}^{p\times p}} such that \eqn{X^\top X=R^\top R}. See Golub and Van Loan (2013) for further details on the method.
//' @param R a \eqn{p\times p} upper triangular matrix.
//' @param k position where the columns or the rows are removed.
//' @param m number of columns or rows to be removed.
//' @param U a \eqn{p\times m} matrix of rows to be removed. It should only be provided when rows are being removed.
//' @param type either 'row' of 'column', for removing rows or columns.
//' @param fast fast mode: disable to check whether the provided matrices are valid inputs. Default is FALSE.
//' @return R the updated R matrix.
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
//' R1     <- R[1:p,]
//'
//' ## select the column to be deleted from X and update X
//' k  <- 2
//' X1 <- X[, -k]
//'
//' ## downdate the R decomposition
//' R2 <- fastQR::rdowndate(R = R1, k = k,
//'                         m = 1, type = "column")
//'
//' ## check
//' max(abs(crossprod(R2) - crossprod(X1)))
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
//' R1     <- R[1:p,]
//'
//' ## select the column to be deleted from X and update X
//' k  <- 2
//' X1 <- X[, -c(k,k+1)]
//'
//' ## downdate the R decomposition
//' R2 <- fastQR::rdowndate(R = R1, k = k,
//'                         m = 2, type = "column")
//'
//' ## check
//' max(abs(crossprod(R2) - crossprod(X1)))
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
//' R1     <- R[1:p,]
//'
//' # select the row to be deleted from X and update X
//' k  <- 5
//' X1 <- X[-k,]
//' U  <- as.matrix(X[k,], p, 1)
//'
//' ## downdate the R decomposition
//' R2 <-  rdowndate(R = R1, k = k, m = 1,
//'                  U = U, fast = FALSE, type = "row")
//'
//' ## check
//' max(abs(crossprod(R2) - crossprod(X1)))
//'
//' ## Remove m rows
//' ## create data: n > p
//' set.seed(10)
//' n      <- 10
//' p      <- 6
//' X      <- matrix(rnorm(n * p, 1), n, p)
//' output <- fastQR::qr(X, type = "householder",
//'                     nb = NULL,
//'                      complete = TRUE)
//' Q      <- output$Q
//' R      <- output$R
//' R1     <- R[1:p,]
//'
//' ## select the rows to be deleted from X and update X
//' k  <- 2
//' m  <- 2
//' X1 <- X[-c(k,k+m-1),]
//' U  <- t(X[k:(k+m-1), ])
//'
//' ## downdate the R decomposition
//' R2 <- rdowndate(R = R1, k = k, m = m,
//'                 U = U, fast = FALSE, type = "row")
//'
//' ## check
//' max(abs(crossprod(R2) - crossprod(X1)))
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
Eigen::MatrixXd rdowndate (const Eigen::MatrixXd& R,
                           Rcpp::Nullable<int> k = R_NilValue,
                           Rcpp::Nullable<int> m = R_NilValue,
                           Rcpp::Nullable<Rcpp::NumericMatrix> U = R_NilValue,
                           Rcpp::Nullable<bool> fast = R_NilValue,
                           Rcpp::Nullable<std::string> type = R_NilValue) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  std::string type_ = "column";
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int R_nrow = R.rows();
  const int R_ncol = R.cols();                // p
  int m_           = 0;                       // m
  int k_           = 0;
  bool fast_       = false;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   check for NULL:                                    */
  if (fast.isNotNull()) {
    fast_ = Rcpp::as<bool>(fast);
  } else {
    fast_ = false;
  }
    
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Pre-processing: Q, R and U matrices:            */
  if (!fast_) {
    if (R_nrow != R_ncol) {
      stop("* rupdate : the input 'R' is not the 'reduced' R matrix.");
    }
  }
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Rs(R_nrow, R_nrow);               Rs.setZero();

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
  if (k.isNotNull()) {
    k_ = Rcpp::as<int>(k);
  } else {
    k_ = 0;
  }
  if (m.isNotNull()) {
    m_ = Rcpp::as<int>(m);
  } else {
    if (type_ == "column") {
      m_ = R_ncol;
      warning("* rdowndate : m has been set equal to the number of columns of R!");
    } else {
      m_ = R_nrow;
      warning("* rdowndate : m has been set equal to the number of rows of R!");
    }
  }
  if (U.isNotNull()) {
    /* Nullable output declaration */
    Rcpp::NumericMatrix U_tmp(U);
    
    // Transform Rcpp vector "vec_rcpp" into an Eigen vector
    Eigen::MatrixXd U_ = Rcpp::as<Eigen::MatrixXd>(wrap(U_tmp));
    
    // warning message
    if (!fast_) {
      warning("* rdowndate : m has been set equal to the the dimension of the provided matrix U!");
    }
    
    // get the dimension
    m_ = U_.cols();
    
    /* ::::::::::::::::::::::::::::::::::::::::::::::
     Update the QR matrix: adding one or more rows:     */
    if (type_ == "row") {
      
      // get the dimension of the matrix U
      if (!fast_) {
        if (U_.rows() != R_ncol) {
          stop("* rupdate : two inputs 'R' and 'U' have non-matching dimensions.");
        }
      }
      if (m_ == 1) {
        // perform add one row
        Rs = thinqrdeleterow(R, U_);
      } else {
        // perform add m rows
        Rs = thinqrdeletemrows(R, U_.transpose());
      }
    }
  }

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   Update the QR matrix: adding one or more columns:     */
  if (type_ == "column") {
    
    // resize output
    Rs.resize(R_nrow-m_, R_ncol-m_);

    if (m_ == 1) {
      // perform add one column
      Rs = thinqrdeletecol(R, k_);
    } else {
      // perform add m columns
      Rs = thinqrdeletemcols_adj(R, k_, m_);
    }
  }
  
  /* return output          */
  return Rs;
}















