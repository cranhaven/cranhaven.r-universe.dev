// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                        :::::::::::::::::::: //
// ::::::::::::::::::::    QR decomposition    :::::::::::::::::::: //
// ::::::::::::::::::::                        :::::::::::::::::::: //
// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //

// Utility routines

// Authors:
//           Bernardi Mauro, University of Padova
//           Last update: october 7, 2024

// [[Rcpp::depends(RcppArmadillo)]]
#include "QRdecomposition.h"
#include <time.h>

// ::::::::::::::::::::::::::::::::::::::::::::::::::
// QR: householder reflections
// function to compute QR factorization of X with Householder rotations
// X matrix m x n to be factorized
Rcpp::List householderQR (const Eigen::MatrixXd& X,
                          int complete) {
  
  int i, n = X.rows(), p = X.cols();
  double beta = 0.0, mu = 0.0;
  Eigen::MatrixXd R = X;
  Eigen::MatrixXd Q(n, n); Q.setIdentity();
  
  if (n > p) {
    Eigen::VectorXd v(n-1); v.setZero();
    VectorXd tmp(n+2); tmp.setZero();
    
    for (i = 0; i < p; i++) {
      // Householder rotation
      tmp.resize(n-i+2);
      tmp  = householder(R.block(i, i, n-i, 1));
      v    = tmp.head(n-i);
      beta = tmp(n-i);
      mu   = tmp(n-i+1);
      
      // update R
      R(i, i) = mu;
      R.block(i+1, i, n-i-1, 1).setZero();
      if (i < p-1)
        R.block(i, i+1, n-i, p-i-1) -= (beta * v) * (v.transpose() * R.block(i, i+1, n-i, p-i-1));
      
      // update Q
      Q.block(0, i, Q.rows(), n-i) -= beta * (Q.block(0, i, Q.rows(), n-i) * v) * v.transpose();
    }
    
    if (complete == 0) {
      Eigen::MatrixXd Q1(n, R.cols()); Q1.setZero();
      Q1 = Q.leftCols(R.cols());
      /* Return output      */
      return Rcpp::List::create(Rcpp::Named("Q") = Q1,
                                Rcpp::Named("R") = R.topRows(R.cols()));
    } else {
      return Rcpp::List::create(Rcpp::Named("Q") = Q,
                                Rcpp::Named("R") = R);
    }
  } else {
    Eigen::VectorXd v(n-1); v.setZero();
    VectorXd tmp(n+2); tmp.setZero();
    
    for (i = 0; i < n-1; i++) {
      // Householder rotation
      tmp.resize(n-i+2);
      tmp = householder(R.block(i, i, n-i, 1));
      v = tmp.head(n-i);
      beta = tmp(n-i);
      mu = tmp(n-i+1);
      
      // update R
      R(i, i) = mu;
      R.block(i+1, i, n-i-1, 1).setZero();
      if (i < n-1)
        R.block(i, i+1, n-i, p-i-1) -= (beta * v) * (v.transpose() * R.block(i, i+1, n-i, p-i-1));
      
      // update Q
      Q.block(0, i, Q.rows(), n-i) -= beta * (Q.block(0, i, Q.rows(), n-i) * v) * v.transpose();
    }
    return Rcpp::List::create(Rcpp::Named("Q") = Q,
                              Rcpp::Named("R") = R);
  }
}

// ::::::::::::::::::::::::::::::::::::::::::::::::::
// QR: givens rotations
// function to compute QR decomposition with givens rotations
// X matrix m x n to be factorized
Rcpp::List givensQR (const Eigen::MatrixXd& X,
                     int complete) {
  // function to compute QR decomposition with givens rotations
  // X matrix m x n to be factorized
  
  int i, j, n = X.rows(), p = X.cols();
  double cc = 0.0, ss = 0.0;
  Eigen::MatrixXd R = X;
  Eigen::MatrixXd Q(n, n); Q.setIdentity();
  Eigen::MatrixXd Givens(2, 2); Givens.setZero();
  Eigen::Vector2d tmp; tmp.setZero();
  
  if (n > p) {
    for (j = 0; j < p; j++) {
      for (i = n-1; i > j; i--) {
        
        // Givens rotation
        tmp = givens(R(i-1, j), R(i, j));
        cc  = tmp(0);
        ss  = tmp(1);
        Givens << cc, ss, -ss, cc;
        
        // update A and Q
        R.block(i-1, j, 2, p-1-j+1)  = Givens.transpose() * R.block(i-1, j, 2, p-1-j+1);
        Q.block(0, i-1, Q.rows(), 2) = Q.block(0, i-1, Q.rows(), 2) * Givens;
      }
    }
    
    // output
    R.triangularView<Eigen::StrictlyLower>().setZero();
    
    if (complete == 0) {
      Eigen::MatrixXd Q1(n, R.cols()); Q1.setZero();
      Q1 = Q.leftCols(R.cols());
      /* Return output      */
      return Rcpp::List::create(Rcpp::Named("Q") = Q1,
                                Rcpp::Named("R") = R.topRows(R.cols()));
    } else {
      return Rcpp::List::create(Rcpp::Named("Q") = Q,
                                Rcpp::Named("R") = R);
    }
  } else {
    for (j = 0; j < n-1; j++) {
      for (i = n-1; i > j; i--) {
        
        // Givens rotation
        tmp = givens(R(i-1, j), R(i, j));
        cc = tmp(0);
        ss = tmp(1);
        Givens << cc, ss, -ss, cc;
        
        // update A and Q
        R.block(i-1, j, 2, p-1-j+1) = Givens.transpose() * R.block(i-1, j, 2, p-1-j+1);
        Q.block(0, i-1, Q.rows(), 2) = Q.block(0, i-1, Q.rows(), 2) * Givens;
      }
    }
    
    // output
    R.triangularView<Eigen::StrictlyLower>().setZero();
    return Rcpp::List::create(Rcpp::Named("Q") = Q,
                              Rcpp::Named("R") = R);
  }
}

// ::::::::::::::::::::::::::::::::::::::::::::::::::
// QR: recursive block 
// function to compute thin QR decomposition with block recursion
// X matrix m x n to be factorized
// nb number of blocks
Rcpp::List rbQR (const Eigen::MatrixXd& X,
                 const int& nb,
                 const int& complete) {
  
  Eigen::MatrixXd As = X;
  const int m  = As.rows(), n = As.cols();
  const int n1 = std::floor(n / 2);
  Eigen::MatrixXd Q(m, n);              Q.setZero();
  Eigen::MatrixXd R(n, n);              R.setZero();
  Eigen::MatrixXd thinQ(m, n);          thinQ.setZero();
  Eigen::MatrixXd Q1(m, n1);            Q1.setZero();
  Eigen::MatrixXd Q2(m, n1);            Q2.setZero();
  Eigen::MatrixXd R11(n1, n1);          R11.setZero();
  Eigen::MatrixXd R12(n1, n-n1);        R12.setZero();
  Eigen::MatrixXd R22(n-n1, n-n1);      R22.setZero();
  
  if (n < nb) {
    Eigen::HouseholderQR<Eigen::MatrixXd> qr(m, n);
    qr.compute(As);
    thinQ.setIdentity();
    Q = qr.householderQ() * thinQ;
    R = qr.matrixQR().template triangularView<Eigen::Upper>();
    
    /* Return output      */
    if (complete == 0) {
      Eigen::MatrixXd Q1(n, R.cols()); Q1.setZero();
      Q1 = Q.leftCols(R.cols());
      return Rcpp::List::create(Rcpp::Named("Q") = Q1,
                                Rcpp::Named("R") = R.topRows(R.cols()));
    } else {
      return Rcpp::List::create(Rcpp::Named("Q") = Q,
                                Rcpp::Named("R") = R);
    }
  } else {
    // QR left half
    Rcpp::List tmp(2);
    tmp = rbQR(As.leftCols(n1), nb, complete);
    Q1  = tmp(0);
    R11 = tmp(1);
    
    // QR right half
    R12                 = Q1.transpose() * As.rightCols(n-n1);
    As.rightCols(n-n1) -= Q1 * R12;
    tmp                 = rbQR(As.rightCols(n-n1), nb, complete);
    Q2                  = tmp(0);
    R22                 = tmp(1);
    
    // return
    Q.leftCols(n1)              = Q1;
    Q.rightCols(n-n1)           = Q2;
    R.block(0, 0, n1, n1)       = R11;
    R.block(0, n1, n1, n-n1)    = R12;
    R.block(n1, n1, n-n1, n-n1) = R22;
    R.triangularView<Eigen::StrictlyLower>().setZero();
    
    /* Return output      */
    if (complete == 0) {
      Eigen::MatrixXd Q1(n, R.cols()); Q1.setZero();
      Q1 = Q.leftCols(R.cols());
      return Rcpp::List::create(Rcpp::Named("Q") = Q1,
                                Rcpp::Named("R") = R.topRows(R.cols()));
    } else {
      return Rcpp::List::create(Rcpp::Named("Q") = Q,
                                Rcpp::Named("R") = R);
    }
  }
}

Eigen::MatrixXd householderR (const Eigen::MatrixXd& X) {
  
  int i = 0, n = X.rows(), p = X.cols();
  double beta = 0.0, mu = 0.0;
  Eigen::MatrixXd R = X;
  
  if (n >= p) {
    Eigen::VectorXd v(n-1); v.setZero();
    VectorXd tmp(n+2); tmp.setZero();
    
    for (i = 0; i < p; i++) {
      // Householder rotation
      tmp.resize(n-i+2);
      tmp  = householder(R.block(i, i, n-i, 1));
      v    = tmp.head(n-i);
      beta = tmp(n-i);
      mu   = tmp(n-i+1);
      
      // update R
      R(i, i) = mu;
      R.block(i+1, i, n-i-1, 1).setZero();
      if (i < p-1)
        R.block(i, i+1, n-i, p-i-1) -= (beta * v) * (v.transpose() * R.block(i, i+1, n-i, p-i-1));
    }
  } else {
    Rcpp::stop("* fastQR : if n is smaller than p the Cholesky factorization can not be computed!\n");
  }
  return R.topRows(R.cols());
}






// end file
