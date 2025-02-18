// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                     :::::::::::::::::::: //
// ::::::::::::::::::::    thinQR update    :::::::::::::::::::: //
// ::::::::::::::::::::                     :::::::::::::::::::: //
// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //

// Utility routines

// Authors:
//           Bernardi Mauro, University of Padova
//           Last update: october 7, 2024

// [[Rcpp::depends(RcppArmadillo)]]
#include "thinQRupdate.h"
#include <time.h>

// ::::::::::::::::::::::::::::::::::::::::::
// thinQR update: add one column at the end
// function to update triangular matrix R when a column u is added to matrix X at the end
// input R p x p
// X n x p matrix
// u n-dim vector to be added
Eigen::MatrixXd thinqraddcol (const Eigen::MatrixXd& R, 
                              const Eigen::MatrixXd& X, 
                              const Eigen::VectorXd& u) {
  
  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int n = R.cols();
  
  // ::::::::::::::::::::::::::::::::::::::
  // compute R12
  // compute R12 with forward substitutions
  const Eigen::VectorXd r12 = R.transpose().triangularView<Eigen::Lower>().solve(X.transpose() * u);
  
  // insert R12 in R
  Eigen::MatrixXd Rs(n+1, n+1); Rs.setZero();
  Rs.topLeftCorner(n, n)  = R;
  Rs.topRightCorner(n, 1) = r12;
  
  // ::::::::::::::::::::::::::::::::::::::
  // compute r22
  Rs(n, n) = (u.transpose() * u - r12.transpose() * r12).value();
  Rs(n, n) = sqrt(std::abs(Rs(n, n)));
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  return Rs;
}

// ::::::::::::::::::::::::::::::::::::::
// thinQR update: add m columns at the end
// function to update triangular matrix R when block of m columns U is added to matrix X at the end
// input R p x p
// X n x p matrix
// U n x m matrix to be added
Eigen::MatrixXd thinqraddmcols (const Eigen::MatrixXd& R,
                                const Eigen::MatrixXd& X,
                                const Eigen::MatrixXd& U) {
  
  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int n = R.cols(), m = U.cols();
  
  // ::::::::::::::::::::::::::::::::::::::
  // update log-marginal
  const Eigen::MatrixXd R12 = R.transpose().triangularView<Eigen::Lower>().solve(X.transpose() * U);
  
  Eigen::MatrixXd R22(m, m); R22.setZero();
  Eigen::VectorXd ti = R12.col(0);
  
  R22(0, 0)               = std::sqrt(std::abs(U.col(0).squaredNorm() - ti.squaredNorm()));
  R22.block(0, 1, 1, m-1) = (U.col(0).transpose() * U.rightCols(m-1) - ti.transpose() * R12.rightCols(m-1)) / R22(0, 0);
  
  // compute R[(n+2):(n+m), (n+2):(n+m)]
  Eigen::VectorXd rsi(2); rsi.setZero();
  Eigen::MatrixXd rsj(1, m-1); rsj.setZero();
  for (int i = 1; i < m; i++) {
    // compute diagonal element n+i, n+i
    ti        = R12.col(i);
    rsi       = R22.block(0, i, i, 1);
    R22(i, i) = std::sqrt(std::abs(U.col(i).squaredNorm() - ti.squaredNorm() - rsi.squaredNorm()));
    
    // compute elements out of the diagonal n+j, n+i
    if (i < m-1) {
      rsj = R22.block(0, i+1, i, m-1-i);
      R22.block(i, i+1, 1, m-1-i) = (U.col(i).transpose() * U.rightCols(m-1-i) - ti.transpose() * R12.rightCols(m-1-i) - rsi.transpose() * rsj) / R22(i, i);
    }
  }
  
  // ::::::::::::::::::::::::::::::::::::::
  // update R
  Eigen::MatrixXd Rs(n+m, n+m); Rs.setZero();
  Rs.topLeftCorner(n, n) = R;
  Rs.topRightCorner(n, m) = R12;
  Rs.bottomRightCorner(m, m) = R22;
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  return Rs;
}

// ::::::::::::::::::::::::::::::::::::::
// thinQR update: delete one column at position k
// function to update triangular matrix R when a column u is deleted from matrix X at position k
// input R p x p
// k position at which the column is deleted
Eigen::MatrixXd thinqrdeletecol (const Eigen::MatrixXd& R, 
                                 const int& k) {
  
  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = R.cols();
  
  // ::::::::::::::::::::::::::::::::::::::
  // check if position k is greater/equal/less than p
  // if k > n stop: ERROR 
  // if k = n delete last column 
  // if k < n permute matrix
  // if (k > n) {Rcpp::Rcout << "\n Error: k > ncol(R)"; return nullptr;}
  if (k == n) 
    return R.topLeftCorner(n-1, k-1);
  
  double cc = 0.0, ss = 0.0;
  Eigen::Vector2d tmp; tmp.setZero();
  Eigen::Matrix2d Givens(2, 2); Givens.setZero();
  Eigen::MatrixXd R2 = R.bottomRightCorner(n-k+1, n-k);
  
  // ::::::::::::::::::::::::::::::::::::::
  // update R
  // for each column update R[i, i], R[i:(i+1), (i+1):(p-1)]
  for (i = 0; i < n-k; i++) {
    // Givens rotation 
    tmp = givens(R2(i, i), R2(i+1, i));
    cc  = tmp(0);
    ss  = tmp(1);
    Givens << cc, ss, -ss, cc;
    
    R2(i, i) = cc * R2(i, i) - ss * R2(i+1, i);
    if (i < n-k-1) {
      R2.block(i, i+1, 2, n-k-i-1) = Givens.transpose() * R2.block(i, i+1, 2, n-k-i-1);
      R2(i+1, i)                   = 0.0;
    }
  }
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  Eigen::MatrixXd Rs(n-1, n-1); Rs.setZero();
  Rs.topLeftCorner(k-1, k-1)     = R.topLeftCorner(k-1, k-1);
  Rs.topRightCorner(k-1, n-k)    = R.topRightCorner(k-1, n-k);
  Rs.bottomRightCorner(n-k, n-k) = R2.topRows(n-k);
  
  // ::::::::::::::::::::::::::::::::::::::
  // return output
  return Rs;
}

// ::::::::::::::::::::::::::::::::::::::
// thinQR update: delete m adjacent columns at position k
// function to update QR factorization of X when a block of m columns is deleted at position k
// input R p x p
// k position at which the block is deleted
// m number of adjacent columns to be deleted
Eigen::MatrixXd thinqrdeletemcols_adj (const Eigen::MatrixXd& R, 
                                       const int& k,
                                       const int& m) {
  
  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int i, n = R.cols();
  
  // ::::::::::::::::::::::::::::::::::::::
  // check if position k is greater/equal/less than p
  // if k > p-m +1 stop: ERROR 
  // if k = n-m+1 delete last column 
  // if k < n-m+1 permute matrix
  //if ((k-1) > (n-m)) {Rcpp::Rcout << "\n Error: k > ncol(R)-m+1"; return 0;}
  if ((k-1) == (n-m)) 
    return R.topLeftCorner(n-m, k-1);
  
  double beta = 0.0, mu = 0.0;
  Eigen::VectorXd tmp(m+3); tmp.setZero();
  Eigen::VectorXd v(m+1); v.setZero();
  Eigen::VectorXd v1(m+1); v1.setZero();
  Eigen::MatrixXd R2 = R.bottomRightCorner(n-k+1, n-k-m+1);
  
  // ::::::::::::::::::::::::::::::::::::::
  // update R
  // for each column update R[i, i], R[i:(i+m), (i+1):(p-m)]
  for (i = 0; i < R2.cols()-1; i++) {
    // Householder reflection
    tmp  = householder(R2.block(i, i, m+1, 1));
    v    = tmp.head(m+1);
    beta = tmp(m+1);
    mu   = tmp(m+2);
    v1   = beta * v;
    
    R2(i, i) = mu;
    R2.block(i+1, i, m, 1).setZero();
    R2.block(i, i+1, m+1, R2.cols()-i-1) -= v1 * (v.transpose() * R2.block(i, i+1, m+1, R2.cols()-i-1));
  }
  R2(R2.cols()-1, R2.cols()-1) = R2.block(R2.cols()-1, R2.cols()-1, m+1, 1).norm();
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  Eigen::MatrixXd Rs(n-m, n-m); Rs.setZero();
  Rs.topLeftCorner(k-1, k-1)             = R.topLeftCorner(k-1, k-1);
  Rs.topRightCorner(k-1, n-k-m+1)        = R.topRightCorner(k-1, n-k-m+1);
  Rs.bottomRightCorner(n-k-m+1, n-k-m+1) = R2.topRows(n-k-m+1);
  
  // ::::::::::::::::::::::::::::::::::::::
  // return output
  return Rs;
}

// ::::::::::::::::::::::::::::::::::::::
// thinQR update: delete m non-adjacent columns
// function to update QR factorization of X when a block of m columns is deleted at position k
// input R p x p
// k m-dim, columns to be deleted
Eigen::MatrixXd thinqrdeletemcols (const Eigen::MatrixXd& R, 
                                   const Eigen::VectorXi& k) {
  
  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = R.cols(), m = k.size(), u = 0, q = 0, mi = 0;
  double cc = 0.0, ss = 0.0, beta = 0.0, mu = 0.0;
  
  // ::::::::::::::::::::::::::::::::::::::
  // check: if m = 1 use alg for deleting one column
  // check: if m columns are adjacent ((k(m-1) - k(0)) == (m-1)) use alg for deleting a block of cols
  if (m == 1) 
    return thinqrdeletecol(R, k(0));
  if ((k(m-1) - k(0)) == (m-1)) 
    return thinqrdeletemcols_adj(R, k(0), m);
  
  // ::::::::::::::::::::::::::::::::::::::
  // delete columns from R
  Eigen::ArrayXi knot = setdiff(seq(n)+1, k);
  u                   = knot(knot.size()-1);
  q                   = m - n + u;
  if (q == 1) 
    return thinqrdeletecol(R.topLeftCorner(u, u), k(0));
  if ((k(q-1) - k(0)) == (q-1)) 
    return thinqrdeletemcols_adj(R.topLeftCorner(u, u), k(0), q);
  
  knot = knot.tail(knot.size()-k(0)+1).eval();
  Eigen::VectorXi ai(knot.size()); ai.setZero();
  ai(0) = knot(0) - k(0); 
  
  // ::::::::::::::::::::::::::::::::::::::
  // update R
  // Givens rotation
  Eigen::Matrix2d Givens; Givens.setZero();
  
  // Householder reflections
  Eigen::VectorXd v(ai(0)+1); v.setZero();
  Eigen::VectorXd v1(ai(0)+1); v.setZero();
  
  Eigen::MatrixXd R2(n-k(0)+1, knot.size());
  for (i = 0; i < knot.size(); i++) 
    R2.col(i) = R.block(k(0)-1, knot(i)-1, R2.rows(), 1);
  
  Eigen::Vector2d tmp_g; tmp_g.setZero();
  Eigen::VectorXd tmp(m); tmp.setZero();
  
  for (i = 0; i < R2.cols()-1; i++) {
    mi = ai(i);
  
    // compute a(i+1)
    ai(i+1) = mi + (knot(i+1)-knot(i)) - 1;
    if (mi > ai(i-1)) 
      v.resize(mi+1);
    
    if (mi > 1) {
      // Householder reflection
      tmp  = householder(R2.block(i, i, mi+1, 1));
      v    = tmp.head(mi+1);
      beta = tmp(mi+1);
      mu   = tmp(mi+2);
      v1   = beta * v;
      
      R2(i, i) = mu;
      R2.block(i+1, i, mi, 1).setZero();
      R2.block(i, i+1, mi+1, R2.cols()-i-1) -= v1 * (v.transpose() * R2.block(i, i+1, mi+1, R2.cols()-i-1));
    } else {
      // Givens rotation
      tmp_g = givens(R2(i, i), R2(i+1, i));
      cc    = tmp_g(0);
      ss    = tmp_g(1);
      Givens << cc, ss, -ss, cc;
      
      R2(i, i)                           = cc * R2(i, i) - ss * R2(i+1, i);
      R2(i+1, i)                         = 0.0;
      R2.block(i, i+1, 2, R2.cols()-i-1) = Givens.transpose() * R2.block(i, i+1, 2, R2.cols()-i-1);
    }
  }
  R2(R2.cols()-1, R2.cols()-1) = R2.block(R2.cols()-1, R2.cols()-1, m+1, 1).norm();
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  Eigen::MatrixXd Rs(n-m, n-m); Rs.setZero();
  Rs.topLeftCorner(k(0)-1, k(0)-1) = R.topLeftCorner(k(0)-1, k(0)-1);
  for (i = k(0)-1; i < Rs.cols(); i++) 
    Rs.block(0, i, k(0)-1, 1) = R.block(0, knot(i-k(0)+1)-1, k(0)-1, 1);
  Rs.bottomRightCorner(R2.cols(), R2.cols()) = R2.topRows(R2.rows()-m+1);
  // ::::::::::::::::::::::::::::::::::::::
  // return output
  return Rs;
}

// ::::::::::::::::::::::::::::::::::::::
// thinQR update: add one row
// function to update triangular matrix R when a row u is added to matrix X
// input R p x p
// u p-dim row to be added
Eigen::MatrixXd thinqraddrow (const Eigen::MatrixXd& R,
                              const Eigen::VectorXd& u) {
  
  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = R.cols(), p = R.cols();
  double cc = 0.0, ss = 0.0;
  Eigen::Vector2d tmp_g; tmp_g.setZero();
  Eigen::VectorXd tmp(n); tmp.setZero();
  Eigen::VectorXd us = u;
  Eigen::MatrixXd Rs = R;
  
  // ::::::::::::::::::::::::::::::::::::::
  // update R
  // for each column update R[i, i], R[i, (i+1):p], u[, (i+1):p]
  for (i = 0; i < n; i++) {
    // update R[i, i]
    tmp_g    = givens(Rs(i, i), us(i));
    cc       = tmp_g(0);
    ss       = tmp_g(1);
    Rs(i, i) = cc * Rs(i, i) - ss * us(i);
    
    // update R[i, (i+1):p] and u[, (i+1):p]
    if (i < n-1) {
      tmp.transpose()            = Rs.block(i, i+1, 1, n-i-1);
      Rs.block(i, i+1, 1, n-i-1) = cc * tmp.transpose() - ss * us.tail(n-i-1).transpose();
      us.tail(n-i-1)             = ss * tmp + cc * us.tail(n-i-1);
    }
  }
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  return Rs.topRows(p);
}

// ::::::::::::::::::::::::::::::::::::::
// thinQR update: add m rows
// function to update QR factorization of X when a block of rows U is added
// input R p x p
// U m x p rows to be added
Eigen::MatrixXd thinqraddmrows (const Eigen::MatrixXd& R,
                                const Eigen::MatrixXd& U) {

  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = R.cols(), m = U.rows(), p = R.cols();
  double beta = 0.0, mu = 0.0;
  Eigen::VectorXd v(m); v.setZero();
  Eigen::VectorXd tmp(m+1); tmp.setZero();
  Eigen::VectorXd tmp_h(m+3); tmp_h.setZero();
  
  Eigen::MatrixXd Rs = R;
  Eigen::MatrixXd Us = U;
  
  // ::::::::::::::::::::::::::::::::::::::
  // update R
  // for each column update R[i, i], R[i, (i+1):p], U[, (i+1):p]
  for (i = 0; i < n-1; i++) {
    // Householder reflection
    tmp.resize(m+1);
    tmp(0)            = Rs(i, i);
    tmp.segment(1, m) = Us.col(i);
    
    tmp_h = householder(tmp);
    v     = tmp_h.head(m+1);
    beta  = tmp_h(m+1);
    mu    = tmp_h(m+2);
    
    // update R
    Rs(i, i) = mu;
    tmp.resize(n-i-1);
    tmp = beta * (Rs.block(i, i+1, 1, n-i-1).transpose() + (v.segment(1, v.size()-1).transpose() * Us.rightCols(n-i-1)).transpose());
    
    Rs.block(i, i+1, 1, n-i-1) -= tmp.transpose();
    Us.rightCols(n-i-1)        -= v.segment(1, v.size()-1) * tmp.transpose();
  }
  Rs(n-1, n-1) = std::sqrt(std::pow(Rs(n-1, n-1), 2) + Us.col(n-1).transpose() * Us.col(n-1));
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  return Rs.topRows(p);
}

// ::::::::::::::::::::::::::::::::::::::
// thinQR update: delete one row
// function to update triangular matrix R when a row u is deleted from matrix X
// input R p x p
// u p-dim row to be deleted
Eigen::MatrixXd thinqrdeleterow (const Eigen::MatrixXd& R,
                                 const Eigen::VectorXd& u) {
  
  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = R.cols();
  double cc = 0.0, ss = 0.0, r = 0.0;
  Eigen::VectorXd us = u;
  Eigen::MatrixXd Rs = R;
  
  // ::::::::::::::::::::::::::::::::::::::
  // update R
  // for each column update R[i, i], R[i, (i+1):p], u[, (i+1):p]
  for (i = 0; i < n ; i++) {
    // update R[i, i]
    r = Rs(i, i);
    Rs(i, i) = std::sqrt(std::abs(std::pow(Rs(i, i), 2) - std::pow(us(i), 2)));
    
    // update R[i, (i+1):p] and u[, (i+1):p]
    if (i < n-1) {
      // Givens rotation
      cc                         = Rs(i, i) / r;
      ss                         = -us(i) / r;
      Rs.block(i, i+1, 1, n-i-1) = (Rs.block(i, i+1, 1, n-i-1) + ss * us.tail(n-i-1).transpose()) / cc;
      us.tail(n-i-1)             = ss * Rs.block(i, i+1, 1, n-i-1).transpose() + cc * us.tail(n-i-1);
    }
  }
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  return Rs;
}

// ::::::::::::::::::::::::::::::::::::::
// thinQR update: delete m rows
// function to update triangular matrix R when block of rows U is deleted from matrix X
// input R p x p
// U m x p block of rows to be deleted
Eigen::MatrixXd thinqrdeletemrows (const Eigen::MatrixXd& R,
                                   const Eigen::MatrixXd& U) {
  
  // ::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = R.cols(), m = U.rows();
  double beta = 0.0, r = 0.0, s = 0.0;
  Eigen::VectorXd v(m+1); v.setZero();
  Eigen::VectorXd v1(m); v1.setZero();
  Eigen::VectorXd tmp(n); tmp.setZero();
  Eigen::MatrixXd Rs = R, Us = U;
  
  // ::::::::::::::::::::::::::::::::::::::
  // update R
  // for each column update R[i, i], R[i, (i+1):p], U[, (i+1):p]
  for (i = 0; i < n ; i++) {
    // update R[i, i]
    s        = Us.col(i).transpose() * Us.col(i);
    r        = Rs(i, i);
    Rs(i, i) = std::sqrt(std::abs(std::pow(Rs(i, i), 2) - s));
    if (i < n-1) {
      // Householder reflection
      v.setZero();                                      // versione modificata da mauro
      v(0)                = Rs(i, i) - r;
      v.tail(v.size()-1)  = Us.col(i);
      beta                = 2.0 * std::pow(v(0), 2) / (std::pow(v(0), 2) + s);
      v.tail(v.size()-1) /= v(0);
      v1                  = v.tail(v.size()-1);
      
      // update R[i, (i+1):p]
      tmp.resize(n-i-1);
      tmp.transpose()            = beta * v1.transpose() * Us.rightCols(n-i-1);
      Rs.block(i, i+1, 1, n-i-1) = (Rs.block(i, i+1, 1, n-i-1) + tmp.transpose()) / (1.0 - beta);
      
      // update U[, (i+1):p]
      Us.rightCols(n-i-1) -= v1 * (beta * Rs.block(i, i+1, 1, n-i-1) + tmp.transpose());
    }
  }
  
  // ::::::::::::::::::::::::::::::::::::::
  // output
  return Rs;
}








// end file
