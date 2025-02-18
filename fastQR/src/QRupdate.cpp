// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                 :::::::::::::::::::: //
// ::::::::::::::::::::    QR update    :::::::::::::::::::: //
// ::::::::::::::::::::                 :::::::::::::::::::: //
// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //

// Utility routines

// Authors:
//           Bernardi Mauro, University of Padova
//           Last update: october 7, 2024

// [[Rcpp::depends(RcppArmadillo)]]
#include "QRupdate.h"
#include <time.h>


/* :::::::::::::::::::::::::::::::::::::::::::::::
The following functions are for adding rows or columns
    from the QR decomposition:
 
    1. add 1 column   : qraddcol;
    2. add m>1 coluns : qraddmcols;
    3. add 1 row      : qraddrow;
    4. add m>1 rows   : qraddmrows;
  :::::::::::::::::::::::::::::::::::::::::::::::         */

// :::::::::::::::::::::::::::::::::::::::::::::::
// QR update: add one column at position k 
// function to update QR factorization of X when a column is added at position k
// input Q n x n
// input R p x p
// k position of the column to be added
// u n-dim column to be added
Rcpp::List qraddcol (const Eigen::MatrixXd& Q,
                     const Eigen::MatrixXd& R,
                     const int& k,
                     const Eigen::VectorXd& u) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = Q.rows(), p = R.cols();
  double cc = 0.0, ss = 0.0;
  Eigen::Vector2d tmp; tmp.setZero();
  Eigen::Matrix2d Givens; Givens.setZero();
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // set R and Q
  Eigen::MatrixXd Rs(p+1, p+1); Rs.setZero();
  if (k == (p+1)) {
    Rs.topLeftCorner(p, p) = R;
  } else {
    Rs.topLeftCorner(p, k-1)    = R.leftCols(k-1);
    Rs.topRightCorner(p, p-k+1) = R.rightCols(p-k+1);
  }
  Eigen::MatrixXd Qs = Q;
  Eigen::VectorXd v  = Qs.transpose() * u;
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R and Q
  for (i = (n-1); i > k-1; i--) {
    // Givens rotation 
    tmp = givens(v(i-1), v(i));
    cc  = tmp(0);
    ss  = tmp(1);
    Givens << cc, ss, -ss, cc;
    
    // update v 
    v(i-1) = cc * v(i-1) - ss * v(i);
    v(i)   = 0.0;
    
    // update R 
    if (i <= p)  
      Rs.block(i-1, i, 2, p-i+1) = Givens.transpose() * Rs.block(i-1, i, 2, p-i+1);
    
    // update Q 
    Qs.block(0, i-1, Qs.rows(), 2) = Qs.block(0, i-1, Qs.rows(), 2) * Givens;
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  Rs.col(k-1) = v.head(p+1);
  return Rcpp::List::create(Rcpp::Named("Q") = Qs,
                            Rcpp::Named("R") = Rs);
}

// :::::::::::::::::::::::::::::::::::::::::::::::
// QRupdate: add m columns at position k
// function to update QR factorization of X when a block of m columns is added at position k
// input Q
// input R
// k position of the column to be added
// U n x m block to be added
Rcpp::List qraddmcols (const Eigen::MatrixXd& Q,
                       const Eigen::MatrixXd& R,
                       const int& k, const
                       Eigen::MatrixXd& U) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, j = 0, n = Q.rows(), p = R.cols(), m = U.cols();
  double cc = 0.0, ss = 0.0;
  Eigen::Vector2d tmp; tmp.setZero();
  Eigen::Matrix2d Givens; Givens.setZero();
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // set R and Q
  Eigen::MatrixXd Qs = Q;
  Eigen::MatrixXd Rs(p+m, p+m); Rs.setZero();
  
  if (k == (p+1)) {
    Rs.topLeftCorner(p, p) = R;
  } else {
    if (k == 1) {
      Rs.topRightCorner(p, p) = R;
    } else {
      Rs.topLeftCorner(p, k-1) = R.leftCols(k-1);
      Rs.topRightCorner(p, p-k+1) = R.rightCols(p-k+1);
    }
  }
  Eigen::MatrixXd V = Qs.transpose() * U;
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R and Q
  for (j = 0; j < m; j++) {
    for (i = n-1; i > k-1+j; i--) {
      // Givens rotation 
      tmp = givens(V(i-1, j), V(i, j));
      cc  = tmp(0);
      ss  = tmp(1);
      Givens << cc, ss, -ss, cc;
      
      // update V 
      V(i-1, j) = cc * V(i-1, j) - ss * V(i, j);
      V(i, j)   = 0.0;
      if (j < (m-1)) 
        V.block(i-1, j+1, 2, m-j-1) = Givens.transpose() * V.block(i-1, j+1, 2, m-j-1);
      
      // update R 
      if (i <= (p+j))
        Rs.block(i-1, i-j+m-1, 2, p-i+j+1) = Givens.transpose() * Rs.block(i-1, i-j+m-1, 2, p-i+j+1); 
      
      // update Q 
      Qs.block(0, i-1, n, 2) = Qs.block(0, i-1, n, 2) * Givens;
    }
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  Rs.block(0, k-1, p+m, m) = V.topRows(p+m);
  return Rcpp::List::create(Rcpp::Named("Q") = Qs,
                            Rcpp::Named("R") = Rs);
}

// :::::::::::::::::::::::::::::::::::::::::::::::
// QR update: add one row at position k
// function to update QR factorization of X when a row is added at position k
// input Q n x n
// input R n x p
// k position of the row to be added
// U p-dim row to be added (matrix)
Rcpp::List qraddrow (const Eigen::MatrixXd& Q,
                     const Eigen::MatrixXd& R,
                     const int& k,
                     const Eigen::RowVectorXd& u) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = Q.rows(), p = R.cols();
  
  double cc = 0.0, ss = 0.0;
  Eigen::Vector2d tmp_g; tmp_g.setZero();
  Eigen::RowVectorXd tmp(p); tmp.setZero();
  
  // permute Q 
  Eigen::MatrixXd Qs(n+1, n+1); Qs.setZero();
  if (k != 1) 
    Qs.topLeftCorner(k-1, n) = Q.topRows(k-1);
  
  Qs(k-1, n) = 1.0;
  if (k != (n+1))
    Qs.bottomLeftCorner(n-k+1, n) = Q.bottomRows(n-k+1);
  
  Eigen::MatrixXd Rs    = R;
  Eigen::RowVectorXd us = u;
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R and Q
  for (i = 0; i < p; i++) {
    
    // Givens rotation 
    tmp_g = givens(R(i, i), us(i));
    cc    = tmp_g(0);
    ss    = tmp_g(1);
    
    // update R 
    Rs(i, i) = cc * Rs(i, i) - ss * us(0, i);
    if (i+1 < p) {
      tmp.resize(p-i-1);
      tmp                        = Rs.block(i, i+1, 1, p-i-1);
      Rs.block(i, i+1, 1, p-i-1) = cc * tmp - ss * us.segment(i+1, p-i-1);
      
      // update u 
      us.segment(i+1, p-i-1) = ss * tmp + cc * us.segment(i+1, p-i-1);
    }
    
    // update Q 
    tmp       = Qs.col(i).transpose();
    Qs.col(i) = cc * Qs.col(i) - ss * Qs.col(n);
    Qs.col(n) = ss * tmp.transpose() + cc * Qs.col(n);
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  return Rcpp::List::create(Rcpp::Named("Q") = Qs,
                            Rcpp::Named("R") = Rs);
}

// :::::::::::::::::::::::::::::::::::::::::::::::
// QR update: add m rows at position k
// function to update QR factorization of X when a block of m rows is added at position k
// input Q n x n
// input R n x p
// k position of the rows to be added
// U m x p rows to be added (matrix)
Rcpp::List qraddmrows (const Eigen::MatrixXd& Q,
                       const Eigen::MatrixXd& R,
                       const int& k,
                       const Eigen::MatrixXd& U) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = Q.rows(), p = R.cols(), m = U.rows();
  double beta = 0.0, mu = 0.0;
  Eigen::VectorXd v(m); v.setZero();
  Eigen::VectorXd v1(m); v1.setZero();
  Eigen::VectorXd tmp(m+1); tmp.setZero();
  Eigen::VectorXd tmp_h(m+3); tmp_h.setZero();
  
  // Ri old row of R, Qi old column of Q 
  Eigen::VectorXd Ri(p); Ri.setZero();
  Eigen::VectorXd Qi(n+p); Qi.setZero();
  
  // permute Q 
  Eigen::MatrixXd Qs(n+m, n+m); Qs.setZero();
  if (k != 1) 
    Qs.topLeftCorner(k-1, n) = Q.topRows(k-1);
  
  Qs.block(k-1, n, m, m).setIdentity();
  if (k != (n+1))
    Qs.bottomLeftCorner(n-k+1, n) = Q.bottomRows(n-k+1);
  
  Eigen::MatrixXd Rs = R;
  Eigen::MatrixXd Us = U;
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R and Q
  for (i = 0; i < p; i++) {
    
    // Householder rotation 
    tmp(0)                 = Rs(i, i);
    tmp.tail(tmp.size()-1) = Us.col(i);
    tmp_h                  = householder(tmp);
    v                      = tmp_h.head(m+1);
    beta                   = tmp_h(m+1);
    mu                     = tmp_h(m+2);
    v1                     = beta * v.tail(v.size()-1);
    Rs(i, i)               = mu;
    
    // update R
    if ((i+1) < p) {
      Ri.resize(p-i-1);
      Ri                          = (beta * Rs.block(i, i+1, 1, p-i-1) + v1.transpose() * Us.rightCols(p-i-1)).transpose();
      Rs.block(i, i+1, 1, p-i-1) -= Ri.transpose();
      Us.rightCols(p-i-1)        -= v.tail(v.size()-1) * Ri.transpose();
    }
    
    // update Q 
    Qi              = beta * Qs.col(i) + Qs.rightCols(m) * v1;
    Qs.col(i)       = Qs.col(i) - Qi;
    Qs.rightCols(m) = Qs.rightCols(m) - Qi * v.tail(v.size()-1).transpose();
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  return Rcpp::List::create(Rcpp::Named("Q") = Qs,
                            Rcpp::Named("R") = Rs);
}

/* :::::::::::::::::::::::::::::::::::::::::::::::
The following functions are for removing rows or columns
    from the QR decomposition:
 
    1. remove 1 column          : qraddcol;
    2. remove m>1 coluns        : qraddmcols;
    3. remove 1 row             : qraddrow;
    4. remove m>1 rows          : qraddmrows;
    5. remove m>1 columns (adj) : qrdeletemcols_adj;
    6. remove m>1 columns       : qrdeletemcols;
  :::::::::::::::::::::::::::::::::::::::::::::::         */


// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// QR update: delete one row at position k
// function to update QR factorization of X when a row is deleted at position k
// input Q n x n
// input R n x p
// k position of the row to be deleted
Rcpp::List qrdeleterow (const Eigen::MatrixXd& Q,
                        const Eigen::MatrixXd& R,
                        const int& k) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, n = Q.rows(), p = R.cols();
  double cc = 0.0, ss = 0.0;
  Eigen::Vector2d tmp; tmp.setZero();
  Eigen::Matrix2d Givens; Givens.setZero();
  Eigen::MatrixXd Qs = Q;
  Eigen::MatrixXd Rs(p+1, p); Rs.setZero();
  Rs.topRows(p) = R;
  
  // define the vector q to remove from Q
  Eigen::RowVectorXd q = Q.row(k-1);
  
  // permute Q 
  if (k-1 != 0) 
    Qs.block(1, 0, k-1, Qs.cols()) = Qs.block(0, 0, k-1, Qs.cols()).eval();
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R and Q
  for (i = n-1; i > 0 ; i--) {
    // Givens rotation 
    tmp = givens(q(i-1), q(i));
    cc  = tmp(0);
    ss  = tmp(1);
    Givens << cc, ss, -ss, cc;
    
    // update q 
    q(i-1) = cc * q(i-1) - ss * q(i);
    
    // update R 
    if (i-1 < p)  
      Rs.block(i-1, i-1, 2, p-i+1) = Givens.transpose() * Rs.block(i-1, i-1, 2, p-i+1);
    
    // update Q 
    if (i-1 > 0)  
      Qs.block(1, i-1, n-1, 2) = Qs.block(1, i-1, n-1, 2) * Givens;
  }
  
  // update second last column of Q
  // no need to update last column 
  Qs.block(1, 1, n-1, 1) = ss * Qs.block(1, 0, n-1, 1) + cc * Qs.block(1, 1, n-1, 1);
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  return Rcpp::List::create(Rcpp::Named("Q") = Qs.bottomRightCorner(n-1, n-1),
                            Rcpp::Named("R") = Rs.bottomRightCorner(p, p));
}

// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// QR update: delete m rows at position k
// function to update QR factorization of X when a block of m rows is deleted at position k
// input Q n x n
// input R n x p
// k position of the rows to be deleted
// m number of adjacent rows to be deleted
Rcpp::List qrdeletemrows (const Eigen::MatrixXd& Q,
                          const Eigen::MatrixXd& R,
                          const int& k,
                          const int& m) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, j = 0, n = Q.rows(), p = R.cols();
  double cc = 0.0, ss = 0.0;
  
  Eigen::Vector2d tmp; tmp.setZero();
  Eigen::Matrix2d Givens; Givens.setZero();
  
  Eigen::MatrixXd Qs = Q;
  Eigen::MatrixXd Rs(p+m, p); Rs.setZero();
  Rs.topRows(p)     = R;
  Eigen::MatrixXd W = Qs.block(k-1, 0, m, Qs.cols());
  
  // permute Q 
  if (k-1 != 0) 
    Qs.block(m, 0, k-1, Qs.cols()) = Qs.block(0, 0, k-1, Qs.cols()).eval();
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R and Q
  for (j = 0; j < m; j++) {
    for (i = n-2; i >= j; i--) {
      // Givens rotation
      tmp = givens(W(j, i), W(j, i+1));
      cc  = tmp(0);
      ss  = tmp(1);
      Givens << cc, ss, -ss, cc;
      
      // update W 
      W(j, i) = cc * W(j, i) - ss * W(j, i+1);
      if (j+1 < m) 
        W.block(j+1, i, m-j-1, 2) = W.block(j+1, i, m-j-1, 2) * Givens;
      
      // update R 
      if (i <= p+j) 
        Rs.block(i, i-j, 2, p-i+j) = Givens.transpose() * Rs.block(i, i-j, 2, p-i+j);
      
      // update Q
      Qs.block(m, i, n-m, 2) = Qs.block(m, i, n-m, 2) * Givens;
    }
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  return Rcpp::List::create(Rcpp::Named("Q") = Qs.bottomRightCorner(n-m, n-m),
                            Rcpp::Named("R") = Rs.bottomRightCorner(p, p));
}

// :::::::::::::::::::::::::::::::::::::::::::::::
// QR update: delete one column at position k
// function to update QR factorization of X when a column is deleted at position k
// input Q n x n
// input R p x p
// k position of the column to be deleted
Rcpp::List qrdeletecol (const Eigen::MatrixXd& Q,
                        const Eigen::MatrixXd& R,
                        const int& k) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  const int p = R.cols();
  int i = 0;
  double cc = 0.0, ss = 0.0;
  Eigen::Vector2d tmp; tmp.setZero();
  Eigen::Matrix2d Givens; Givens.setZero();
  Eigen::MatrixXd Qs = Q;
  Eigen::MatrixXd Rs(p, p-1); Rs.setZero();
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // check position
  // stop if k = p
  if (k == p)
    return Rcpp::List::create(Rcpp::Named("Q") = Q,
                              Rcpp::Named("R") = R.topLeftCorner(p-1, p-1));
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // do algorithm if k != p
  if (k == 1) {
    Rs = R.rightCols(p-1);
  } else {
    Rs.leftCols(k-1) = R.leftCols(k-1);
    Rs.rightCols(p-k) = R.rightCols(p-k);
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R and Q
  for (i = k-1; i < p-1; i++) {
    // Givens rotation
    tmp = givens(Rs(i, i), Rs(i+1, i));
    cc  = tmp(0);
    ss  = tmp(1);
    Givens << cc, ss, -ss, cc;
    
    // update R
    Rs(i, i)   = cc * Rs(i, i) - ss * Rs(i+1, i);
    Rs(i+1, i) = 0.0;
    
    if (i < p-2)
      Rs.block(i, i+1, 2, p-i-2) = Givens.transpose() * Rs.block(i, i+1, 2, p-i-2);
    
    // update Q
    Qs.block(0, i, Qs.rows(), 2) = Qs.block(0, i, Qs.rows(), 2) * Givens;
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  return Rcpp::List::create(Rcpp::Named("Q") = Qs.leftCols(p-1),
                            Rcpp::Named("R") = Rs.topRows(p-1));
}

// :::::::::::::::::::::::::::::::::::::::::::::::
// QR update: delete m columns at position k
// function to update QR factorization of X when a block of m columns is deleted at position k
// input Q n x n
// input R n x p
// k position of the columns to be deleted
// m number of adjacent columns to be deleted
Rcpp::List qrdeletemcols_adj (const Eigen::MatrixXd& Q,
                              const Eigen::MatrixXd& R,
                              const int& k,
                              const int& m) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // check position
  // stop if k = p-m
  const int p = R.cols();
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0;
  double beta = 0.0, mu = 0.0;
  Eigen::VectorXd v(m); v.setZero();
  Eigen::VectorXd vs(m); vs.setZero();
  Eigen::VectorXd r(m); r.setZero();
  VectorXd tmp(m+3); tmp.setZero();
  Eigen::MatrixXd Qs = Q;
  Eigen::MatrixXd Rs(p, p-m); Rs.setZero();
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // check position
  // stop if k = p-m
  if ((k-1) == (p-m))
    return Rcpp::List::create(Rcpp::Named("Q") = Q,
                              Rcpp::Named("R") = R.topLeftCorner(p-m, p-m));
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // do algorithm if k != p-m
  if (k == 1) {
    Rs = R.rightCols(p-m);
  }
  else {
    Rs.leftCols(k-1)      = R.leftCols(k-1);
    Rs.rightCols(p-k-m+1) = R.rightCols(p-k-m+1);
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R and Q
  for (i = (k-1); i < (p-m); i++) {
    // Householder rotation
    r    = Rs.block(i, i, m+1, 1);
    tmp  = householder(r);
    v    = tmp.head(m+1);
    beta = tmp(m+1);
    mu   = tmp(m+2);
    vs   = beta * v;
    
    // update R
    Rs(i, i) = mu;
    Rs.block(i+1, i, m, 1).setZero();
    
    if (i < (p-m-1))
      Rs.block(i, i+1, m+1, p-m-i-1) -= vs * (v.transpose() * Rs.block(i, i+1, m+1, p-m-i-1));
    
    // update Q
    Qs.block(0, i, Qs.rows(), m+1) -= Qs.block(0, i, Qs.rows(), m+1) * vs * v.transpose();
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  return Rcpp::List::create(Rcpp::Named("Q") = Qs.leftCols(p-m),
                            Rcpp::Named("R") = Rs.topRows(p-m));
}

// :::::::::::::::::::::::::::::::::::::::::::::::
// QR update: delete m non-adjacent columns
// function to update QR factorization of X when a block of m columns is deleted at position k
// input R p x p
// k m-dim, columns to be deleted
Rcpp::List qrdeletemcols (const Eigen::MatrixXd& Q,
                          const Eigen::MatrixXd& R,
                          const Eigen::VectorXi& k) {
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // initialization
  int i = 0, j = 0, p = R.cols(), m = k.size(), mi = 0;
  double cc = 0.0, ss = 0.0, beta = 0.0, mu = 0.0;
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // check: if m = 1 use alg for deleting one column
  // check: if m columns are adjacent ((k(m-1) - k(0)) == (m-1)) use alg for deleting a block of cols
  if (m == 1)
    return qrdeletecol(Q, R, k(0));
  if ((k(m-1) - k(0)) == (m-1))
    return qrdeletemcols_adj(Q, R, k(0), m);
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // delete columns from R
  Eigen::ArrayXi knot = setdiff(seq(p)+1, k);
  int u = knot(knot.size()-1);
  int q = m - p + u;
  
  if (q == 1)
    return qrdeletecol(Q, R.topLeftCorner(u, u), k(0));
  
  if ((k(q-1) - k(0)) == (q-1))
    return qrdeletemcols_adj(Q, R.topLeftCorner(u, u), k(0), q);
  
  Eigen::MatrixXd Qs = Q;
  Eigen::MatrixXd Rs(p, knot.size());
  for (i = 0; i < knot.size(); i++)
    Rs.col(i) = R.col(knot(i)-1);
  
  knot = knot.tail(knot.size()-k(0)+1).eval();
  Eigen::VectorXi ai(knot.size()); ai.setZero();
  ai(0) = knot(0) - k(0);
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // update R
  // Givens rotation
  Eigen::Matrix2d Givens; Givens.setZero();
  
  // Householder reflections
  Eigen::VectorXd v(ai(0)+1); v.setZero();
  Eigen::VectorXd vs(ai(0)+1); v.setZero();
  Eigen::VectorXd r(ai(0)+1); r.setZero();

  // temp vectors
  Eigen::Vector2d tmp_g; tmp_g.setZero();
  Eigen::VectorXd tmp(m); tmp.setZero();
  
  // for each column apply Givens rotation or Householder reflections
  for (i = 0; i < knot.size(); i++) {
    // compute a(i+1)
    j  = i+k(0)-1;
    mi = ai(i);
    
    if (i < knot.size()-1)
      ai(i+1) = mi + (knot(i+1)-knot(i)) - 1;
    
    if (mi > 1) {
      r = Rs.block(j, j, mi+1, 1);
      
      // Householder reflection
      tmp  = householder(r);
      v    = tmp.head(mi+1);
      beta = tmp(mi+1);
      mu   = tmp(mi+2);
      vs   = beta * v;
      
      Rs(j, j) = mu;
      Rs.block(j+1, j, mi, 1).setZero();
      if (j < Rs.cols()-1)
        Rs.block(j, j+1, mi+1, Rs.cols()-j-1) -= vs * (v.transpose() * Rs.block(j, j+1, mi+1, Rs.cols()-j-1));
      
      // update Q
      Qs.block(0, j, Qs.rows(), mi+1) -= Qs.block(0, j, Qs.rows(), mi+1) * vs * v.transpose();
    }
    else {
      // Givens rotation
      tmp_g = givens(Rs(j, j), Rs(j+1, j));
      cc    = tmp_g(0);
      ss    = tmp_g(1);
      Givens << cc, ss, -ss, cc;
      
      Rs(j, j)   = cc * Rs(j, j) - ss * Rs(j+1, j);
      Rs(j+1, j) = 0.0;
      if (j < Rs.cols()-1)
        Rs.block(j, j+1, 2, Rs.cols()-j-1) = Givens.transpose() * Rs.block(j, j+1, 2, Rs.cols()-j-1);
      
      // update Q
      Qs.block(0, j, Qs.rows(), 2) = Qs.block(0, j, Qs.rows(), 2) * Givens;
    }
  }
  
  // :::::::::::::::::::::::::::::::::::::::::::::::
  // output
  return Rcpp::List::create(Rcpp::Named("Q") = Qs,
                            Rcpp::Named("R") = Rs.topRows(p-m));
}



















// end of file
