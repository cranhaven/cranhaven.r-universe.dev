# include <RcppArmadillo.h>
# include <utility>
# include <algorithm>

// [[Rcpp::depends(RcppArmadillo)]]

//' Matrix exponential
//'
//' \code{ExpM(X)} computes the matrix exponential using Armardillo. 
//'
//' @param X A square matrix 
//' @returns A matrix having the same size as the input
//' @export
// [[Rcpp::export()]]
arma::mat ExpM(const arma::mat& X) {
  // if (X.is_symmetric()) {
    // return arma::expmat_sym(X);
  // } else {
    return arma::expmat(X);
  // }
}


//' Matrix logarithm
//'
//' \code{LogM(X)} computes the matrix logarithm of a general matrix using Armardillo. The returned value is a complex matrix but converted into a real one.
//'
//' @param X A square matrix 
//' @returns A matrix having the same size as the input
//' @export
// [[Rcpp::export()]]
arma::mat LogM(const arma::mat& X) {
  arma::cx_mat Xc(X, arma::zeros<arma::mat>(X.n_rows, X.n_cols));
  return arma::real(arma::logmat(Xc));
}


//' Matrix logarithm of a symmetric positive definite
//'
//' \code{LogMSPD(X)} computes the matrix logarithm of an SPD matrix using Armardillo. The returned value is a real matrix.
//'
//' @param X An SPD matrix 
//' @returns A symmetric matrix
//' @export
// [[Rcpp::export()]]
arma::mat LogMSPD(const arma::mat& X) {
  return arma::logmat_sympd(X);
}


// rieLog.AffInv, rieExp.AffInv, distance.AffInv, metric, norm
// Geodesic distance between one and one matrix. This is slower than the symmetric implementation below
// [[Rcpp::export()]]
double distAffInv11(const arma::mat& X, const arma::mat& Y) {
  arma::mat A = arma::real(arma::logmat(arma::solve(X, Y)));
  double D = sqrt(trace( A * A ));
  return D;
}


// [[Rcpp::export()]]
double distAffInv11_2(const arma::mat& X, const arma::mat& Y) {

  arma::mat XHalfInv = arma::inv_sympd(arma::sqrtmat_sympd(X));
  arma::mat A = arma::logmat_sympd(XHalfInv * Y * XHalfInv);
  return arma::norm(A, "fro");

}


// X is an SPD matrix. Each column of Y is a vectorized SPD matrix.
// [[Rcpp::export()]]
Rcpp::NumericVector distAffInv1m(const arma::mat& X, const arma::mat& Y) {

  arma::uword p = X.n_rows, 
              n = Y.n_cols;
  Rcpp::NumericVector res(n);

  arma::mat XHalfInv = arma::inv_sympd(arma::sqrtmat_sympd(X));

  arma::mat A(p, p); // , yy(p, p);
  for (arma::uword i = 0; i < n; i++) {
    // yy.fill(Y.col(i));
    A = arma::logmat_sympd(XHalfInv * reshape(Y.col(i), p, p) * XHalfInv);
    res(i) = arma::norm(A, "fro");
  }

  return res;
}


// X and Y are matrices in which columns contain individual SPD matrices. Implements recycling.
// [[Rcpp::export()]]
Rcpp::NumericVector distAffInv(arma::mat& X, arma::mat& Y) {

  if (X.n_cols == 0 || Y.n_cols == 0) {
    Rcpp::NumericVector res(0);
    return res;
  }

  arma::uword p = sqrt(X.n_rows);

  if (X.n_cols > Y.n_cols) {
    std::swap<arma::mat>(X, Y);
  }

  arma::uword n = Y.n_cols;
  Rcpp::NumericVector res(n);

  arma::mat tmp(p, p), Xmat(p, p), Ymat(p, p), XHalfInv(p, p);

  if (X.n_cols == 1) {
    Xmat = arma::reshape(X, p, p);
    XHalfInv = arma::inv_sympd(arma::sqrtmat_sympd(Xmat));
    for (arma::uword i = 0; i < n; i++) {
      Ymat = arma::reshape(Y.col(i), p, p);
      tmp = arma::logmat_sympd(XHalfInv * Ymat * XHalfInv);
      res(i) = arma::norm(tmp, "fro");
    }
  } else { // X.n_cols > 1
    for (arma::uword i = 0; i < n; i++) {
      Xmat = arma::reshape(X.col(i), p, p);
      Ymat = arma::reshape(Y.col(i), p, p);
      XHalfInv = arma::inv_sympd(arma::sqrtmat_sympd(Xmat));
      tmp = arma::logmat_sympd(XHalfInv * Ymat * XHalfInv);
      res(i) = arma::norm(tmp, "fro");
    }
  }

  return res;
}

// // X and Y are both SPD matrices.
// // [[Rcpp::export()]]
// arma::mat logAffInv11(const arma::mat& X, const arma::mat& Y) {

  // arma::mat XHalfInv = arma::inv_sympd(arma::sqrtmat_sympd(X));
  // arma::mat A = arma::logmat_sympd(XHalfInv * Y * XHalfInv);
  // return A;

// }


// X and Y are SPD matrices in which columns contain individual SPD matrices. Implements recycling.
// [[Rcpp::export()]]
arma::mat logAffInv(const arma::mat& p, const arma::mat& X) {

  if (p.n_cols == 0 || X.n_cols == 0) {
    arma::mat res(p.n_rows, 0);
    return res;
  }

  arma::uword pp = sqrt(p.n_rows);
  arma::uword n = std::max(p.n_cols, X.n_cols);
  arma::mat res(pp * pp, n);

  arma::mat Xmat(pp, pp), pmat(pp, pp), pHalfInv(pp, pp), tmp(pp, pp);

  if (p.n_cols == 1) {
    pmat = reshape(p, pp, pp);
    pHalfInv = arma::inv_sympd(arma::sqrtmat_sympd(pmat));
    for (arma::uword i = 0; i < n; i++) {
      Xmat = arma::reshape(X.col(i), pp, pp);
      res.col(i) = vectorise(arma::logmat_sympd(pHalfInv * Xmat * pHalfInv));
    }
  } else if (p.n_cols > 1 && X.n_cols == 1) {
    Xmat = arma::reshape(X, pp, pp);
    for (arma::uword i = 0; i < n; i++) {
      pmat = arma::reshape(p.col(i), pp, pp);
      pHalfInv = arma::inv_sympd(arma::sqrtmat_sympd(pmat));
      res.col(i) = vectorise(arma::logmat_sympd(pHalfInv * Xmat * pHalfInv));
    }
  } else if (p.n_cols > 1 && X.n_cols > 1) {
    for (arma::uword i = 0; i < n; i++) {
      Xmat = arma::reshape(X.col(i), pp, pp);
      pmat = arma::reshape(p.col(i), pp, pp);
      pHalfInv = arma::inv_sympd(arma::sqrtmat_sympd(pmat));
      res.col(i) = vectorise(arma::logmat_sympd(pHalfInv * Xmat * pHalfInv));
    }
  }

  return res;
}
