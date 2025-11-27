# include <RcppArmadillo.h>
# include <utility>
# include <algorithm>

// [[Rcpp::depends(RcppArmadillo)]]


arma::mat LogM(const arma::mat&);


// X and Y are matrices in which columns contain individual SO matrices. Implements recycling.
// [[Rcpp::export()]]
Rcpp::NumericVector distSO(arma::mat& X, arma::mat& Y) {

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

  arma::mat tmp(p, p), Xmat(p, p), XInv(p, p), Ymat(p, p);

  if (X.n_cols == 1) {
    XInv = arma::reshape(X, p, p).t();
    for (arma::uword i = 0; i < n; i++) {
      Ymat = arma::reshape(Y.col(i), p, p);
      tmp = LogM(Ymat * XInv);
      res(i) = arma::norm(tmp, "fro") / sqrt(2.0); // Care only about the lower triangle of the log matrix
    }
  } else { // X.n_cols > 1
    for (arma::uword i = 0; i < n; i++) {
      XInv = arma::reshape(X.col(i), p, p).t();
      Ymat = arma::reshape(Y.col(i), p, p);
      tmp = LogM(Ymat * XInv);
      res(i) = arma::norm(tmp, "fro") / sqrt(2.0);
    }
  }

  return res;
}



// X and Y are SO matrices in which columns contain individual SO matrices. Implements recycling.
// [[Rcpp::export()]]
arma::mat logSO(const arma::mat& p, const arma::mat& X) {

  if (p.n_cols == 0 || X.n_cols == 0) {
    arma::uword pp = sqrt(p.n_rows);
    arma::mat res(pp * (pp - 1) / 2, 0); // dimTangent
    return res;
  }

  arma::uword pp = sqrt(p.n_rows);
  arma::uword n = std::max(p.n_cols, X.n_cols);
  arma::mat res(pp * pp, n);

  arma::mat Xmat(pp, pp), pInv(pp, pp);

  if (p.n_cols == 1) {
    pInv = reshape(p, pp, pp).t();
    for (arma::uword i = 0; i < n; i++) {
      Xmat = arma::reshape(X.col(i), pp, pp);
      res.col(i) = vectorise(LogM(Xmat * pInv));
    }
  } else if (p.n_cols > 1 && X.n_cols == 1) {
    Xmat = arma::reshape(X, pp, pp);
    for (arma::uword i = 0; i < n; i++) {
      pInv = reshape(p.col(i), pp, pp).t();
      res.col(i) = vectorise(LogM(Xmat * pInv));
    }
  } else if (p.n_cols > 1 && X.n_cols > 1) {
    for (arma::uword i = 0; i < n; i++) {
      Xmat = arma::reshape(X.col(i), pp, pp);
      pInv = reshape(p.col(i), pp, pp).t();
      res.col(i) = vectorise(LogM(Xmat * pInv));
    }
  }

  arma::uvec lowerInd = arma::trimatl_ind(size(Xmat), -1);

  return res.rows(lowerInd);
}

