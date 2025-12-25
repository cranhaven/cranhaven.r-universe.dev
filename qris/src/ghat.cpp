#include <RcppArmadillo.h>
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

//' @noRd
// [[Rcpp::export]]
arma::vec ghatC(arma::vec Time, arma::vec censor, arma::vec wgt) {
  arma::vec T0 = arma::sort(arma::unique(Time));
  int n = T0.n_elem;
  arma::vec d(n, arma::fill::zeros);
  arma::vec r(n, arma::fill::zeros); 
  for (int i = 0; i < n; i++) {
    arma::uvec ind1 = find(Time == T0[i]);
    d[i] = sum(censor.elem(ind1) % wgt.elem(ind1));
    r(span(0, i)) += sum(wgt.elem(ind1));
    // r.elem(regspace<uvec>(0, i)) += sum(wgt.elem(ind1));
  }
  return(cumprod(1 - d / r));
}

//' @noRd
//  [[Rcpp::export]]
arma::mat isObjE(arma::vec b, arma::mat X, arma::mat H,
                 arma::vec I, arma::vec logT, arma::vec D,
                 double t0, double Q, int B) {
  int p = b.n_elem;
  int n = logT.n_elem;
  arma::vec T = exp(logT); 
  arma::vec uniqT = arma::sort(arma::unique(T));
  int m = uniqT.n_elem;
  arma::mat out(p, B, arma::fill::zeros); 
  arma::mat m2 = normcdf((X * b - logT) / sqrt(sum(X % (X * H), 1)));
  for (int i = 0; i < B; i++) {
    arma::vec eta(n, arma::fill::randu);
    eta = -log(eta);
    arma::vec W(n, arma::fill::ones);
    if (any(D > 0)) {
      arma::vec survp = ghatC(T, 1 - D, eta);
      double ghatstart0 = 1;
      if (t0 > min(uniqT)) ghatstart0 = survp(index_max(uniqT > t0) - 1);
      arma::vec survpi(n, arma::fill::ones);
      if (m == n) {
	survpi(sort_index(T)) = survp;
      } else {
        for (int j = 0; j < n; j++) {
  	  survpi[j] = survp(find(uniqT == T[j])).eval()(0);
        }
      }
      W = D / survpi * ghatstart0;
    } 
    W.replace(datum::inf, datum::nan);
    W.replace(datum::nan, max(W));
    arma::mat m1 = X;
    m1.each_col() %= I % eta;  
    out.col(i) = m1.t() * (m2 % W - Q);
  }
  return out / n;
}
