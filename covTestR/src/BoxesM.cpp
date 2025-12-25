#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double BoxesMStat(List x) {
  int len = x.length();
  arma::mat pmat = x[1];
  List samplecov(len);
  double p = pmat.n_cols;
  double ntot = 0;
  arma::mat Apool(p, p);
  Apool.fill(0);
  arma::vec ns(len);

 for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    int nsi = mats.n_rows;
    int ps = mats.n_cols;
    arma::mat covar = cov(mats);
    samplecov[i] = covar;
    p = ps;
    ntot += nsi - 1.0;
    ns[i] = nsi;
    Apool += covar * (nsi - 1.0);
  }

  arma::mat pooledCov = Apool / ntot;

  double stat = 0;
  for(int i = 0; i < len; ++i){
    arma::mat sampcov = samplecov[i];
    double n = ns[i] - 1.0;
    stat += n * log(det(sampcov));
  }

  return ntot * log(det(pooledCov)) - stat;
}

