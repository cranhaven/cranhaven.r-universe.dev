#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Chaipitak2013poolStat(List x) {
  int len = x.length();
  arma::vec a2i(len);
  arma::mat pmat = x[1];
  double p = pmat.n_cols;
  double ntot = 0;
  arma::mat Apool(p, p);
  Apool.fill(0);
  double ninv = 0;
  double ninv2 = 0;

 for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    double ns = mats.n_rows;
    double ps = mats.n_cols;
    arma::mat covar = cov(mats);
    double cov2trace = trace(covar * covar);
    double covtrace = trace(covar);


    a2i[i] = pow(ns - 1.0, 2.0) /
      ps * (ns - 2.0) * (ns + 1.0) *
      (cov2trace - (1.0 / (ns - 1.0))) * pow(covtrace, 2.0);

    ntot += ns - 1.0;
    Apool += covar * (ns - 1.0);
    ninv += 1.0 / (ns - 1.0);
    ninv2 += pow(ns - 1.0, -2.0);
  }

  arma::mat pooledCov = Apool / ntot;
  double pooledcov2trace = trace(pooledCov * pooledCov);
  double pooledcovtrace = trace(pooledCov);
  double pooledcov4trace = trace(pooledCov * pooledCov * pooledCov * pooledCov);
  double pooledcov3trace = trace(pooledCov * pooledCov * pooledCov);
  double a2 = pow(ntot, 2.0) *
               pow(p * (ntot - 1.0) * (ntot + 2.0), -1.0) *
    (pooledcov2trace - pow(ntot, -1.0) *
    pow(pooledcovtrace, 2.0));

  double f = - 4.0 / ntot;
  double c = - (2.0 * pow(ntot, 2.0) + 3.0 * ntot - 6.0) * pow(ntot * (pow(ntot, 2.0) + ntot + 2.0), -1.0);
  double d =  (2.0 * (5.0 * ntot + 6.0)) * pow(ntot * (pow(ntot, 2.0) + ntot + 2.0), -1.0);
  double e = - (5.0 * ntot + 6.0) * pow(pow(ntot, 2.0) * (pow(ntot, 2.0) + ntot + 2.0), -1.0);

  double tau = (pow(ntot, 5.0) * (pow(ntot, 2.0) + ntot + 2.0)) *
    pow((ntot + 1.0) * (ntot + 2.0) * (ntot + 4) * (ntot + 6.0) * (ntot - 1.0) *
    (ntot - 2.0) * (ntot - 3.0), -1.0);

  double a4star = tau * pow(p, -1.0) *
    (pooledcov4trace +
    f * pooledcov3trace * pooledcovtrace +
    c * pow(pooledcov2trace, 2.0) +
    d * pooledcov2trace * pow(pooledcovtrace, 2.0) +
    e * pow(pooledcovtrace, 4.0));

  double delta2 = 4.0 * ((2.0 * a4star * ninv) * pow(pow(a2, 2.0) * p, -1.0) + ninv2);

  double stat = 0;
  for(int i = 0; i < len; ++i){
    stat += pow(a2i[i] * pow(a2, -1.0) - 1.0, 2.0) * pow(delta2, -1.0);
  }

  return stat;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Chaipitak2013Stat(List x) {
  int len = x.length();
  arma::vec a2i(len);
  arma::mat pmat = x[1];
  double p = pmat.n_cols;
  double ntot = 0;
  arma::mat Apool(p, p);
  Apool.fill(0);
  double ninv = 0;
  double ninv2 = 0;

  for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    double ns = mats.n_rows;
    double ps = mats.n_cols;
    arma::mat covar = cov(mats);

    double cov2trace = trace(covar * covar);
    double covtrace = trace(covar);
    
    
    a2i[i] = pow(ns - 1.0, 2.0) /
      ps * (ns - 2.0) * (ns + 1.0) *
        (cov2trace - (1.0 / (ns - 1.0))) * pow(covtrace, 2.0);

    ntot += ns - 1.0;
    Apool += covar * (ns - 1.0);
    ninv += pow(ns - 1.0, -1.0);
    ninv2 += pow(ns - 1.0, -2.0);
  }

  arma::mat pooledCov = Apool / ntot;
  double pooledcov2trace = trace(pooledCov * pooledCov);
  double pooledcovtrace = trace(pooledCov);
  double pooledcov4trace = trace(pooledCov * pooledCov * pooledCov * pooledCov);
  double pooledcov3trace = trace(pooledCov * pooledCov * pooledCov);
  double a2 = pow(ntot, 2.0) *
    pow(p * (ntot - 1.0) * (ntot + 2.0), -1.0) *
    (pooledcov2trace - pow(ntot, -1.0) *
    pow(pooledcovtrace, 2.0));

  double f = - 4.0 * pow(ntot, -1.0);
  double c = - (2.0 * pow(ntot, 2.0) + 3.0 * ntot - 6.0) * pow(ntot * (pow(ntot, 2.0) + ntot + 2.0), -1.0);
  double d =  (2.0 * (5.0 * ntot + 6.0)) * pow(ntot * (pow(ntot, 2.0) + ntot + 2.0), -1.0);
  double e = - (5.0 * ntot + 6.0) * pow(pow(ntot, 2.0) * (pow(ntot, 2.0) + ntot + 2.0), -1.0);

  double tau = (pow(ntot, 5.0) * (pow(ntot, 2.0) + ntot + 2.0)) *
    pow((ntot + 1.0) * (ntot + 2.0) * (ntot + 4) * (ntot + 6.0) * (ntot - 1.0) *
    (ntot - 2.0) * (ntot - 3.0), -1.0);

  double a4star = tau * pow(p, -1.0) *
    (pooledcov4trace +
    f * pooledcov3trace * pooledcovtrace +
    c * pow(pooledcov2trace, 2.0) +
    d * pooledcov2trace * pow(pooledcovtrace, 2.0) +
    e * pow(pooledcovtrace, 4.0));

  double delta2 = 4.0 * ((2.0 * a4star * ninv) * pow(pow(a2, 2.0) * p, -1.0) + ninv2);

  double stat = 0;
  for(int i = 0; i < len; ++i){
    for(int j = i + 1; j < len; ++j){
    stat += pow(a2i[i] * pow(a2i[j], -1.0) - 1.0, 2.0) * pow(delta2, -1.0);
    }
  }

  return stat;
}

