#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Schott2007Stat(List x) {
  int len = x.length();
  arma::mat pmat = x[1];
  arma::vec a2i(len);
  List samplecov(len);
  double p = pmat.n_cols;
  double ntot = 0;
  arma::mat Apool(p, p);
  Apool.fill(0);
  double ninv = 0;

 for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    double ns = mats.n_rows;
    double ps = mats.n_cols;

    arma::mat covar = cov(mats);
    samplecov[i] = covar;

    
    double cov2trace = trace(covar * covar);
    double covtrace = trace(covar);
    
    
    a2i[i] = pow(ns - 1.0, 2.0) /
      ps * (ns - 2.0) * (ns + 1.0) *
        (cov2trace - (1.0 / (ns - 1.0))) * pow(covtrace, 2.0);

    Apool += covar * (ns - 1.0);
    ntot += ns - 1.0;
    ninv += pow(ns - 1.0, -1.0);
  }

 arma::mat pooledCov = Apool * pow(ntot, -1);
 double pooledcov2trace = trace(pooledCov * pooledCov);
 double pooledcovtrace = trace(pooledCov);
 double a2 = pow(ntot, 2.0) *
   pow(p * (ntot - 1.0) * (ntot + 2.0), -1.0) *
   (pooledcov2trace - pow(ntot, -1.0) * pow(pooledcovtrace, 2.0));

  double theta = 2.0 * a2 * ninv;

  double stat = 0;
  for(int i = 0; i < len; ++i){
    arma::mat sampcovi = samplecov[i];
    double ai = a2i[i];
    for(int j = i + 1; j < len; ++j){
      double aj = a2i[j];
      arma::mat sampcovj = samplecov[j];
      double samplecovijtrace = trace(sampcovi * sampcovj);
    stat += pow(pow(theta, -1.0) *  (ai + aj - 2.0 * pow(p, -1.0) * samplecovijtrace), 2.0);
    }
  }

  return stat;
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Schott2007pooledStat(List x) {
  int len = x.length();
  arma::mat pmat = x[1];
  arma::vec a2i(len);
  List samplecov(len);
  double p = pmat.n_cols;
  double ntot = 0;
  arma::mat Apool(p, p);
  Apool.fill(0);
  double ninv = 0;

  for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    double ns = mats.n_rows;
    double ps = mats.n_cols;

    arma::mat covar = cov(mats);
    samplecov[i] = covar;

    double cov2trace = trace(covar * covar);
    double covtrace = trace(covar);
    
    
    a2i[i] = pow(ns - 1.0, 2.0) /
      ps * (ns - 2.0) * (ns + 1.0) *
        (cov2trace - (1.0 / (ns - 1.0))) * pow(covtrace, 2.0);

    Apool += covar * (ns - 1.0);
    ntot += ns - 1.0;
    ninv += pow(ns - 1.0, -1.0);
  }

  arma::mat pooledCov = Apool * pow(ntot, -1);

  double a2 = pow(ntot, 2.0) *
    pow(p * (ntot - 1.0) * (ntot + 2.0), -1.0) *
    (trace(pooledCov * pooledCov) - pow(ntot, -1.0) * pow(trace(pooledCov), 2.0));

  double theta = 2.0 * a2 * ninv;

  double stat = 0;
  for(int i = 0; i < len; ++i){
    arma::mat sampcovi = samplecov[i];
    double ai = a2i[i];

    stat += pow(pow(theta, -1.0) *  (ai + a2 - 2.0 * pow(p, -1.0) * trace(sampcovi * pooledCov)), 2.0);
  }

  return stat;
}

