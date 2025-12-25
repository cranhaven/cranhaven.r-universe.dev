#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Srivastava2014Stat(List x) {
  int len = x.length();
  arma::vec a2i(len);
  arma::mat pmat = x[1];
  List samplecov(len);
  double p = pmat.n_cols;
  double ntot = 0;
  double ninv = 0;
  arma::vec ns(len);
  List Ai(len);
  List Di(len);

 for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    ns[i] = mats.n_rows;
    double ps = mats.n_cols;
    arma::mat covar = cov(mats);
    samplecov[i] = covar;

    Ai[i] = covar * (ns[i] - 1.0);

    arma::rowvec scaled = mean(mats);
    arma::mat scaleddf(ns[i], ps);

    for(int k = 0; k < ps; ++k){
      scaleddf.col(k) = mats.col(k) - scaled(k);
    }

    arma::mat d = scaleddf * scaleddf.t();
    arma::mat D(ns[i], ns[i]);
    D.fill(0);

    for(int z = 0; z < ns[i]; ++z){
      D(z, z) = d(z, z);
    }

    Di[i] = D;
    ntot += ns[i] - 1.0;

    ninv += pow(ns[i] - 1.0, -1.0);
  }

 for(int i = 0; i < len; ++i){
   arma::mat Ais = Ai[i];
   arma::mat D = Di[i];

   a2i[i] = pow(p * ns[i] * (ns[i] - 1.0) * (ns[i] - 2.0) * (ns[i] - 3.0), -1.0) *
     ((ns[i] - 2.0) * (ns[i] - 1.0) * trace(Ais * Ais) -
     (ntot + len) * ntot * trace(D * D) +
     trace(Ais * Ais));
 }


 double a2num = 0;
 for(int i = 0; i < len; ++i){
   a2num += ns[i] * a2i[i];
 }

 double a2 = a2num * pow(ntot, -1);

 double theta = 2.0 * a2 * ninv;

  double stat = 0;
  for(int i = 0; i < len; ++i){
    arma::mat sampcovi = samplecov[i];
    for(int j = i + 1; j < len; ++j){
      arma::mat sampcovj = samplecov[j];
      double samplecovijtrace = trace(sampcovi * sampcovj);
    stat += pow(a2i[i] + a2i[j] - (2.0 * pow(p, -1.0)) * samplecovijtrace, 2.0) *
      pow(theta, -2.0);
    }
  }

  return stat;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Srivastava2014poolStat(List x) {
  int len = x.length();
  arma::vec a2i(len);
  arma::mat pmat = x[1];
  List samplecov(len);
  double p = pmat.n_cols;
  double ntot = 0;
  double ninv = 0;
  arma::vec ns(len);
  List Ai(len);
  List Di(len);
  arma::mat Apool(p, p);
  Apool.fill(0);

  for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    ns[i] = mats.n_rows;
    double ps = mats.n_cols;
    arma::mat covar = cov(mats);
    samplecov[i] = covar;
    arma::mat A = covar * (ns[i] - 1.0);
    Ai[i] = A;

    arma::rowvec scaled = mean(mats);
    arma::mat scaleddf(ns[i], ps);

    for(int k = 0; k < ps; ++k){
      scaleddf.col(k) = mats.col(k) - scaled(k);
    }

    arma::mat d = scaleddf * scaleddf.t();
    arma::mat D(ns[i], ns[i]);
    D.fill(0);

    for(int z = 0; z < ns[i]; ++z){
      D(z, z) = d(z, z);
    }

    Di[i] = D;
    ntot += ns[i] - 1.0;
    ninv += pow(ns[i] - 1.0, -1.0);
    Apool += A;
  }

  for(int i = 0; i < len; ++i){
    arma::mat Ais = Ai[i];
    arma::mat D = Di[i];

    a2i[i] = pow(p * ns[i] * (ns[i] - 1.0) * (ns[i] - 2.0) * (ns[i] - 3.0), -1.0) *
      ((ns[i] - 2.0) * (ns[i] - 1) * trace(Ais * Ais) -
      (ntot + len) * ntot * trace(D * D) +
      trace(Ais * Ais));
  }


  double a2num = 0;
  for(int i = 0; i < len; ++i){
    a2num += ns[i] * a2i[i];
  }

  double a2 = a2num * pow(ntot, -1.0);

  double theta = 2.0 * a2 * ninv;

  arma::mat pooledCov = Apool * pow(ntot, -1.0);


  double stat = 0;
  for(int i = 0; i < len; ++i){
    arma::mat sampcovi = samplecov[i];
    double ai = a2i[i];
    double samplecovipooledtrace = trace(sampcovi * pooledCov);
    stat += pow(pow(theta, -1.0) *  (ai + a2 - 2.0 * pow(p, -1.0) * samplecovipooledtrace), 2.0);
  }

  return stat;
}


