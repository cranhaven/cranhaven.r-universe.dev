#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double SrivastavaYanagihara2010Stat(List x) {
  int len = x.length();
  arma::vec a2i(len);
  arma::mat pmat = x[1];
  List samplecov(len);
  double p = pmat.n_cols;
  double ntot = 0;
  arma::mat Apool(p, p);
  Apool.fill(0);
  arma::vec ns(len);
  arma::vec a1i(len);

 for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    double n = mats.n_rows;
    ns[i] = n;
    double ps = mats.n_cols;
    arma::mat covar = cov(mats);
    samplecov[i] = covar;

    double covartrace = trace(covar);
    double covar2trace = trace(covar * covar);
    a2i[i] = pow(n - 1.0, 2.0) *
      pow(ps * (n - 2.0) * (n + 1.0), -1.0) *
      (covar2trace - pow(n - 1.0, -1.0) * pow(covartrace, 2.0));

    a1i[i] = covartrace * pow(ps, -1.0);

    ntot += n - 1.0;
    Apool += covar * (n - 1.0);
  }

  arma::mat pooledCov = Apool / ntot;

 double pooledcov2trace = trace(pooledCov * pooledCov);
 double pooledcovtrace = trace(pooledCov);
 double a2 = pow(ntot, 2.0) *
   pow(p * (ntot - 1.0) * (ntot + 2.0), -1.0) *
   (pooledcov2trace - pow(ntot, -1.0) *
   pow(pooledcovtrace, 2.0));

  double a1 = pooledcovtrace * pow(p, -1.0);

  double a3 = pow(ntot * (pow(ntot, 2.0) + 30 * ntot + 4.0), -10) *
    (trace(Apool * Apool * Apool) * pow(p, -1.0) -
    3.0 * ntot * (ntot + 1.0) * p * a2 * a1 -
    ntot * pow(p, 2.0) * pow(a1, 3.0));

  double c0 = pow(ntot, 4.0) + 6.0 * pow(ntot, 3.0) + 21.0 * pow(ntot, 2.0) + 18.0 * ntot;
  double c1 = 4.0 * pow(ntot, 3.0) + 12.0 * pow(ntot, 2.0) + 18.0 * ntot;
  double c2 = 6.0 * pow(ntot, 2.0) + 4.0 * ntot;
  double c3 = 2.0 * pow(ntot, 3.0) + 5.0 * pow(ntot, 2.0) + 7.0 * ntot;

  double a4 = pow(c0, -1.0) *
    (pow(p, -1.0) * trace(Apool * Apool * Apool * Apool) -
    p * c1 * a1 -
    pow(p, 2.0) * c2 * pow(a1, 2.0) * a2 -
    p * c3 * pow(a2, 2.0) -
    ntot * pow(p, 3.0) * pow(a1, 4.0));

 arma::vec ksi2i(len);
 arma::vec gammai(len);
 double gammabarnum = 0;
 double gammabardem = 0;
 for(int i = 0; i < len; ++i){

 ksi2i[i] = 4.0 * pow(ns[i] - 1.0, -2.0) *
   (pow(a2, 2.0) * pow(a1, -4.0) +
   2.0 * (ns[i] - 1.0) * pow(p, -1.0) *
   (pow(a2, 3.0) * pow(a1, -6.0) -
   2.0 * a2 * a3 * pow(a1, -5.0) +
   a4 * pow(a1, -4.0)));

   gammai[i] = a2i[i] * pow(a1i[i], -2.0);

   gammabarnum += gammai[i] * pow(ksi2i[i], -1.0);
   gammabardem += pow(ksi2i[i], -1.0);
 }
double gammabar = gammabarnum / gammabardem;



  double stat = 0;
  for(int i = 0; i < len; ++i){
    stat += pow(gammai[i] - gammabar, 2.0) * pow(ksi2i[i], -1.0);
  }

  return stat;
}

