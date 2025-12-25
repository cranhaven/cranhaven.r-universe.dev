#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double Ishii2016Stat(List x) {
  int len = x.length();
  arma::mat pmat = x[0];
  List samplecov(len);
  double p = pmat.n_cols;
  double ntot = 0;
  arma::mat Apool(p, p);
  Apool.fill(0.0);
  arma::vec ns(len);
  List Ai(len);
  arma::vec lambda(len);
  List eigendual(len);
  arma::vec ki(len);

 for(int i = 0; i < len; ++i){
    arma::mat mats = x[i];
    double nsi = mats.n_rows;
    ns[i] = nsi;
    double ps = mats.n_cols;
    arma::mat diag(ns[i], ns[i]);
    diag.fill(0);
    diag.eye(ns[i], ns[i]);
    arma::mat J(ns[i], ns[i]);
    J.fill(1);
    arma::vec j(ns[i]);
    j.fill(1);
    arma::mat A = mats.t() * (diag - J / ns[i]) * mats;
    Ai[i] = A;
    arma::mat covar = A / (ns[i] - 1.0);
    samplecov[i] = covar;
    p = ps;

    arma::vec lamb;
    arma::mat eigdual;
    eig_sym(lamb, eigdual, covar);
    arma::vec eig = eigdual.col(0);

    double kii = trace(covar) - lamb(0);

    ki[i] = kii;
    lambda[i] = lamb(0);
    eigendual[i] = eig;

    ntot += ns[i] - 1.0;
    Apool += A;

  }

  arma::mat overallcov = Apool / ntot;
  arma::vec overallLambda;
  arma::mat overalleigendual;
  eig_sym(overallLambda, overalleigendual, overallcov);
  double k = trace(overallcov) - overallLambda[0];

  double stat = 0;
  for(int i = 0; i < len; ++i){
    double lambdai = lambda[i];
    arma::vec eigen = eigendual[i];
    double kiii = ki[i];
    double lambdatil = std::max(lambdai * pow(overallLambda[0], -1.0), overallLambda[0] * pow(lambdai, -1.0));
    double eigentil = std::max(arma::as_scalar(eigen.t() * overalleigendual.col(0)), arma::as_scalar(overalleigendual.col(0).t() * eigen));
    double ktil = std::max(kiii * pow(k, -1.0), k * pow(kiii, -1.0));
    stat += lambdatil * eigentil * ktil;
  }

  return stat;
}

