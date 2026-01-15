// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
arma::vec lh_binom(int N, arma::vec x) {
  arma::vec out = lgamma(N + 1) - arma::lgamma(x + 1) - arma::lgamma(N - x + 1);
  return(out);
}

// [[Rcpp::export]]
arma::vec lh_multinom(int N, arma::mat x) {
  arma::mat lf = arma::lgamma(x + 1);
  arma::vec sumlf = sum(lf, 1);
  arma::vec out = lgamma(N + 1) - sumlf;
  return(out);
}

// [[Rcpp::export]]
arma::mat est_cov(arma::mat dataset) {
  int n = dataset.n_rows;
  arma::mat out = arma::zeros(dataset.n_cols, dataset.n_cols);
  for (int i = 0; i < n - 1; i++) {
    arma::rowvec temp = dataset.row(i) - dataset.row(i + 1);
    out += temp * temp.t();
  }
  out = out / (2.0 * (n - 1));
  return(out);
}

// [[Rcpp::export]]
List g_smry_mean(arma::mat dataset, arma::mat Sigma) {
  List dat_smry;
  int n = dataset.n_rows;
  int p = dataset.n_cols;
  arma::mat cumsum_x = arma::zeros(p, n + 1);
  arma::cube cumsum_x2 = arma::zeros(p, p, n + 1);
  for (int i = 0; i < n; i++) {
    cumsum_x.col(i + 1) = cumsum_x.col(i) + arma::trans(dataset.row(i));
    cumsum_x2.slice(i + 1) = cumsum_x2.slice(i) + arma::trans(dataset.row(i)) * dataset.row(i);
  }
  dat_smry["cumsum_x"] = cumsum_x;
  dat_smry["cumsum_x2"] = cumsum_x2;
  dat_smry["Sigma"] = Sigma;
  dat_smry["Omega"] = arma::inv_sympd(Sigma);
  return(dat_smry);
}

// [[Rcpp::export]]
List g_smry_var(arma::mat dataset, arma::vec mu) {
  List dat_smry;
  int n = dataset.n_rows;
  int p = dataset.n_cols;
  arma::mat cumsum_x = arma::zeros(p, n + 1);
  arma::cube cumsum_x2 = arma::zeros(p, p, n + 1);
  for (int i = 0; i < n; i++) {
    cumsum_x.col(i + 1) = cumsum_x.col(i) + arma::trans(dataset.row(i));
    cumsum_x2.slice(i + 1) = cumsum_x2.slice(i) + arma::trans(dataset.row(i)) * dataset.row(i);
  }
  dat_smry["cumsum_x"] = cumsum_x;
  dat_smry["cumsum_x2"] = cumsum_x2;
  dat_smry["mu"] = mu;
  return(dat_smry);
}

// [[Rcpp::export]]
List g_smry_meanvar(arma::mat dataset, SEXP param_opt) {
  List dat_smry;
  int n = dataset.n_rows;
  int p = dataset.n_cols;
  arma::mat cumsum_x = arma::zeros(p, n + 1);
  arma::cube cumsum_x2 = arma::zeros(p, p, n + 1);
  for (int i = 0; i < n; i++) {
    cumsum_x.col(i + 1) = cumsum_x.col(i) + arma::trans(dataset.row(i));
    cumsum_x2.slice(i + 1) = cumsum_x2.slice(i) + arma::trans(dataset.row(i)) * dataset.row(i);
  }
  dat_smry["cumsum_x"] = cumsum_x;
  dat_smry["cumsum_x2"] = cumsum_x2;
  return(dat_smry);
}

// [[Rcpp::export]]
List g_smry_em(arma::mat dataset, List param_opt) {

  int n = dataset.n_rows;
  std::string em = param_opt["em"];
  List dat_smry;
  dat_smry["em"] = em;

  if (em == "binom") {
    int N = param_opt["N"];
    dat_smry["N"] = N;
    arma::vec x = arma::zeros(n + 1);
    x(arma::span(1, n)) = dataset.col(0);
    arma::vec cumsum_x = arma::cumsum(x);
    arma::vec lh = lh_binom(N, x);
    arma::vec cumsum_lh = arma::cumsum(lh);
    dat_smry["cumsum_x"] = cumsum_x;
    dat_smry["cumsum_lh"] = cumsum_lh;
  }
  if (em == "multinom") {
    int N = param_opt["N"];
    dat_smry["N"] = N;
    arma::mat x = arma::zeros(n + 1, dataset.n_cols);
    x(arma::span(1, n), arma::span::all) = dataset;
    arma::mat cumsum_x = arma::cumsum(x);
    arma::vec lh = lh_multinom(N, x);
    arma::vec cumsum_lh = arma::cumsum(lh);
    dat_smry["cumsum_x"] = cumsum_x;
    dat_smry["cumsum_lh"] = cumsum_lh;
  }
  if (em == "pois") {
    arma::vec x = arma::zeros(n + 1);
    x(arma::span(1, n)) = dataset.col(0);
    arma::vec cumsum_x = arma::cumsum(x);
    arma::vec lh = -arma::lgamma(x + 1);
    arma::vec cumsum_lh = arma::cumsum(lh);
    dat_smry["cumsum_x"] = cumsum_x;
    dat_smry["cumsum_lh"] = cumsum_lh;
  }
  if (em == "exp") {
    arma::vec x = arma::zeros(n + 1);
    x(arma::span(1, n)) = dataset.col(0);
    arma::vec cumsum_x = arma::cumsum(x);
    dat_smry["cumsum_x"] = cumsum_x;
  }
  if (em == "geom") {
    arma::vec x = arma::zeros(n + 1);
    x(arma::span(1, n)) = dataset.col(0);
    arma::vec cumsum_x = arma::cumsum(x);
    dat_smry["cumsum_x"] = cumsum_x;
  }

  return(dat_smry);
}
