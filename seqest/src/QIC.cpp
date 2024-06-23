#include <RcppArmadillo.h>
#include <Rcpp.h>

// [[Rcpp::depends(RcppArmadillo)]]

// using namespace arma;
using namespace Rcpp;

typedef arma::mat (* cor_matQIC)(const double &, const int &);
typedef arma::vec (* linkfunQIC)(arma::vec &);
typedef arma::vec (* linkinvQIC)(arma::vec &);
typedef arma::vec (* mu_etaQIC)(arma::vec &);
typedef double (* quasiQIC)(arma::vec &, arma::vec &);


arma::vec linkfun_identQIC(arma::vec &mu) {return mu;}
arma::vec linkinv_identQIC(arma::vec &eta) {return eta;}
arma::vec mu_eta_identQIC(arma::vec &eta) {return arma::ones(eta.n_rows);}

arma::vec linkfun_logitQIC(arma::vec &mu) {
  return log(mu/(1 - mu));
}


double quasi_logitQIC(arma::vec &y, arma::vec &mu) {
  return sum(y % log(mu / (1 - mu)) + log(1 - mu));
}
double quasi_gaussQIC(arma::vec &y, arma::vec &mu) {
  return sum((pow((y - mu), 2) / -2));
}

arma::vec linkinv_logitQIC(arma::vec &eta) {
  double thres = - log(DBL_EPSILON);  //around 36.0437 on my computer
  // if eta > 36, then exp(eta) is too huge to continue
  arma::vec z1 = arma::zeros(eta.n_elem, 1);
  for (size_t i=0; i < eta.n_elem; i++) {
    z1[i] = (eta[i] < thres)? eta[i]: thres;
  }

  arma::vec z2 = arma::zeros(eta.n_elem, 1);
  for (size_t i=0; i < eta.n_elem; i++) {
    z2[i] = (z1[i] > -thres)? z1[i]: -thres;
  }
  return exp(z2)/(1 + exp(z2));
}


arma::vec mu_eta_logitQIC(arma::vec &eta) {
  double thres = - log(DBL_EPSILON);
  arma::uvec con = (abs(eta) > thres);
  arma::vec mu_eta = arma::zeros(eta.n_elem, 1);
  for (size_t i=0; i < eta.n_elem; ++i) {
    mu_eta[i] = (abs(eta[i]) > thres)? DBL_EPSILON: exp(eta[i])/pow(1 + exp(eta[i]), 2);
  }
  return mu_eta;
}

arma::mat cor_indepQIC(const double &, const int &m) {
  return arma::eye(m, m);
}

arma::mat cor_ar1QIC(const double &rho, const int &m) {
  arma::mat ans(m, m);
  for (int i = 0; i < m; i++)
    for (int j = 0; j < m; j++)
      ans(i,j) = (i == j) ? 1.0 : pow(rho, abs(i - j));
  return ans;
}


arma::mat cor_exchQIC(const double &rho, const int &m) {
  arma::mat ans(m,m);
  for (int i = 0; i < m; i++)
    for (int j = 0; j < m; j++)
      ans(i,j) = (i == j) ? 1.0 : rho;
  return ans;
}

arma::umat id2tabQIC(arma::uvec &id) {
  arma::uvec clusznew = find(abs(diff(id)) > 0);
  arma::uvec endpoints(clusznew.n_rows+1);
  arma::uvec clusz(clusznew.n_rows+1);
  endpoints.rows(0, clusznew.n_rows - 1) = clusznew;
  endpoints(clusznew.n_rows) = id.n_rows-1;
  clusz.rows(1, clusznew.n_rows) = diff(endpoints);
  clusz(0) = endpoints(0) + 1;
  arma::uvec startpoints = endpoints - clusz + 1;
  arma:: umat tab(clusz.n_rows, 4);
  tab.col(0) = id.rows(startpoints);
  tab.col(1) = clusz;
  tab.col(2) = startpoints;
  tab.col(3) = endpoints;
  return(tab);
}



//' @title Calculate quasi-likelihood under the independence model criterion
//'   (QIC) for Generalized Estimating Equations.
//' @description Select the optimal model according to the QIC criterion
//' @details
//' QIC calculates the value of the quasi-likelihood under the independence
//' model criterion for Generalized Estimating Equations. The QIC criterion is
//' actually a generalization of the AIC criterion in the statistical inference
//' of parameters in the longitudinal data analysis framework.
//'
//' @param y A matrix containing current response variable
//' @param X A data frame containing the covariate for the current samples
//' @param id  The id for each subject in the X
//' @param beta The paramters that we estimate when we use the current samples
//' @param nonZeroSet The set of the index of the non zero coefficient
//' @param rho A numeric number indicating the estimate of correlation coefficient
//' @param linkv A specification for the model link function.
//' @param corstrv A character string specifying the correlation structure. The
//'   following are permitted: "independence", "exchangeable" and "ar1".
//' @return a value indicating how well the model fits
// [[Rcpp::export]]
double QIC(arma::vec &y, arma::mat &X, arma::uvec &id, arma::vec &beta, arma::uvec &nonZeroSet, double &rho, int &linkv, int &corstrv) {

  linkinvQIC Linkinv = 0;
  mu_etaQIC Mu_eta = 0;
  quasiQIC Quais = 0;

  enum links {L_0, IDENT, LOGIT};

  enum correlations {C_0, INDEPENDENCE, EXCHANGEABLE, AR1};


  switch(linkv) {
  case IDENT:
    Linkinv = linkinv_identQIC;
    Mu_eta  = mu_eta_identQIC;
    Quais   = quasi_gaussQIC;
    break;
  case LOGIT:
    Linkinv = linkinv_logitQIC;
    Mu_eta  = mu_eta_logitQIC;
    Quais   = quasi_logitQIC;
    break;
  }

  cor_matQIC Cor_mat = 0;
  switch(corstrv) {
  case INDEPENDENCE:
    Cor_mat = cor_indepQIC;
    break;
  case EXCHANGEABLE:
    Cor_mat = cor_exchQIC;
    break;
  case AR1:
    Cor_mat = cor_ar1QIC;
    break;
  }
  arma::umat id_tab = id2tabQIC(id);
  int p = beta.n_rows;
  arma::vec eta = X * beta;
  nonZeroSet = nonZeroSet - 1;
  arma::vec mu = Linkinv(eta), A = sqrt(Mu_eta(eta));
  arma::vec error = y - mu;
  arma::vec E = (1 / A) % error;
  arma::mat D = X.each_col() % A;
  // mat R_SK(p, p, fill::zeros); // suggested by Balan and Schiopu-Kratina 2005
  arma::mat M(p, p, arma::fill::zeros);
  arma::mat H(p, p, arma::fill::zeros);

  for (size_t i = 0; i < id_tab.n_rows; i++) {
    arma::uword start = id_tab(i, 2), end = id_tab(i, 3);
    arma::uword m = id_tab(i, 1);
    arma::mat R_inv = inv_sympd(Cor_mat(rho, m));
    arma::mat Di = D.rows(start, end);
    arma::vec Ei = E.rows(start, end);
    arma::mat R0 = Ei * Ei.t();
    // R_SK += R0;
    M += Di.t() * R_inv * R0 * R_inv * Di;
    H += Di.t() * R_inv * Di;
    // R_SK = R_inv;
  }

  arma::mat sandwich = H * inv_sympd(M) * H;
  arma::mat omegaI = inv_sympd(M.submat(nonZeroSet, nonZeroSet));
  arma::mat Vr = sandwich.submat(nonZeroSet, nonZeroSet);
  double quasiV = Quais(y, mu);
  double traceR = sum(diagvec(omegaI * Vr));
  double QIC = 2 * (traceR - quasiV);
  return QIC;
}
