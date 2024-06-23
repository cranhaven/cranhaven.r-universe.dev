#include <RcppArmadillo.h>
#include <Rcpp.h>

// [[Rcpp::depends(RcppArmadillo)]]

// using namespace arma;
using namespace Rcpp;

typedef arma::mat (* cor_matMH)(const double &, const int &);
typedef arma::vec (* linkfunMH)(arma::vec &);
typedef arma::vec (* linkinvMH)(arma::vec &);
typedef arma::vec (* mu_etaMH)(arma::vec &);
typedef double (* quasiMH)(arma::vec &, arma::vec &);


arma::vec linkfun_identMH(arma::vec &mu) {return mu;}
arma::vec linkinv_identMH(arma::vec &eta) {return eta;}
arma::vec mu_eta_identMH(arma::vec &eta) {return arma::ones(eta.n_rows);}

arma::vec linkfun_logitMH(arma::vec &mu) {
  return log(mu/(1 - mu));
}


double quasi_logitMH(arma::vec &y, arma::vec &mu) {
  return sum(y % log(mu / (1 - mu)) + log(1 - mu));
}
double quasi_gaussMH(arma::vec &y, arma::vec &mu) {
  return sum((pow((y - mu), 2) / -2));
}

arma::vec linkinv_logitMH(arma::vec &eta) {
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


arma::vec mu_eta_logitMH(arma::vec &eta) {
  double thres = - log(DBL_EPSILON);
  arma::uvec con = (abs(eta) > thres);
  arma::vec mu_eta = arma::zeros(eta.n_elem, 1);
  for (size_t i=0; i < eta.n_elem; ++i) {
    mu_eta[i] = (abs(eta[i]) > thres)? DBL_EPSILON: exp(eta[i])/pow(1 + exp(eta[i]), 2);
  }
  return mu_eta;
}

arma::mat cor_indepMH(const double &, const int &m) {
  return arma::eye(m, m);
}

arma::mat cor_ar1MH(const double &rho, const int &m) {
  arma::mat ans(m, m);
  for (int i = 0; i < m; i++)
    for (int j = 0; j < m; j++)
      ans(i,j) = (i == j) ? 1.0 : pow(rho, abs(i - j));
  return ans;
}


arma::mat cor_exchMH(const double &rho, const int &m) {
  arma::mat ans(m,m);
  for (int i = 0; i < m; i++)
    for (int j = 0; j < m; j++)
      ans(i,j) = (i == j) ? 1.0 : rho;
  return ans;
}

arma::umat id2tabMH(arma::uvec &id) {
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



//' @title Get the matrices M and H for the clustered data for the GEE case
//' @description Get the matrices M and H to approximate the true covariance
//'   matrix of the GEE case
//' @details
//' getMH uses the current samples to obtain the covariance matrix.
//' @param y A matrix containing current response variable
//' @param X A data frame containing the covariate for the current samples
//' @param id  The id for each subject in the X
//' @param beta The paramters that we estimate when we use the current samples
//' @param rho A numeric number indicating the estimate of correlation
//'   coefficient
//' @param linkv A specification for the model link function.
//' @param corstrv A character string specifying the correlation structure. The
//'   following are permitted: "independence", "exchangeable" and "ar1".
//' @return a list contains several components
//' \item{sandwich}{the sandwich information matrix for covariance}
//' \item{M}{the matrix for calculating the sandwich information matrix for covariance}
//' \item{H}{the matrix for calculating the sandwich information matrix for covariance}
// [[Rcpp::export]]
List getMH(arma::vec &y, arma::mat &X, arma::uvec &id,
           arma::colvec &beta, double &rho, int &linkv, int &corstrv) {


  linkinvMH Linkinv = 0;
  mu_etaMH Mu_eta = 0;

  enum links {L_0, IDENT, LOGIT};

  enum correlations {C_0, INDEPENDENCE, EXCHANGEABLE, AR1};

  switch(linkv) {
  case IDENT:
    Linkinv = linkinv_identMH;
    Mu_eta  = mu_eta_identMH;
    break;
  case LOGIT:
    Linkinv = linkinv_logitMH;
    Mu_eta  = mu_eta_logitMH;
    break;
  }

  cor_matMH Cor_mat = 0;

  switch(corstrv) {
  case INDEPENDENCE:
    Cor_mat = cor_indepMH;
    break;
  case EXCHANGEABLE:
    Cor_mat = cor_exchMH;
    break;
  case AR1:
    Cor_mat = cor_ar1MH;
    break;
  }

  arma::umat id_tab = id2tabMH(id);

  int p = beta.n_rows;
  arma::vec eta = X * beta;
  arma::vec mu = Linkinv(eta), A = sqrt(Mu_eta(eta));
  arma::vec error = y - mu;
  arma::mat D = X.each_col() % A;
  arma::vec E = (1 / A) % error;
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

  return List::create(Named("sandwich") = wrap(sandwich),
                      Named("M")        = wrap(M),
                      Named("H")        = wrap(H));
}
