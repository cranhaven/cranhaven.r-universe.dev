#include <RcppArmadillo.h>
#include <Rcpp.h>

// [[Rcpp::depends(RcppArmadillo)]]

// using namespace arma;
using namespace Rcpp;


typedef arma::mat (* cor_matD)(const double &, const int &);
typedef arma::vec (* linkfunD)(arma::vec &);
typedef arma::vec (* linkinvD)(arma::vec &);
typedef arma::vec (* mu_etaD)(arma::vec &);
typedef double (* quasiD)(arma::vec &, arma::vec &);


arma::vec linkfun_identD(arma::vec &mu) {return mu;}
arma::vec linkinv_identD(arma::vec &eta) {return eta;}
arma::vec mu_eta_identD(arma::vec &eta) {return arma::ones(eta.n_rows);}

arma::vec linkfun_logitD(arma::vec &mu) {
  return log(mu/(1 - mu));
}


double quasi_logitD(arma::vec &y, arma::vec &mu) {
  return sum(y % log(mu / (1 - mu)) + log(1 - mu));
}
double quasi_gaussD(arma::vec &y, arma::vec &mu) {
  return sum((pow((y - mu), 2) / -2));
}

arma::vec linkinv_logitD(arma::vec &eta) {
  double thres = - log(DBL_EPSILON);  //around 36.0437 on my computer
  // if eta > 36, then exp(eta) is too huge to continue
  arma::vec z1 = arma::zeros(eta.n_elem, 1);
  for (size_t  i=0; i < eta.n_elem; i++) {
    z1[i] = (eta[i] < thres)? eta[i]: thres;
  }

  arma::vec z2 = arma::zeros(eta.n_elem, 1);
  for (size_t  i=0; i < eta.n_elem; i++) {
    z2[i] = (z1[i] > -thres)? z1[i]: -thres;
  }
  return exp(z2)/(1 + exp(z2));
}


arma::vec mu_eta_logitD(arma::vec &eta) {
  double thres = - log(DBL_EPSILON);
  arma::uvec con = (abs(eta) > thres);
  arma::vec mu_eta = arma::zeros(eta.n_elem, 1);
  for (size_t  i=0; i < eta.n_elem; ++i) {
    mu_eta[i] = (abs(eta[i]) > thres)? DBL_EPSILON: exp(eta[i])/pow(1 + exp(eta[i]), 2);
  }
  return mu_eta;
}

arma::mat cor_indepD(const double &, const int &m) {
  return arma::eye(m, m);
}

arma::mat cor_ar1D(const double &rho, const int &m) {
  arma::mat ans(m, m);
  for (int i = 0; i < m; i++)
    for (int j = 0; j < m; j++)
      ans(i,j) = (i == j) ? 1.0 : pow(rho, abs(i - j));
  return ans;
}


arma::mat cor_exchD(const double &rho, const int &m) {
  arma::mat ans(m,m);
  for (int i = 0; i < m; i++)
    for (int j = 0; j < m; j++)
      ans(i,j) = (i == j) ? 1.0 : rho;
  return ans;
}

arma::umat id2tabD(arma::uvec &id) {
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


//' @title Get the most informative subjects for the clustered data
//' @description Get the most informative subjects for the highly stratified
//'   response data by the D-optimility.
//' @details
//' D_optimal uses the D-optimality criterion from the experimental design to
//' choose the most informative subjects for the highly stratified response
//' data.
//' @param X A data frame contains all the random samples which we will choose
//'   subject from.
//' @param id  The id for each subject in the X
//' @param beta The paramters that we estimate under the current samples
//' @param nonZeroSet The set of the index of the non zero coefficient
//' @param M A numeric matrix calculated by the \code{\link{getMH}} function
//' @param rho A numeric number indicating the estimate of correlation
//'   coefficient
//' @param linkv A specification for the model link function.
//' @param corstrv A character string specifying the correlation structure. The
//'   following are permitted: "independence", "exchangeable" and "ar1".
//' @return a index of the most informative subject
// [[Rcpp::export]]
int D_optimal(arma::mat &X, arma::uvec &id, arma::vec &beta, arma::uvec &nonZeroSet, arma::mat &M, double &rho, int &linkv, int &corstrv) {




  enum links {L_0, IDENT, LOGIT};

  enum correlations {C_0, INDEPENDENCE, EXCHANGEABLE, AR1};

  linkinvD Linkinv = 0;
  mu_etaD Mu_eta = 0;
  switch(linkv) {
  case IDENT:
    Linkinv = linkinv_identD;
    Mu_eta  = mu_eta_identD;
    break;
  case LOGIT:
    Linkinv = linkinv_logitD;
    Mu_eta  = mu_eta_logitD;
    break;
  }

  cor_matD Cor_mat = 0;
  switch(corstrv) {
  case INDEPENDENCE:
    Cor_mat = cor_indepD;
    break;
  case EXCHANGEABLE:
    Cor_mat = cor_exchD;
    break;
  case AR1:
    Cor_mat = cor_ar1D;
    break;
  }

  arma::umat id_tab = id2tabD(id);

  nonZeroSet = nonZeroSet - 1;
  arma::vec beta_sub = beta.rows(nonZeroSet);
  int p = beta_sub.n_rows;
  arma::mat X_sub = X.cols(nonZeroSet);
  arma::vec eta = X_sub * beta_sub;
  arma::vec mu = Linkinv(eta), A = sqrt(Mu_eta(eta));
  arma::mat D = X_sub.each_col() % A;
  // mat R_SK(p, p, fill::zeros); // suggested by Balan and Schiopu-Kratina 2005
  arma::mat tmpM(p, p, arma::fill::zeros), M_sub = M.submat(nonZeroSet, nonZeroSet);
  // mat H(p, p, fill::zeros);
  // vec det_val(id_tab.n_rows, fill::zeros);
  double det_val = DBL_MIN;
  int ans = 0;

  for (size_t  i = 0; i < id_tab.n_rows; i++) {
    arma::uword start = id_tab(i, 2), end = id_tab(i, 3);
    arma::uword m = id_tab(i, 1);
    arma::mat R_inv = inv_sympd(Cor_mat(rho, m));
    arma::mat Di = D.rows(start, end);
    tmpM = M_sub + Di.t() * R_inv * Di;  // the Second term accually is H
    // or say assume R0 = R
    if (det(tmpM) > det_val) {
      det_val = det(tmpM);
      ans = id_tab(i, 0);
    }
  }
  return ans;
}
