// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

using namespace arma;
using namespace Rcpp;
using namespace std;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// simple example of creating two arma::matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//


//' Check Spearman correlations between interactions in X and treatment
//' 
//' @param y A arma::vector of outcomes.
//' @param X A arma::matrix of spline bases.
//' @param alpha.schedule The prior on lambda
//' @export

//[[Rcpp::export]]

List setupGCV(arma::vec y, arma::mat X, arma::vec alphas) {
  int n = X.n_rows;
  int p = X.n_cols;
  arma::mat XpX = X.t()*X;
  arma::mat Xpy = X.t()*y;
  arma::vec beta = arma::zeros(p);
  arma::vec GCV = arma::zeros(alphas.n_rows);
  GCV(0) = 1e10;
  int count = 1;
  double sigma = stddev(y);
  arma::vec Etausqinv(p);
  Etausqinv.fill(alphas(0)*stddev(y));
  return List::create(Named("n") = n, _["p"] = p, _["alphas"] = alphas, _["XpX"] = XpX, _["Xpy"] = Xpy, _["beta"] = beta, _["GCV"] = GCV, _["Etausqinv"] = Etausqinv, _["sigma"] = sigma, _["count"] = count, _["X"] = X, _["y"] = y);
}

//[[Rcpp::export]]
List update(List L, double tol) {
  //Etausqinv(0) = 0; //zero out the GCV from previous
  int n = L["n"];
  int p = L["p"];
  int count = L["count"];
  arma::vec beta = L["beta"];
  arma::vec Ewtsqtausq = arma::ones(p);
  arma::vec Etausqinv = L["Etausqinv"];
  arma::mat XpX = L["XpX"];
  arma::mat Xpy = L["Xpy"];
  arma::mat XpXsolve = XpX;
  arma::mat X = L["X"];
  arma::mat y = L["y"];
  arma::vec alphas = L["alphas"];
  arma::vec fits = arma::zeros(n);
  double lambda_0 = 1;
  double sdy = L["sigma"];
  double sigma = sdy;
  double sigma_sq = sigma*sigma;
  double conv = 1;
  double edf = 0;
  double GCV = 0;
  double a = 0;
  double w = 0;
  
  //Trying to just reinitialize?
  //Etausqinv = ones(p);
  //Ewtsqtausq = ones(p);
  for(int i = 0; i < p; ++i){
    if(Etausqinv(i)< 10) Etausqinv(i)=10;
    if(Etausqinv(i)> 100) Etausqinv(i)=100;
    //if(Ewtsqtausq(i)< 1) Ewtsqtausq(i)=1;
    //if(Ewtsqtausq(i)> 1000) Ewtsqtausq(i)=1000;
  }
  
  //arma::vec beta = arma::zeros(p);
  arma::vec beta_last = beta;
  
  int iters = 500; //I like to put constants like this as their own variables, so we can change the value later easily
  for (int i = 0; i < iters; ++i) {
    
    if (conv/sdy > tol) {
      for (int j = 2; j < p; ++j) {
        XpXsolve(j, j) = XpX(j, j) + Etausqinv(j) + 1e-6;    
      }
      beta_last = beta;
      // Only select subarma::matrix after the first iteration
      
      arma::uvec update_ind;
      
      if(i == 0) {
        update_ind = find(abs(beta) > -1);  
      }
      else {
        update_ind = find(abs(beta) > tol*sdy); 
      }
      
      beta(update_ind) = solve(XpXsolve.submat(update_ind,update_ind),Xpy.rows(update_ind));
      fits = X*beta;
      for (int j= 1; j < p; ++j) {
        //fitting with gamma=2, things are tractable.
        a = (abs(beta(j))/(lambda_0*sigma)+pow(lambda_0,-2))*lambda_0*lambda_0/2;
        w = 4*pow(a+1,1.5)/pow(3.14159,.5)*(1/(2*(a+1)*(a+1)));
        Ewtsqtausq(j)  = abs(beta(j))/(lambda_0*sigma)*w+pow(lambda_0,-2);
        Etausqinv(j)  = lambda_0/abs(beta(j))*sigma*w;
      }
      Etausqinv(0) = 0;
      Ewtsqtausq(0) = 0;
      
      Etausqinv(1) = 0;
      Ewtsqtausq(1) = 0;
      
      lambda_0 = sqrt((alphas(count) - 1)/(sum(Ewtsqtausq)/2 + 1));
      sigma_sq = (sum((y - fits) % (y - fits)) + sum(beta % beta % Etausqinv/2))/(n/2 + p/2 + 1);
      sigma = sqrt(sigma_sq);
      
      conv = max(abs(beta - beta_last));
    }
    arma::uvec update_ind = find(abs(beta) > tol*sdy );
    edf = trace(XpX.submat(update_ind,update_ind)*pinv(XpXsolve.submat(update_ind,update_ind)));
    // double den = (n-log(n)/2*edf);
    double den = (n-edf);
    GCV = sum((y-fits)%(y-fits))/(den*den);
  }
  //Etausqinv(0) = GCV;
  //Etausqinv = arma::vectorise(Etausqinv);
  arma::vec v = L["GCV"];
  v(count) = GCV;
  ++count;
  return List::create(Named("n") = n, _["p"] = p, _["alphas"] = alphas, _["XpX"] = XpX, _["Xpy"] = Xpy, _["beta"] = beta, _["GCV"] = v, _["Etausqinv"] = Etausqinv, _["sigma"] = sigma, _["count"] = count, _["X"] = X, _["y"] = y);
}


//[[Rcpp::export]]
List GCV(arma::vec y, arma::mat X, arma::vec alphas, double tol) {
  List L = setupGCV(y, X, alphas);
  vector<arma::vec> betas;
  vector<arma::vec> Etausqinvs;
  betas.push_back(L["beta"]);
  Etausqinvs.push_back(L["Etausqinv"]);
  for (unsigned int i = 1; i < alphas.n_rows; ++i) {
    L = update(L, tol);
    betas.push_back(L["beta"]);
    Etausqinvs.push_back(L["Etausqinv"]);
    if (i > 3) {
      arma::vec v = L["GCV"];
      if (v(i) > v(i - 1) && v(i - 1) > v(i - 2) && v(i - 2) > v(i - 3)) {
        return List::create(Named("beta") = betas[i - 3], _["Etausqinv"] = Etausqinvs[i - 3], _["GCV"] = v(i - 3));
      }
    }
  }
  
  arma::vec w = L["GCV"];
  return List::create(Named("beta") = betas[alphas.n_rows - 2], _["Etausqinv"] = Etausqinvs[alphas.n_rows - 2], _["GCV"] = w(alphas.n_rows - 2));
}
