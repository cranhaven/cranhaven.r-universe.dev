// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
#include <queue>
#include <vector>
#include <time.h>
#include <map>

using namespace arma;
using namespace Rcpp;
using namespace std;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]


//[[Rcpp::export]]
arma::vec checkcor(arma::mat X, double thresh) {
  arma::mat cors = arma::cor(X);
  arma::vec v = ones(cors.n_cols); //include all vars initially. This is a bitmask where 1 means to include var and 0 means not to
  double threshsq = thresh*thresh;
  for (unsigned int i = 0; i < cors.n_rows; ++i) {
    if (stddev(X.col(i)) > 0) {
      for (unsigned int j = i+1; j < cors.n_cols; ++j) {
        if ( pow(cors(i, j),2) >= threshsq) {
          v(j) = 0;
        }
      }
    }
    else {
      v(i) = 0;
    }
  }
  return v; //vars marked zero are ones to not include
}


struct Comp { //this is a comparator, used for the heap (priority_queue) in the function below
public:
  bool operator()(arma::vec a, arma::vec b) {
    return a(3) > b(3);
  }
};



//[[Rcpp::export]]
List namesAndCorrs(arma::mat XSubsamp, arma::vec ySubsamp, arma::mat treatSubsamp, arma::mat XConstruct, arma::mat treatConstruct, arma::mat XConstructDerivative, arma::mat treatConstructDerivative,  long long unsigned int a) {
  //a is number of top results, i.e. top 100 or top 300
  
  priority_queue<arma::vec, std::vector<arma::vec>, Comp> pq;
  arma::vec indexCurr=arma::zeros(4);
  double cor_temp = 0;
  double sd_adjust = 0;
  
  for (double i = 0; i < treatSubsamp.n_cols; ++i) {
    for (double j = 0; j < XSubsamp.n_cols; ++j) {
      for (double k = j; k < XSubsamp.n_cols; ++k) {
      // for (double k = 0; k < XSubsamp.n_cols; ++k) {
        arma::vec inter_temp = treatSubsamp.col(i) % XSubsamp.col(j) % XSubsamp.col(k); 
        
        
        cor_temp = as_scalar(arma::cor(ySubsamp, inter_temp));
        if(cor_temp < 0) cor_temp = -cor_temp;
        if(!arma::is_finite(cor_temp)) cor_temp = 0;
        
        indexCurr(0) = i;
        indexCurr(1) = j;
        indexCurr(2) = k;
        indexCurr(3) = cor_temp;
        pq.push(indexCurr);
        if (pq.size() > a) { //this keeps the size of the heap to exactly a
          pq.pop();
        }
      }
    }
  }
  
  arma::vec iCurr = arma::zeros(4);
  std::vector<arma::vec> indexCurrs(a, iCurr);
  
  int i = a - 1;
  
  while (!pq.empty()) {
    indexCurrs[i] = pq.top();
    pq.pop();
    --i;
  }
  
  arma::mat Msubsamp;
  arma::mat MConstruct;
  arma::mat MConstructDerivative;
  
  for (unsigned int l = 0; l < indexCurrs.size(); ++l) {
    arma::vec indexCurr = indexCurrs[l];
    int i = indexCurr(0);
    int j = indexCurr(1);
    int k = indexCurr(2);
    arma::vec interSubsamp = treatSubsamp.col(i) % XSubsamp.col(j) % XSubsamp.col(k);
    sd_adjust = stddev(interSubsamp);
    interSubsamp = (interSubsamp - mean(interSubsamp)) / sd_adjust;
    
    arma::vec interConstruct = treatConstruct.col(i) % XConstruct.col(j) % XConstruct.col(k);
    interConstruct = (interConstruct-mean(interSubsamp)) / sd_adjust;
    
    arma::vec interConstructDerivative = treatConstructDerivative.col(i) % XConstructDerivative.col(j) % XConstructDerivative.col(k);
    interConstructDerivative = interConstructDerivative / sd_adjust;
    
    Msubsamp = arma::join_rows(Msubsamp, interSubsamp);
    MConstruct = arma::join_rows(MConstruct, interConstruct);
    MConstructDerivative = arma::join_rows(MConstructDerivative, interConstructDerivative);
  }
  
  return List::create(Named("cors") = indexCurrs, _["Msubsamp"] = Msubsamp, _["MConstruct"] = MConstruct, _["MConstructDerivative"] = MConstructDerivative); //returns a highest correlations, matrix M, variable names
}

std::vector<string> names(std::vector<arma::vec> indexCurrs, std::vector<string> Xnames, std::vector<string> treatNames) {
  //std::vector<arma::vec> indexCurrs = L["cors"], where L is the output of namesAndCorrs
  std::vector<string> n;
  
  for (unsigned int l = 0; l < indexCurrs.size(); ++l) {
    arma::vec indexCurr = indexCurrs[l];
    int i = indexCurr(0);
    int j = indexCurr(1);
    int k = indexCurr(2);
    std::string s = treatNames[i] + "_" + to_string(i) + " x " + Xnames[j] + "_" + to_string(j) + " x " + Xnames[k] + "_" + to_string(k);
    n.push_back(s);
  }
  
  return n;
}
