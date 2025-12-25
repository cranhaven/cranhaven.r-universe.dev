//
//  tmvrnormGibbs.h
//  
//
//  Created by kuojung on 2020/3/11.
//
#include <RcppArmadillo.h>
//#include <RcppDist.h>
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]

using namespace std;

using namespace Rcpp;
using namespace arma;

#ifndef tmvrnormGibbs_KJLEE_h
#define tmvrnormGibbs_KJLEE_h

mat rtmvnorm_gibbs_KJLEE(int n, vec mean, mat Sigma, vec lower, vec upper, int burn_in, vec start_value, int thinning);

#endif /* tmvrnormGibbs_h */
