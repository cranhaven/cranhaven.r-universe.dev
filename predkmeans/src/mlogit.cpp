//
//  mlogitcpp.cpp
//  
//
//  Created by Joshua Keller on 3/26/15.
//  Last Modified 11/1/17
//

#include <stdio.h>
//#include <Rcpp.h>
#include "R.h"
#include "Rmath.h"
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;


// [[Rcpp::export]]
double loglikeCpp(arma::mat X, arma::mat b, arma::mat y, int n)
{
    arma::mat eta;
    arma::vec denomMXmax;
    arma::mat denomMX;
    arma::vec denom;
    double res;
    arma::mat zerovec = arma::zeros<arma::mat>(n, 1);
    
    
    eta = X*b;
    
    // More stable version of:
    // denom = log(1+ sum(exp(eta),1));
    denomMX = join_rows(zerovec, eta);
    denomMXmax = max(denomMX, 1);
    denomMX = denomMX.each_col() - denomMXmax;
    denom = denomMXmax + log(sum(exp(denomMX), 1));
    
    y %= eta;
    res = accu(y) - accu(denom);
    return res;
}

// [[Rcpp::export]]
arma::mat gradientMultinomialCpp(arma::mat X, arma::mat b, arma::mat y, int k)
{
    arma::mat eta;
    arma::mat etaTemp;
    arma::mat prob;
    arma::mat res;
    
    // More stable version of
    // eta = exp(X*b);
    // prob = 1+ sum(eta, 1);
    // eta.each_col() /=prob;
    eta = X*b;
    prob = eta; //overwrite this later
    for (int j=0; j<(k-1); j++){
        etaTemp = exp(eta.each_col() - eta.col(j));
        prob.col(j) = exp(-eta.col(j)) + sum(etaTemp, 1);
    }
    prob = pow(prob, -1);
    prob = y - prob;
    res=X.t()*prob;
    return res;
}


// [[Rcpp::export]]
arma::mat hessianMultinomialCpp(arma::mat X, arma::mat b, arma::mat y, int p, int k)
{
    arma::mat eta;
    arma::mat etaTemp;
    arma::mat prob;

    arma::mat H(p*(k-1),p*(k-1));
    arma::mat Hn(p, p);
    arma::mat Xtemp;

    /*
    prob = exp(X*b);
    probA = 1+ sum(prob, 1);
    prob.each_col() /=probA;
    */
    eta = X*b;
    prob = eta; //overwrite this later
    for (int j=0; j<(k-1); j++){
        etaTemp = exp(eta.each_col() - eta.col(j));
        prob.col(j) = exp(-eta.col(j)) + sum(etaTemp, 1);
    }
    prob = pow(prob, -1);
    
    for (int i=0; i<(k-1); i++){
        for (int j=0; j<(k-1); j++){
            Xtemp=X;
            if(i==j){
                Xtemp.each_col() %=prob.col(i)%(1-prob.col(j));
                Hn= X.t()*Xtemp;
                //Hn = X.t()*(prob.col(i)%(1-prob.col(j))%X);
            } else {
                Xtemp.each_col() %=-prob.col(i)%prob.col(j);
                Hn=X.t()*Xtemp;
               // Hn=X.t()*(-prob.col(i)%prob.col(j)%X);
            }
            H( arma::span(i*p, (i+1)*p-1), arma::span(j*p, (j+1)*p-1) )=Hn;
        }
    }
    return H;
}
