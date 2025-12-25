// -----------------------------------------------------------------------------
// Copyright (C) 2019 University of Liège
// <penPHcure is an R package for for estimation, variable selection and
//  simulation of the semiparametric proportional-hazards (PH) cure model with
//  time-varying covariates.>
//  Authors: Alessandro Beretta & Cédric Heuchenne
//  Contact: a.beretta@uliege.be
//     
//  Licence as published by the Free Software Foundation, either version 3 of 
//  the Licence, or any later version. This program is distributed in the hope 
//  that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
//  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
//  General Public Licence for more details. You should have received a copy of 
//  the GNU General Public Licence along with this program.
//  If not, see <http://www.gnu.org/licenses/>.
// -----------------------------------------------------------------------------

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <iostream>

using namespace arma;
using namespace std;
using namespace Rcpp;

////////////////////////////////////////////////////////////////////////////////
//  Generation of EVENT TIMES                                                 //
////////////////////////////////////////////////////////////////////////////////

// Cumulative hazard function (piece-wise constant)
double cum_hazard_func(const double& t,const vec& S,const vec& lambda){
  double Lambda=0;
  if (t<=S(0)){
    Lambda=lambda(0)*t;
  }
  else if (t>S(0) && t<=S(S.n_elem-1)){
    Lambda=lambda(0)*S(0);
    for (uword j=1;j<S.n_elem;++j){
      if (S(j)>t){
        Lambda=Lambda+lambda(j)*(t-S(j-1));
        break;
      }
      Lambda=Lambda+lambda(j)*(S(j)-S(j-1));
    }
  }
  else if (t>S(S.n_elem-1)){
    Lambda=lambda(0)*S(0);
    for (uword j=1;j<S.n_elem;++j){
      Lambda=Lambda+lambda(j)*(S(j)-S(j-1));
    }
    Lambda=Lambda+lambda(S.n_elem-1)*(t-S(S.n_elem-1));
  }
  return Lambda;
}

// Cumulative density function of a piece-wise exponential distribution
double cdf_PEXP(const double& t,const vec& S,const vec& lambda){
  double cdf=1-exp(-cum_hazard_func(t,S,lambda));
  return cdf;
}

// Inverse of the CDF of a piece-wise exponential distribution
double cdf_PEXP_INV(const double& u,const vec& S,const vec& lambda){
  double x,t=0;
  x=-log(1-u);
  if (u<=cdf_PEXP(S(0),S,lambda)){
    t=x/lambda(0);
  }
  else if (u>cdf_PEXP(S(0),S,lambda) && u<=cdf_PEXP(S(S.n_elem-1),S,lambda)){
    for (uword j=1;j<S.n_elem;++j){
      if (u>cdf_PEXP(S(j-1),S,lambda) && u<=cdf_PEXP(S(j),S,lambda)){
        t = S(j-1)+(x-cum_hazard_func(S(j-1),S,lambda))/lambda(j);
        break;
      }
    }
  }
  else if (u>cdf_PEXP(S(S.n_elem-1),S,lambda)){ //  
    t = S(S.n_elem-1)+(x-cum_hazard_func(S(S.n_elem-1),S,lambda))/lambda(S.n_elem-1);
  }
  return t;
}


////////////////////////////////////////////////////////////////////////////////
//  Data generation CURE model                                                //
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
Rcpp::List datagen_cure_cpp(arma::vec beta0,arma::vec b0,double lambdaC,
                            arma::vec S,arma::uword N,arma::cube Z,arma::mat X,
                            arma::vec C,double gamma){
  
  vec lambda;
  vec T=zeros(N);
  vec delta=zeros(N);
  uvec index;
  mat temp,data;
  double W;
  vec g_inv_S = pow(S,gamma);
  vec cure=zeros(N);
  vec prob=1/(1+exp(-join_rows(ones(X.n_rows),X)*b0));
  
  for (uword i=0;i<N;++i){
    
    lambda=exp(Z.slice(i)*beta0);
    
    W = cdf_PEXP_INV(randu(),g_inv_S,lambda);
    W = pow(W,1.0/gamma);
    
    
    if (randu()>prob(i)){
      T(i)=C(i);
      delta(i)=0;
      cure(i)=1;
    }
    else if (W>C(i)){
      T(i)=C(i);
      delta(i)=0;
    }
    else{
      T(i)=W;
      delta(i)=1;
    }

    // Save data in counting process format
    if (T(i)<=S(0) || S.n_elem==1){
      temp = zeros(1,4);
      temp(0,0) = i+1;
      temp(0,2) = T(i);
      temp(0,3) = delta(i);
      temp = join_rows(temp,Z.slice(i).row(0));
      temp = join_rows(temp,X.row(i));
    } else {
      temp = zeros(S.n_elem,4);
      temp.col(0).fill(i+1);
      temp(span(1,S.n_elem-1),1) = S(span(0,S.n_elem-2));
      temp.col(2) = S;
      temp = temp.rows(find(temp.col(1)<T(i)));
      temp(temp.n_rows-1,2) = T(i);
      temp(temp.n_rows-1,3) = delta(i);
      temp = join_rows(temp,Z.slice(i).rows(0,temp.n_rows-1));
      temp = join_rows(temp,repmat(X.row(i),temp.n_rows,1));
    }
    data = join_cols(data,temp);
  }
  return Rcpp::List::create(Rcpp::Named("data") = data,
                            Rcpp::Named("perc_cure") = mean(cure),
                            Rcpp::Named("perc_cens") = 1 - mean(delta));
}
