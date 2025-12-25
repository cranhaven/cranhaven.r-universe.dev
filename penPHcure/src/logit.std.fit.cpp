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
#include <math.h>

using namespace arma;
using namespace std;
using namespace Rcpp;



double logL_logit_cpp(const arma::vec& b,const arma::mat& X,const arma::vec& Y){
  vec Xb = X*b;
  vec exp_Xb = exp(Xb);
  vec logit = exp_Xb / (1 + exp_Xb);
  double logL = sum(Y%Xb - log(1+exp_Xb));
  return logL;
}



void logLgradHess_logit_cpp(double& logL,arma::vec& logLgrad,
                            arma::mat& logLhess,const arma::vec& b,
                            const arma::mat& X,const arma::vec& Y){
  vec Xb = X*b;
  vec exp_Xb = exp(Xb);
  vec logit = exp_Xb / (1 + exp_Xb);
  logL = sum(Y%Xb - log(1+exp_Xb));
  logLgrad = X.t()*(Y-logit);
  logLhess = - X.t() * diagmat( logit%(1-logit)  ) * X ;
}



// [[Rcpp::export]]
Rcpp::List fit_logit_cpp(const arma::vec& b_SV,const arma::mat& X,
                         const arma::vec& Y,const bool& warnings,
                         const arma::uword& maxiter,const double& tol){
  
  vec b,b_old,logLgrad(X.n_cols),dir;
  double logL,logL_old;
  mat logLhess(X.n_cols,X.n_cols);
  uword i,fail=0,halving=0;
  int rank_old=X.n_cols,rank;
  b=b_SV;
  
  logLgradHess_logit_cpp(logL,logLgrad,logLhess,b,X,Y);
  
  logL_old = logL;
  
  for (i=1; i<=maxiter; i++){
    if (i==1){
      fail = isnan(logL) + isinf(logL);
      if (fail>0){
        Rcpp::Rcout << "WARNING :: Fit standard logit model" <<
          " :: bad starting values." << endl;
        break;
      } 
      b_old = b;
      b = b_old + solve(-logLhess,logLgrad);
    } else {
      fail = 0;
      rank = arma::rank(-logLhess);
      fail += !logLhess.is_finite() + isnan(logL) + isinf(logL) + abs(rank_old-rank);
      if (fail>0 || logL<logL_old) {
        halving++;
        b = (b_old*halving + b)/(halving+1.0);
      } else {
        halving = 0;
        if (fail==0 && halving==0 && abs(1-(logL_old/logL))<=tol) break;
        logL_old = logL;
        b_old = b;
        b = b_old + solve(-logLhess,logLgrad);
      }
    }
    logLgradHess_logit_cpp(logL,logLgrad,logLhess,b,X,Y);
    if (i==maxiter) 
      Rcpp::Rcout << "WARNING :: Fit standard logit model" <<
        " :: NR algorithm exceeded maximum number of iterations." << endl;
  }
  
  return Rcpp::List::create(Rcpp::Named("coef") = b,
                            Rcpp::Named("logL") = logL,
                            Rcpp::Named("logLgradient") = logLgrad,
                            Rcpp::Named("logLhessian") = logLhess,
                            Rcpp::Named("iter") = i-1);
}
