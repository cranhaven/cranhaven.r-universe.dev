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
#include <string>

#include "logit.std.fit.h"
#include "penalties.h"

using namespace arma;
using namespace std;
using namespace Rcpp;

double logL_pen_logit_cpp(const arma::vec& b,const arma::mat& X,
                          const arma::vec& Y,const arma::uword& N,
                          const arma::vec& tun_par,const arma::vec& pen_weights,
                          const std::string& pen_type,const double& epsilon){
  uword ncov = X.n_cols;
  double logL = logL_logit_cpp(b,X,Y);
  
  if (pen_type=="SCAD"){
    for (uword j=0;j<ncov;++j){
      logL = logL - double(N) * 
        scad_penalty_pert(b(j),tun_par(0)*pen_weights(j),tun_par(1),epsilon);
    }
  }
  else if (pen_type=="LASSO"){
    for (uword j=0;j<ncov;++j){
      logL = logL - double(N) * 
        lasso_penalty_pert(b(j),tun_par(0)*pen_weights(j),epsilon);
    }
  }
  return logL;
}



void logLgradHess_pen_logit_cpp(double& logL,arma::vec& logLgrad,
                                arma::mat& logLhess,const arma::vec& b,
                                const arma::mat& X,const arma::vec& Y,
                                const arma::uword& N,const arma::vec& tun_par,
                                const arma::vec& pen_weights,
                                const std::string& pen_type,
                                const double& epsilon){
  uword ncov = X.n_cols;
  mat E = zeros(ncov,ncov);
  logLgradHess_logit_cpp(logL,logLgrad,logLhess,b,X,Y);
  
  if (pen_type=="SCAD"){
    for (uword j=0;j<ncov;++j){
      logL = logL - double(N) * 
        scad_penalty_pert(b(j),tun_par(0)*pen_weights(j),tun_par(1),epsilon);
      E(j,j) = scad_penalty_prime_pert(b(j),tun_par(0)*pen_weights(j),
        tun_par(1),epsilon);
    }
  }
  else if (pen_type=="LASSO"){
    for (uword j=0;j<ncov;++j){
      logL = logL - double(N) * 
        lasso_penalty_pert(b(j),tun_par(0)*pen_weights(j),epsilon);
      E(j,j)=lasso_penalty_prime_pert(b(j),tun_par(0)*pen_weights(j),epsilon);
    }
  }
  logLgrad=logLgrad-double(N)*E*b;
  logLhess=logLhess-double(N)*E;
}



// [[Rcpp::export]]
Rcpp::List fit_pen_logit_cpp(const arma::vec& b_SV,const arma::mat& X,
                             const arma::vec& Y,const bool& warnings,
                             const arma::uword& maxiter,const double& tol,
                             const arma::uword& N,const arma::vec& tun_par,
                             const arma::vec& pen_weights,
                             const std::string& pen_type,const double& epsilon){
  
  vec b,b_old,logLgrad(X.n_cols),dir;
  double logL,logL_old;
  mat logLhess(X.n_cols,X.n_cols);
  uword i,fail=0,halving=0;
  int rank_old=X.n_cols,rank;
  b=b_SV;
  b_old=b;

  logLgradHess_pen_logit_cpp(logL,logLgrad,logLhess,b,X,Y,N,
                             tun_par,pen_weights,pen_type,epsilon);
  logL_old = logL;
  
  for (i=1; i<=maxiter; i++){
    if (i==1){
      fail = isnan(logL) + isinf(logL);
      if (fail>0){
        Rcpp::Rcout << "WARNING :: Fit penalized logit model" << 
                       " :: bad starting values." << endl;
        break;
      }
      b_old = b;
      b = b_old + solve(-logLhess,logLgrad);
    } else {
      fail = 0;
      rank = arma::rank(-logLhess);
      fail += !logLhess.is_finite() + isnan(logL) + 
              isinf(logL) + abs(rank_old-rank);
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
    logLgradHess_pen_logit_cpp(logL,logLgrad,logLhess,b,X,Y,N,
                               tun_par,pen_weights,pen_type,epsilon);
    if (i==maxiter) 
      Rcpp::Rcout << "WARNING :: Fit penalized logit model" <<
        " :: NR algorithm exceeded maximum number of iterations." << endl;
  }

  return Rcpp::List::create(Rcpp::Named("coef") = b,
                            Rcpp::Named("logL") = logL,
                            Rcpp::Named("logLgradient") = logLgrad,
                            Rcpp::Named("logLhessian") = logLhess,
                            Rcpp::Named("iter") = i);
}



