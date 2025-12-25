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

#include "cox.std.fit.h"
#include "penalties.h"

using namespace arma;
using namespace std;
using namespace Rcpp;


double logL_pen_cox_cpp(const arma::vec& beta,const arma::uword& nused,
                        const arma::uword& nvar,const arma::vec& start,
                        const arma::vec& tstop,const arma::uvec& event,
                        const arma::uvec& sort1,const arma::uvec& sort2,
                        const arma::mat& covar,const arma::vec& offset,
                        const arma::uword& method,const arma::uword& N,
                        const arma::vec& tun_par,const arma::vec& pen_weights,
                        const std::string& pen_type,const double& epsilon){
  
  uword ncov = covar.n_cols;
  double logL = logL_cox_cpp(beta,nused,nvar,start,tstop,
                             event,sort1,sort2,covar,offset,method);
  
  if (pen_type=="SCAD"){
    for (uword j=0;j<ncov;++j){
      logL = logL - double(N) * 
        scad_penalty_pert(beta(j),tun_par(0)*pen_weights(j),tun_par(1),epsilon);
    }
  }
  else if (pen_type=="LASSO"){
    for (uword j=0;j<ncov;++j){
      logL = logL - double(N) * 
        lasso_penalty_pert(beta(j),tun_par(0)*pen_weights(j),epsilon);
    }
  }
  
  return logL;
}


void logLgradHess_pen_cox_cpp(double& logL,arma::vec& logLgrad,
                              arma::mat& logLhess,const arma::vec& beta,
                              const arma::uword& nused,const arma::uword& nvar, 
                              const arma::vec& start,const arma::vec& tstop,
                              const arma::uvec& event,const arma::uvec& sort1,
                              const arma::uvec& sort2,const arma::mat& covar,
                              const arma::vec& offset,const arma::uword& method,
                              const arma::uword& N,const arma::vec& tun_par,
                              const arma::vec& pen_weights,
                              const std::string& pen_type,const double& epsilon){
  uword ncov = covar.n_cols;
  mat E = zeros(ncov,ncov);
  
  logLgradHess_cox_cpp(logL,logLgrad,logLhess,beta,nused,nvar,
                       start,tstop,event,sort1,sort2,covar,offset,method);
  
  if (pen_type=="SCAD"){
    for (uword j=0;j<ncov;++j){
      logL = logL - double(N) * 
        scad_penalty_pert(beta(j),tun_par(0)*pen_weights(j),tun_par(1),epsilon);
      E(j,j) = scad_penalty_prime_pert(beta(j),tun_par(0)*pen_weights(j),
                                       tun_par(1),epsilon);
    }
  }
  else if (pen_type=="LASSO"){
    for (uword j=0;j<ncov;++j){
      logL = logL - double(N) * 
        lasso_penalty_pert(beta(j),tun_par(0)*pen_weights(j),epsilon);
      E(j,j)=lasso_penalty_prime_pert(beta(j),tun_par(0)*pen_weights(j),epsilon);
    }
  }

  logLgrad=logLgrad-(double(N)*E*beta);
  logLhess=logLhess + double(N)*E;
  
}



// [[Rcpp::export]]
Rcpp::List fit_pen_cox_cpp(const arma::vec& beta_SV,const arma::uword& nobs,
                           const arma::vec& tstart,const arma::vec& tstop,
                           const arma::uvec& sort1,const arma::uvec& sort2,
                           const arma::uvec& status,const arma::vec& fail_times,
                           const arma::vec& nfails,const arma::vec& offset,
                           const arma::mat& Z,const arma::uword& method,
                           const bool& warnings,const arma::uword& maxiter,
                           const double& tol,const arma::uword& N,
                           const arma::vec& tun_par,const arma::vec& pen_weights,
                           const std::string& pen_type,const double& epsilon){
  uword nvar = Z.n_cols;
  vec beta,beta_old,dir;
  vec logLgrad(nvar);
  double logL,logL_old;
  mat logLhess(nvar,nvar);
  uword i,fail=0,halving=0;
  int rank_old=Z.n_cols,rank;
  beta=beta_SV;
  beta_old = beta;
  
  logLgradHess_pen_cox_cpp(logL,logLgrad,logLhess,beta,nobs,nvar,tstart,tstop,
                           status,sort1,sort2,Z,offset,method,N,tun_par,
                           pen_weights,pen_type,epsilon);
  
  logL_old = logL;
  
  for (i=1; i<=maxiter; i++){
    if (i==1){
      fail = isnan(logL) + isinf(logL);
      if (fail>0){
        Rcpp::Rcout << "WARNING :: Fit penalized cox model" << 
                       " :: bad starting values." << endl;
        break;
      } 
      beta_old = beta;
      beta = beta_old + solve(logLhess,logLgrad);
    } else {
      fail = 0;
      rank = arma::rank(logLhess);
      fail += !logLhess.is_finite() + isnan(logL) +
              isinf(logL) + abs(rank_old-rank);
      if (fail>0 || logL<logL_old) {
        halving++;
        beta = (beta_old*halving + beta)/(halving+1.0);
      } else {
        halving = 0;
        if (fail==0 && halving==0 && abs(1-(logL_old/logL))<=tol) break;
        logL_old = logL;
        beta_old = beta;
        beta = beta_old + solve(logLhess,logLgrad);
      }
    }
    logLgradHess_pen_cox_cpp(logL,logLgrad,logLhess,beta,nobs,nvar,tstart,tstop,
                             status,sort1,sort2,Z,offset,method,N,tun_par,
                             pen_weights,pen_type,epsilon);
  }
  
  if (i==maxiter ) 
    Rcpp::Rcout << "WARNING :: Fit penalized cox model" <<
           " :: NR algorithm exceeded maximum number of iterations." << endl;
  
  return Rcpp::List::create(Rcpp::Named("coef") = beta,
                            Rcpp::Named("logL") = logL,
                            Rcpp::Named("logLgradient") = logLgrad,
                            Rcpp::Named("logLhessian") = logLhess,
                            Rcpp::Named("num_iter") = i);
}
