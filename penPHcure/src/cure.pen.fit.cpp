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

#include "logit.pen.fit.h"
#include "cox.pen.fit.h"
#include "cox.base_hazard.h"
#include "cure.std.fit.h"

using namespace arma;
using namespace std;
using namespace Rcpp;


// [[Rcpp::export]]
List fit_pen_cure_cpp(const arma::vec& b_SV,const arma::vec& beta_SV,
                      const arma::vec& base_hazard_SV,const arma::vec& tstart,
                      const arma::vec& tstop,const arma::uvec& sort1,
                      const arma::uvec& sort2,const arma::uvec& status,
                      const arma::vec& fail_times,const arma::vec& nfails,
                      const arma::uvec& nobs_i,const arma::vec& status_FIX,
                      const arma::mat& X_FIX,const arma::mat& Z,
                      const arma::uword& method,const bool& warnings,
                      const arma::uword& maxiterNR,const arma::uword& maxiterEM,
                      const bool& constraint,const double& tol,
                      const std::string& pen_type,const arma::vec& tun_parCURE,
                      const arma::vec& tun_parSURV,const arma::vec& pen_weightsCURE,
                      const arma::vec& pen_weightsSURV,const double& epsilon,
                      const double& thres_zero){
  uword nobs=status.n_elem;
  uword N=status_FIX.n_elem;
  vec b=b_SV;
  vec beta=beta_SV;
  vec base_hazard=base_hazard_SV;
  vec Y,Y_FIX,b_old,beta_old,base_hazard_old;
  Rcpp::List E_step,COX,LR;
  int converged=0;
  uword K=fail_times.n_elem;
  double logL,AIC,BIC;
  
  uword m=0;
  do {
    
    R_CheckUserInterrupt();
    
    beta_old=beta;
    b_old=b;
    base_hazard_old=base_hazard;
    
    // EXPECTATION STEP
    E_step = compute_E_step_cpp(b_old,beta_old,base_hazard_old,constraint,
                                tstart,tstop,N,nobs_i,fail_times,
                                status_FIX,Z,X_FIX);
    Y = as<vec>(E_step["Y"]);
    Y_FIX = as<vec>(E_step["Y_FIX"]);
    // MAXIMIZATION STEP
    COX = fit_pen_cox_cpp(beta_old,nobs,tstart,tstop,sort1,sort2,status,
                          fail_times,nfails,log(Y),Z,method,warnings,maxiterNR,
                          tol,N,tun_parSURV,pen_weightsSURV,pen_type,epsilon);
    beta = as<vec>(COX["coef"]);
    base_hazard = cox_base_hazard_cpp(beta,nobs,K,tstart,tstop,
                                      status,sort1,sort2,Z,log(Y),method);
    LR = fit_pen_logit_cpp(b_old,X_FIX,Y_FIX,warnings,maxiterNR,tol,
                           N,tun_parCURE,pen_weightsCURE,pen_type,epsilon);
    b = as<vec>(LR["coef"]);
    
    m++;
    
    // Check convergence
    if (m==maxiterEM){
      converged = -1;
      if (warnings){
        Rcpp::Rcout << "WARNING :: Penalized Cure model" <<
          " :: EM algorithm exceeded maximum number of iterations." << endl;
      }
      break;
    } else if (b.has_nan() || beta.has_nan() ) {
      converged=-2;
      if(warnings){
        Rcpp::Rcout << "WARNING :: Penalized Cure model" <<
          " :: nan somewhere in the coefficients vector 'b' or 'beta'." << endl;
      }
      b = b_old;
      beta = beta_old;
      base_hazard = base_hazard_old;
      break;
    }
  } while( sum(pow(b-b_old,2)) > tol || sum(pow(beta-beta_old,2)) > tol );
  
  // Check zeros
  double df = Z.n_cols + X_FIX.n_cols;
  for (uword j=0;j<beta.n_elem;j++)  
    if (abs(beta[j])<thres_zero) {beta[j]=0; df-=1;}
  for (uword j=1;j<b.n_elem;j++)
    if (abs(b[j])<thres_zero) {b[j]=0; df-=1;}  
    
  // Compute AIC and BIC criterion
  logL = logL_obs_std_cure_cpp(b,beta,base_hazard,tstart,tstop,nobs,N,nobs_i,
                               fail_times,status_FIX,Z,X_FIX,nfails,constraint);
  AIC = -2.0*logL + 2 * df;
  BIC = -2.0*logL + log(N) * df;
  
  // Compute probabilities
  vec surv = compute_survival_cpp(beta,base_hazard,tstart,tstop,
                                  N,nobs,fail_times,Z,constraint);
  vec prob = 1/(1+exp(-X_FIX*b));
  
  return Rcpp::List::create(Rcpp::Named("b") = b,
                            Rcpp::Named("beta") = beta,
                            Rcpp::Named("cumhaz") = cumsum(base_hazard),
                            Rcpp::Named("AIC") = AIC,
                            Rcpp::Named("BIC") = BIC,
                            Rcpp::Named("df") = df,
                            Rcpp::Named("prob") = prob,
                            Rcpp::Named("surv") = surv,
                            Rcpp::Named("converged") = converged,
                            Rcpp::Named("iter") = m,
                            Rcpp::Named("N") = N,
                            Rcpp::Named("K") = K,
                            Rcpp::Named("isTies") = any(nfails>1),
                            Rcpp::Named("censoring") = 1-mean(status_FIX));
  
}


