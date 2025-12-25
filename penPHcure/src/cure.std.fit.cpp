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

#include "cox.base_hazard.h"
#include "cox.std.fit.h"
#include "logit.std.fit.h"

using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec compute_survival_cpp(const arma::vec& beta,const arma::vec& base_hazard,
                               const arma::vec& tstart,const arma::vec& tstop,
                               const arma::uword& N,const arma::uword& nobs,
                               const arma::vec& fail_times,const arma::mat& Z,
                               const bool& constraint){
  vec expZbeta = exp(Z*beta);
  vec cum_hazard = zeros(N);
  vec surv;
  uword i = 0;
  uword j = 0;
  uword K = fail_times.n_elem;
  uword id = 0;
  double last_fail_time = fail_times[K-1];
  while (i < nobs){
    if (i>0){
      if (tstart[i]<tstop[i-1]){
        id++;
        j = 0;
      }
    }
    while (j<K){
      if (fail_times[j]>tstop[i]) break;
      cum_hazard[id] += expZbeta[i] * base_hazard[j];
      // Zero-tail constraint !!!
      if (constraint & (tstop[i] > last_fail_time)) cum_hazard[id] = datum::inf;
      j++;
    }
    i++;
  }
  
  surv = exp(-cum_hazard);
  
  return surv;
}




Rcpp::List compute_E_step_cpp(const arma::vec& b,const arma::vec& beta,
                              const arma::vec& base_hazard,const bool& constraint,
                              const arma::vec& tstart,const arma::vec& tstop,
                              const arma::uword& N,const arma::uvec& nobs_i,
                              const arma::vec& fail_times,const arma::vec& status_FIX,
                              const arma::mat& Z,const arma::mat& X_FIX){
  uword nobs = sum(nobs_i); 
  vec Y(nobs);
  vec Y_FIX(N);
  vec Xb_FIX = X_FIX*b;
  vec prob = 1 / (1+exp(-Xb_FIX));
  vec surv = compute_survival_cpp(beta,base_hazard,tstart,tstop,
                                  N,nobs,fail_times,Z,constraint);
  uword i = 0;
  uword last=0;
  for (uword id=0;id<N;id++){
    if (status_FIX[id]==1) Y_FIX[id]=1;
    else Y_FIX[id] = (prob[id]*surv[id])/(prob[id]*surv[id]+1-prob[id]);
    last += nobs_i[id];
    while (i<last){
      Y[i] = Y_FIX[id];
      i++;
    }
  }
  return Rcpp::List::create(Rcpp::Named("Y") = Y,
                            Rcpp::Named("Y_FIX") = Y_FIX);
}



double logL_obs_std_cure_cpp(const arma::vec& b,const arma::vec& beta,
                             const arma::vec& base_hazard,const arma::vec& tstart,
                             const arma::vec& tstop,const arma::uword& nobs,
                             const arma::uword& N,const arma::uvec& nobs_i,
                             const arma::vec& fail_times,const arma::vec& status_FIX,
                             const arma::mat& Z,const arma::mat& X_FIX,
                             const arma::vec& nfails,const bool& constraint){

  uword j,id;
  uword K = fail_times.n_elem;
  uword last=0;
  vec Xb_FIX = X_FIX*b;
  vec Zbeta = Z*beta;
  vec cum_hazard = - log(compute_survival_cpp(beta,base_hazard,tstart,tstop,
                                              N,nobs,fail_times,Z,constraint));
  double logL = 0;
  
  for (j=0;j<K;j++){
    if (j==0) logL += nfails[j]*log(base_hazard[j]/fail_times[j]);
    else logL += nfails[j]*log(base_hazard[j]/(fail_times[j]-fail_times[j-1]));
  }
  
  for (id=0;id<N;id++){
    last += nobs_i[id];
    if (status_FIX[id]==1){
      logL += Xb_FIX[id] - 
        log(1+exp(Xb_FIX[id])) + Zbeta[last-1] - cum_hazard[id];
    } 
    else {
      logL += log(1+exp(Xb_FIX[id]-cum_hazard[id])) - log(1+exp(Xb_FIX[id]));
    }
  }
  return logL;
}




// [[Rcpp::export]]
List fit_cure_cpp(arma::vec b_SV,arma::vec beta_SV,arma::vec base_hazard_SV,
                  arma::vec tstart,arma::vec tstop,arma::uvec sort1,
                  arma::uvec sort2,arma::uvec status,arma::vec fail_times,
                  arma::vec nfails,arma::uvec nobs_i,arma::vec status_FIX,
                  arma::mat X_FIX,arma::mat Z,arma::uword method,
                  bool warnings,arma::uword maxiterNR,arma::uword maxiterEM,
                  double tol,bool constraint){
  
  uword nobs=status.n_elem;
  uword N=status_FIX.n_elem;
  vec b=b_SV;
  vec beta=beta_SV;
  vec base_hazard=base_hazard_SV;
  vec Y,Y_FIX,b_old,beta_old,base_hazard_old;
  Rcpp::List E_step,COX,LR;
  int converged=1;
  uword K=fail_times.n_elem;

  uword m=0;
  do {

    R_CheckUserInterrupt();
    
    beta_old=beta;
    b_old=b;
    base_hazard_old=base_hazard;
    
    // EXPECTATION STEP
    E_step = compute_E_step_cpp(b_old,beta_old,base_hazard_old,constraint,tstart,
                                tstop,N,nobs_i,fail_times,status_FIX,Z,X_FIX);
    Y = as<vec>(E_step["Y"]);
    Y_FIX = as<vec>(E_step["Y_FIX"]);

    // MAXIMIZATION STEP
    COX = fit_cox_cpp(beta_old,nobs,tstart,tstop,sort1,sort2,
                      status,log(Y),Z,method,warnings,maxiterNR,tol);
    beta = as<vec>(COX["coef"]);
    base_hazard = cox_base_hazard_cpp(beta,nobs,K,tstart,tstop,
                                      status,sort1,sort2,Z,log(Y),method);
    LR = fit_logit_cpp(b_old,X_FIX,Y_FIX,warnings,maxiterNR,tol);
    b = as<vec>(LR["coef"]);
    
    m++;
    
    // Check convergence
    if (m==maxiterEM){
      converged = -1;
      if (warnings){
        Rcpp::Rcout << "WARNING :: Standard Cure model" <<
          " :: EM algorithm exceeded maximum number of iterations." << endl;
      }
      break;
    } else if (b.has_nan() || beta.has_nan() ) {
      converged=-2;
      if(warnings){
        Rcpp::Rcout << "WARNING :: Standard Cure model" <<
          " :: nan somewhere in the coefficients vector 'b' or 'beta'." << endl;
      }
      b = b_old;
      beta = beta_old;
      base_hazard = base_hazard_old;
      break;
    }
    
  } while( sum(pow(b-b_old,2)) > tol || sum(pow(beta-beta_old,2)) > tol );
  
  // Compute log-likelihood
  double logL = logL_obs_std_cure_cpp(b,beta,base_hazard,tstart,tstop,
                                      nobs,N,nobs_i,fail_times,status_FIX,
                                      Z,X_FIX,nfails,constraint);
  
  return Rcpp::List::create(Rcpp::Named("b") = b,
                            Rcpp::Named("beta") = beta,
                            Rcpp::Named("cumhaz") = cumsum(base_hazard),
                            Rcpp::Named("logL") = logL,
                            Rcpp::Named("converged") = converged,
                            Rcpp::Named("iter") = m,
                            Rcpp::Named("N") = N,
                            Rcpp::Named("K") = K,
                            Rcpp::Named("isTies") = any(nfails>1),
                            Rcpp::Named("censoring") = 1-mean(status_FIX));
}
