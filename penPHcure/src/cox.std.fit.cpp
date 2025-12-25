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
// #include <chrono>
// using namespace std::chrono;

using namespace arma;
using namespace std;
using namespace Rcpp;

double logL_cox_cpp(const arma::vec& beta,const arma::uword& nused,
                    const arma::uword& nvar,const arma::vec& start,
                    const arma::vec& tstop,const arma::uvec& event,
                    const arma::uvec& sort1,const arma::uvec& sort2,
                    const arma::mat& covar,const arma::vec& offset,
                    const arma::uword& method){
  vec keep = zeros(nused);
  vec eta = covar*beta + offset;
  double newlk = 0;
  uword person = 0;
  uword indx1 = 0;
  double denom = 0;
  int nrisk = 0;
  uword i,k,p,p1,deaths;
  double dtime,risk,denom2;
  
  // In order to avoid exponential overflow!!!
  for (i=0; i<nused; i++){ 
    if (eta[i]>37) eta[i]=37;
  }
  
  while (person < nused) {
    R_CheckUserInterrupt();
    
    for (k=person; k< nused; k++) {
      p = sort2[k];
      if (event[p] == 1) {
        dtime = tstop[p];
        break;
      }
    }
    if (k == nused) person = k;  
    else {
      for (; indx1<nused; indx1++) {
        p1 = sort1[indx1];
        if (start[p1] < dtime) break;
        if (keep[p1] == 0) continue;
        nrisk--;
        if (nrisk == 0) {
          denom = 0;
        }
        else {
          risk = exp(eta[p1]);
          denom -= risk;
        }
      }
      
      denom2 = 0;
      deaths = 0;    
      for (; person<nused; person++) {
        p = sort2[person];
        if (tstop[p] < dtime) break;
        risk = exp(eta[p]);
        if (event[p] ==1 ){
          keep[p] =1;
          nrisk++;
          deaths++;
          denom2 += risk;
          newlk += eta[p];
        }
        else if (start[p] < dtime) {
          keep[p] = 1;
          nrisk++;
          denom += risk;
        } 
      }
      
      if (method==0 || deaths ==1) {
        denom += denom2;
        newlk -= deaths*log(denom);
      }
      else {
        for (k=0; k<deaths; k++) {
          denom += denom2/deaths;
          newlk -= log(denom);
        }
      }
    }
  }
  return newlk;
}


void logLgradHess_cox_cpp(double& logL,arma::vec& logLgrad,arma::mat& logLhess,
                          const arma::vec& beta,const arma::uword& nused,
                          const arma::uword& nvar,const arma::vec& start,
                          const arma::vec& tstop,const arma::uvec& event,
                          const arma::uvec& sort1,const arma::uvec& sort2,
                          const arma::mat& covar,const arma::vec& offset,
                          const arma::uword& method){
  vec keep = zeros(nused);
  uword person = 0;
  uword indx1 = 0;
  int nrisk=0;
  double denom=0;
  double newlk = 0;
  uword i,k,p,p1,deaths;
  double dtime,risk,denom2;
  vec a = zeros(nvar);
  mat cmat = zeros(nvar,nvar);
  vec a2 = zeros(nvar);
  mat cmat2 = zeros(nvar,nvar);
  mat imat = zeros(nvar,nvar);
  vec u = zeros(nvar);
  vec eta = covar*beta + offset;
  
  // In order to avoid exponential overflow!!!
  for (i=0; i<nused; i++){ 
    if (eta[i]>37) eta[i]=37;
  }
  
  while (person < nused) {
    R_CheckUserInterrupt();
    
    for (k=person; k< nused; k++) {
      p = sort2[k];
      if (event[p] == 1) {
        dtime = tstop[p];
        break;
      }
    }
    if (k == nused) person = k;  
    else {
      for (; indx1<nused; indx1++) {
        p1 = sort1[indx1];
        if (start[p1] < dtime) break;
        if (keep[p1] == 0) continue; 
        nrisk--;
        if (nrisk == 0) {
          denom = 0;
          a = zeros(nvar);
          cmat = zeros(nvar,nvar);
        }
        else {
          risk = exp(eta[p1]);
          denom -= risk;
          a -= risk * covar.row(p1).t();
          cmat -= risk * covar.row(p1).t() * covar.row(p1);
        }
      }
      
      denom2 = 0;
      deaths = 0;    
      a2 = zeros(nvar);
      cmat2 = zeros(nvar,nvar);
      for (; person<nused; person++){
        p = sort2[person];
        if (tstop[p] < dtime) break;
        risk = exp(eta[p]);
        
        if (event[p] == 1){
          keep[p] = 1;
          nrisk++;
          deaths++;
          denom2 += risk;
          newlk += eta[p];
          u += covar.row(p).t();
          a2 += risk * covar.row(p).t();
          cmat2 += risk * covar.row(p).t() * covar.row(p);
        }
        else if (start[p] < dtime) {
          keep[p] = 1;
          nrisk++;
          denom += risk;
          a += risk * covar.row(p).t();
          cmat += risk * covar.row(p).t() * covar.row(p);
        } 
      }
      
      if (method==0 || deaths ==1) { 
        denom += denom2;
        newlk -= deaths*log(denom);
        a += a2;
        u -= deaths * a / denom;
        cmat += cmat2;
        imat += deaths*((cmat - a*a.t()/denom)/denom);
      }
      else {
        for (k=0; k<deaths; k++) {
          denom += denom2/deaths;
          newlk -= log(denom);
          a += a2/deaths;
          u -= a / denom;
          cmat += cmat2 / deaths;
          imat += (cmat - a*a.t()/denom)/denom; 
        }
      }
    }
  }
  logL=newlk; logLgrad=u; logLhess=imat;
}




// [[Rcpp::export]]
Rcpp::List fit_cox_cpp(const arma::vec& beta_SV,const arma::uword& nobs,
                       const arma::vec& tstart,const arma::vec& tstop,
                       const arma::uvec& sort1,const arma::uvec& sort2,
                       const arma::uvec& status,const arma::vec& offset,
                       const arma::mat& Z,const arma::uword& method,
                       const bool& warnings,const arma::uword& maxiter,
                       const double& tol){
  uword nvar = Z.n_cols;
  vec beta,beta_old,dir;
  vec logLgrad(nvar);
  double logL,logL_old;
  mat logLhess(nvar,nvar);
  uword i,fail=0,halving=0;
  int rank_old=Z.n_cols,rank;
  beta=beta_SV;
  
  logLgradHess_cox_cpp(logL,logLgrad,logLhess,beta,nobs,nvar,
                       tstart,tstop,status,sort1,sort2,Z,offset,method);
  
  logL_old = logL;
  
  for (i=1; i<=maxiter; i++){
    if (i==1){
      fail = isnan(logL) + isinf(logL);
      if (fail>0){
        Rcpp::Rcout << "WARNING :: Fit standard cox model" <<
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
    logLgradHess_cox_cpp(logL,logLgrad,logLhess,beta,nobs,nvar,
                         tstart,tstop,status,sort1,sort2,Z,offset,method);
  }
  
  if (i==maxiter ) 
    Rcpp::Rcout << "WARNING :: Fit standard cox model" << 
            " :: NR algorithm exceeded maximum number of iterations." << endl;
  
  return Rcpp::List::create(Rcpp::Named("coef") = beta,
                            Rcpp::Named("logL") = logL,
                            Rcpp::Named("logLgradient") = logLgrad,
                            Rcpp::Named("logLhessian") = logLhess,
                            Rcpp::Named("iter") = i);
}
