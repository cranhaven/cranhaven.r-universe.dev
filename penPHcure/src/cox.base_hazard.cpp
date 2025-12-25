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


// [[Rcpp::export]]
arma::vec cox_base_hazard_cpp(const arma::vec& beta,const arma::uword& nused,
                              const arma::uword& K,const arma::vec& start,
                              const arma::vec& tstop,const arma::uvec& event,
                              const arma::uvec& sort1,const arma::uvec& sort2,
                              const arma::mat& covar, const arma::vec& offset,
                              const arma::uword& method){
  uword person = 0;
  uword indx1 = 0;
  int nrisk=0;
  double denom=0;
  uword i,j,k,p,p1,deaths;
  double dtime,risk,denom2;
  vec eta = covar*beta + offset;
  vec base_hazard = zeros(K);
  vec keep = zeros(nused);
  
  // In order to avoid exponential overflow!!!
  for (i=0; i<nused; i++){ 
    if (eta[i]>37) eta[i]=37;
  }
  
  j=K-1;
  
  
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
        if (event[p]==1){
          keep[p] =1;
          nrisk++;
          deaths++;
          denom2 += risk;
        }
        else if (start[p] < dtime) {
          keep[p] = 1;
          nrisk++;
          denom += risk;
        } 
      }
      if (method==0 || deaths ==1) { /*Breslow*/
        denom += denom2;
        base_hazard[j] = deaths/denom;
      }
      else { /*Efron*/
        for (k=0; k<deaths; k++) {
          denom += denom2/deaths;
          base_hazard[j] += 1/denom;
        }
      }
    }
    j--;
  }
  return base_hazard;
}
