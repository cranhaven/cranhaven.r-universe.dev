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

#ifndef cure_std_fit_hpp
#define cure_std_fit_hpp

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <iostream>

using namespace arma;
using namespace std;

arma::vec compute_survival_cpp(const arma::vec& beta,const arma::vec& base_hazard,
                               const arma::vec& tstart,const arma::vec& tstop,
                               const arma::uword& N,const arma::uword& nobs,
                               const arma::vec& fail_times,const arma::mat& Z,
                               const bool& constraint);

Rcpp::List compute_E_step_cpp(const arma::vec& b,const arma::vec& beta,
                              const arma::vec& base_hazard,const bool& constraint,
                              const arma::vec& tstart,const arma::vec& tstop,
                              const arma::uword& N,const arma::uvec& nobs_i,
                              const arma::vec& fail_times,const arma::vec& status_FIX,
                              const arma::mat& Z,const arma::mat& X_FIX);
  
double logL_obs_std_cure_cpp(const arma::vec& b,const arma::vec& beta,
                             const arma::vec& base_hazard,const arma::vec& tstart,
                             const arma::vec& tstop,const arma::uword& nobs,
                             const arma::uword& N,const arma::uvec& nobs_i,
                             const arma::vec& fail_times,const arma::vec& status_FIX,
                             const arma::mat& Z,const arma::mat& X_FIX,
                             const arma::vec& nfails,const bool& constraint);

#endif
