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

#ifndef cox_std_fit_hpp
#define cox_std_fit_hpp

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <iostream>

using namespace std;

double logL_cox_cpp(const arma::vec& beta,const arma::uword& nused,
                    const arma::uword& nvar,const arma::vec& start,
                    const arma::vec& tstop,const arma::uvec& event,
                    const arma::uvec& sort1,const arma::uvec& sort2,
                    const arma::mat& covar,const arma::vec& offset,
                    const arma::uword& method);

void logLgradHess_cox_cpp(double& logL,arma::vec& logLgrad,arma::mat& logLhess,
                          const arma::vec& beta,const arma::uword& nused,
                          const arma::uword& nvar,const arma::vec& start,
                          const arma::vec& tstop,const arma::uvec& event,
                          const arma::uvec& sort1,const arma::uvec& sort2,
                          const arma::mat& covar,const arma::vec& offset,
                          const arma::uword& method);
  
Rcpp::List fit_cox_cpp(const arma::vec& beta_SV,const arma::uword& nobs,
                       const arma::vec& tstart,const arma::vec& tstop,
                       const arma::uvec& sort1,const arma::uvec& sort2,
                       const arma::uvec& status,const arma::vec& offset,
                       const arma::mat& Z,const arma::uword& method,
                       const bool& warnings,const arma::uword& maxiter,
                       const double& tol);

#endif
